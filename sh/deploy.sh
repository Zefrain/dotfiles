#!/bin/bash
set -euo pipefail

# ==============================================
#  >>>  可通过环境变量覆盖以下变量  <<<
# ==============================================
ZT_NETWORK_ID="${ZT_NETWORK_ID:-565799d8f6d8da8f}"
: "${ZT_API_TOKEN:?Set ZT_API_TOKEN before running this script}"
ZT_VER="${ZT_VER:-1.14.2}"

COS_SECRET_ID="${COS_SECRET_ID:?Set COS_SECRET_ID before running this script}"
COS_SECRET_KEY="${COS_SECRET_KEY:?Set COS_SECRET_KEY before running this script}"
COS_BUCKET="${COS_BUCKET:-as-lightgbm-1450451715}"
COS_REGION="${COS_REGION:-ap-nanjing}"
COS_ENDPOINT="cos-internal.${COS_REGION}.tencentcos.cn"

PROXY_HOST="${PROXY_HOST:-192.168.168.168}"
PROXY_PORT="${PROXY_PORT:-7893}"
PROXY_URL="http://${PROXY_HOST}:${PROXY_PORT}"

DATA_DIR="${DATA_DIR:-/data}"
CHECKPOINT_DIR="${CHECKPOINT_DIR:-checkpoints}"
# ==============================================

LOGFILE=/root/deploy-v12.log
exec > >(tee -a "$LOGFILE") 2>&1
echo "==== DEPLOY V12 $(date) ===="

apt update -qq && apt upgrade -y -qq
apt install -y -qq curl wget python3-pip git gnupg lsb-release

# --- ZeroTier 静态二进制 ---
echo "--- ZeroTier ---"
if ! command -v zerotier-one &>/dev/null; then
    cd /tmp
    https_proxy=${PROXY_URL} wget --connect-timeout 10 --timeout 60 \
      "https://github.com/zerotier/ZeroTierOne/releases/download/${ZT_VER}/ZeroTierOne-${ZT_VER}-linux-amd64.tar.gz" \
      -O zt.tar.gz
    tar -xzf zt.tar.gz
    cp zerotier-one /usr/sbin/ && chmod +x /usr/sbin/zerotier-one
    ln -sf /usr/sbin/zerotier-one /usr/sbin/zerotier-cli
    ln -sf /usr/sbin/zerotier-one /usr/sbin/zerotier-idtool
    cd /
fi

cat > /etc/systemd/system/zerotier-one.service << 'ZTU'
[Unit]
Description=ZeroTier One
After=network.target
[Service]
Type=simple
ExecStart=/usr/sbin/zerotier-one
Restart=always
RestartSec=2
User=root
[Install]
WantedBy=multi-user.target
ZTU
systemctl daemon-reload
systemctl enable zerotier-one
systemctl start zerotier-one
sleep 2
zerotier-cli join "$ZT_NETWORK_ID"
sleep 3
ZT_MEMBER_ID=$(zerotier-cli info | cut -d' ' -f3)
curl -s -X POST "https://api.zerotier.com/api/v1/network/${ZT_NETWORK_ID}/member/${ZT_MEMBER_ID}" \
  -H "Authorization: token ${ZT_API_TOKEN}" \
  -H "Content-Type: application/json" \
  -d '{"config":{"authorized":true}}' || echo "Approve manually at my.zerotier.com"
echo "ZeroTier OK: $(zerotier-cli status)"

# --- Node.js 20 + Codex ---
echo "--- Node + Codex ---"
curl -fsSL https://npmmirror.com/mirrors/node/v20.18.3/node-v20.18.3-linux-x64.tar.xz -o /tmp/node.tar.xz
tar -xJf /tmp/node.tar.xz -C /usr/local --strip-components=1
node -v && npm -v
npm config set registry https://registry.npmmirror.com
npm install -g @openai/codex
echo "Codex: $(codex --version)"

cat > /usr/local/bin/codex-proxy << WRAP
#!/bin/bash
export https_proxy="${PROXY_URL}"
export http_proxy="${PROXY_URL}"
exec codex "\$@"
WRAP
chmod +x /usr/local/bin/codex-proxy

# --- COS 工具：仅 coscmd（无 coscli）---
echo "--- COS ---"
pip3 config set global.index-url https://mirrors.cloud.tencent.com/pypi/simple/
pip3 config set global.trusted-host mirrors.cloud.tencent.com
pip3 config set global.break-system-packages true
pip3 install coscmd

cat > ~/.cos.conf << COSC
[common]
secret_id = ${COS_SECRET_ID}
secret_key = ${COS_SECRET_KEY}
bucket = ${COS_BUCKET}
endpoint = ${COS_ENDPOINT}
max_thread = 5
part_size = 1
retry = 5
timeout = 60
schema = https
verify = md5
anonymous = False
COSC
mkdir -p /home/ubuntu/.cos && cp ~/.cos.conf /home/ubuntu/.cos.conf && chown ubuntu:ubuntu /home/ubuntu/.cos.conf

echo "coscmd OK"

# --- 镜像启动配置 ---
cat > /etc/image-template.env << INF
ZT_NETWORK_ID="${ZT_NETWORK_ID}"
ZT_API_TOKEN="${ZT_API_TOKEN}"
COS_BUCKET="${COS_BUCKET}"
COS_REGION="${COS_REGION}"
COS_ENDPOINT="${COS_ENDPOINT}"
PROXY_HOST="${PROXY_HOST}"
PROXY_PORT="${PROXY_PORT}"
INF
chmod 600 /etc/image-template.env

# --- /opt 脚本（全部内联生成）---
echo "--- /opt scripts ---"

# cos_backup.sh -- upload/update only; never delete COS objects
cat > /opt/cos_backup.sh << BAK
#!/bin/bash
LOG=/var/log/cos_backup.log
COS_DST="${CHECKPOINT_DIR}/"
DATA_DIR="${DATA_DIR}"
echo "=== COS BACKUP START \$(date) ===" >> "\$LOG"
for i in 1 2 3; do
    timeout 360 coscmd upload -r -s "\$DATA_DIR/" "\$COS_DST" >> "\$LOG" 2>&1 && break
    echo "Attempt \$i failed, retrying..." >> "\$LOG"
    sleep 2
done
echo "=== COS BACKUP END \$(date) ===" >> "\$LOG"
BAK
chmod +x /opt/cos_backup.sh

# spot_watch.sh
cat > /opt/spot_watch.sh << 'SPT'
#!/bin/bash
META="http://metadata.tencentyun.com/latest/meta-data/spot/instance-action"
LOG="/var/log/spot_watch.log"
echo "[spot-watch] start $(date) pid=$$" >> "$LOG"
while true; do
    CODE=$(curl -s -o /dev/null -w "%{http_code}" --max-time 3 "$META" 2>/dev/null)
    RESP=$(curl -sf --max-time 3 "$META" 2>/dev/null)
    case "$CODE" in
        200)
            if echo "$RESP" | grep -q '"action":"terminate"'; then
                echo "[spot-watch] $(date): TERMINATE detected, backup..." >> "$LOG"
                /opt/cos_backup.sh
                break
            fi
            ;;
        404) ;;
        *)
            echo "[spot-watch] $(date): unexpected code=$CODE, backup conservatively" >> "$LOG"
            /opt/cos_backup.sh
            break
            ;;
    esac
    sleep 5
done
echo "[spot-watch] exit $(date)" >> "$LOG"
SPT
chmod +x /opt/spot_watch.sh

# health_check.sh
cat > /opt/health_check.sh << 'HLTH'
#!/bin/bash
e=0
zerotier-cli status | grep -q ONLINE || { echo "FAIL: ZeroTier offline"; e=$((e+1)); }
coscmd list >/dev/null 2>&1 || { echo "FAIL: coscmd unreachable"; e=$((e+1)); }
codex --version >/dev/null 2>&1 || { echo "FAIL: Codex not found"; e=$((e+1)); }
[ $e -eq 0 ] && echo "HEALTH PASS" || echo "HEALTH FAIL $e"
HLTH
chmod +x /opt/health_check.sh

# init_data.sh
cat > /opt/init_data.sh << INIT
#!/bin/bash
set -euo pipefail
COS_SRC="${CHECKPOINT_DIR}/"
LOCAL_DST="${DATA_DIR}"
echo "=== Pulling COS:\${COS_SRC} -> \${LOCAL_DST} ==="
mkdir -p "\$LOCAL_DST"
if ! coscmd download -r -s "\$COS_SRC" "\$LOCAL_DST/"; then
    echo "Pull failed, will retry next boot"
    exit 1
fi
chown -R ubuntu:ubuntu "\$LOCAL_DST"
chmod -R u+rwX,go-rwx "\$LOCAL_DST"
echo "=== Done ==="
ls -lh "\$LOCAL_DST/"
INIT
chmod +x /opt/init_data.sh

# zt-auto.sh -- new instances auto-join after image cleanup resets identity
cat > /opt/zt-auto.sh << 'ZTA'
#!/bin/bash
set -euo pipefail

CONF="/etc/image-template.env"
[ -r "$CONF" ] && . "$CONF"

NET="${ZT_NETWORK_ID:-}"
TOKEN="${ZT_API_TOKEN:-}"
if [ -z "$NET" ] || [ -z "$TOKEN" ]; then
    echo "ZeroTier auto-join config missing: $CONF" >&2
    exit 1
fi

systemctl is-active --quiet zerotier-one || systemctl start zerotier-one

for _ in 1 2 3 4 5; do
    zerotier-cli info >/dev/null 2>&1 && break
    sleep 2
done

zerotier-cli listnetworks 2>/dev/null | grep -q "$NET" && exit 0
zerotier-cli join "$NET"
sleep 3
MEMBER=$(zerotier-cli info | cut -d' ' -f3)
curl -s -X POST "https://api.zerotier.com/api/v1/network/$NET/member/$MEMBER" \
  -H "Authorization: token $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"config":{"authorized":true}}'
ZTA
chmod +x /opt/zt-auto.sh

# prepare_custom_image.sh -- final cleanup before creating the image
cat > /opt/prepare_custom_image.sh << PCI
#!/bin/bash
set -euo pipefail

LOG="/var/log/prepare_custom_image.log"
DATA_DIR="${DATA_DIR}"
exec > >(tee -a "\$LOG") 2>&1

clean_codex_runtime() {
    local dir="\$1"
    [ -d "\$dir" ] || return 0
    rm -rf "\$dir/sessions" "\$dir/.tmp" "\$dir/shell_snapshots"
    rm -f "\$dir/history.jsonl"
    rm -f "\$dir"/logs_*.sqlite "\$dir"/logs_*.sqlite-shm "\$dir"/logs_*.sqlite-wal
    rm -f "\$dir"/goals_*.sqlite "\$dir"/goals_*.sqlite-shm "\$dir"/goals_*.sqlite-wal
    rm -f "\$dir"/state_*.sqlite "\$dir"/state_*.sqlite-shm "\$dir"/state_*.sqlite-wal
}

echo "=== PREPARE CUSTOM IMAGE START \$(date) ==="

echo "--- final COS backup before local data cleanup ---"
if [ -d "\$DATA_DIR" ] && [ -n "\$(find "\$DATA_DIR" -mindepth 1 -maxdepth 1 -print -quit 2>/dev/null)" ]; then
    /opt/cos_backup.sh
else
    echo "\$DATA_DIR is already empty or missing; skip final backup"
fi

echo "--- clean local \$DATA_DIR; remote COS remains authoritative ---"
mkdir -p "\$DATA_DIR"
find "\$DATA_DIR" -mindepth 1 -maxdepth 1 -exec rm -rf -- {} +
chown ubuntu:ubuntu "\$DATA_DIR"
chmod 755 "\$DATA_DIR"

echo "--- reset ZeroTier identity for cloned instances ---"
systemctl stop zerotier-one.service 2>/dev/null || true
rm -f /var/lib/zerotier-one/identity.secret \
      /var/lib/zerotier-one/identity.public \
      /var/lib/zerotier-one/authtoken.secret \
      /var/lib/zerotier-one/metricstoken.secret \
      /var/lib/zerotier-one/zerotier-one.pid \
      /var/lib/zerotier-one/zerotier-one.port
rm -f /var/lib/zerotier-one/networks.d/* \
      /var/lib/zerotier-one/peers.d/*
chown -R zerotier-one:zerotier-one /var/lib/zerotier-one 2>/dev/null || true

echo "--- reset cloud-init, machine-id, and SSH host keys ---"
cloud-init clean --logs --seed || true
truncate -s 0 /etc/machine-id
ln -sf /etc/machine-id /var/lib/dbus/machine-id
rm -f /etc/ssh/ssh_host_*

echo "--- clean package, npm, pip, temp, history, and session caches ---"
apt-get clean
rm -rf /var/lib/apt/lists/*
rm -rf /tmp/* /tmp/.[!.]* /tmp/..?* /var/tmp/* /var/tmp/.[!.]* /var/tmp/..?* 2>/dev/null || true
rm -rf /root/.cache /root/.npm /root/.local/share/pip
rm -rf /home/ubuntu/.cache /home/ubuntu/.npm /home/ubuntu/.local/share/pip
echo "--- clean Codex runtime state while retaining login auth ---"
clean_codex_runtime /root/.codex
clean_codex_runtime /home/ubuntu/.codex
chown -R ubuntu:ubuntu /home/ubuntu/.codex 2>/dev/null || true
echo "Codex login state is intentionally retained."
rm -f /root/.bash_history /root/.python_history /root/.mysql_history /root/.wget-hsts /root/.viminfo
rm -f /home/ubuntu/.bash_history /home/ubuntu/.python_history /home/ubuntu/.mysql_history /home/ubuntu/.wget-hsts /home/ubuntu/.viminfo
rm -f /root/deploy*.log /home/ubuntu/fix.sh /var/log/cos_backup.log /var/log/spot_watch.log /var/log/prepare_custom_image.log

echo "--- truncate logs and journal ---"
journalctl --rotate || true
journalctl --vacuum-time=1s || true
find /var/log -type f -exec truncate -s 0 {} + 2>/dev/null || true

echo "--- harden retained configuration permissions ---"
chmod 600 /etc/image-template.env 2>/dev/null || true
chmod 600 /root/.cos.conf 2>/dev/null || true
chmod 600 /home/ubuntu/.cos.conf 2>/dev/null || true
chown ubuntu:ubuntu /home/ubuntu/.cos.conf 2>/dev/null || true
chmod 700 /opt/deploy.sh 2>/dev/null || true

echo "--- filesystem trim ---"
fstrim -av 2>/dev/null || true

echo "=== PREPARE CUSTOM IMAGE END \$(date) ==="
echo "Shutdown now and create the custom image from the stopped instance."
PCI
chmod +x /opt/prepare_custom_image.sh

# --- systemd 关机回传 ---
cat > /etc/systemd/system/cos-backup.service << 'SV1'
[Unit]
Description=COS backup on shutdown
After=network-online.target
[Service]
Type=oneshot
RemainAfterExit=yes
ExecStop=/opt/cos_backup.sh
TimeoutStopSec=360
[Install]
WantedBy=multi-user.target
SV1
systemctl daemon-reload
systemctl enable cos-backup.service

# --- ZeroTier 自动入网（镜像启动后生成新身份并加入）---
cat > /etc/systemd/system/zt-auto.service << 'SVZ'
[Unit]
Description=Auto join ZeroTier network for image clones
After=network-online.target zerotier-one.service
Wants=network-online.target zerotier-one.service
[Service]
Type=oneshot
ExecStart=/opt/zt-auto.sh
RemainAfterExit=yes
[Install]
WantedBy=multi-user.target
SVZ
systemctl daemon-reload
systemctl enable zt-auto.service

# --- 开机拉取 COS 数据 ---
cat > /etc/systemd/system/init-data.service << 'SVD'
[Unit]
Description=Pull project data from COS on boot
After=network-online.target zt-auto.service
Wants=network-online.target
[Service]
Type=oneshot
ExecStart=/opt/init_data.sh
RemainAfterExit=yes
[Install]
WantedBy=multi-user.target
SVD
systemctl daemon-reload
systemctl enable init-data.service

# --- spot termination watcher ---
cat > /etc/systemd/system/spot-watch.service << 'SPW'
[Unit]
Description=Watch Tencent Cloud spot termination notice and back up COS
After=network-online.target
Wants=network-online.target
[Service]
Type=simple
ExecStart=/opt/spot_watch.sh
Restart=on-failure
RestartSec=5
[Install]
WantedBy=multi-user.target
SPW
systemctl daemon-reload
systemctl enable spot-watch.service
systemctl disable rc-local.service 2>/dev/null || true
rm -f /etc/rc.local /etc/systemd/system/rc-local.service

# --- crontab ---
(crontab -l 2>/dev/null; echo "*/10 * * * * /opt/cos_backup.sh") | crontab -

# --- 版本标记 ---
if [ ! -f /home/ubuntu/.codex/auth.json ] && [ ! -f /root/.codex/auth.json ]; then
    echo "ERROR: Codex login state was not found under /home/ubuntu/.codex or /root/.codex."
    echo "Run codex login before creating the image, then re-run this script."
    exit 1
fi

cat > /etc/mirror-info << INF
BUILD=$(date '+%Y-%m-%d %H:%M')
ZT=${ZT_NETWORK_ID}
COS=${COS_BUCKET}
REGION=${COS_REGION}
CODEX=$(codex --version 2>/dev/null || echo unknown)
CODEX_AUTH=present
INF

# --- 清理 ---
history -c
: > ~/.bash_history
apt clean
npm cache clean --force 2>/dev/null || true

echo "--- prepare image template ---"
/opt/prepare_custom_image.sh

echo ""
echo "==== V12 COMPLETE ===="
echo "  ZeroTier: fresh identity will be generated on next boot"
echo "  Codex:    $(codex --version 2>/dev/null || echo check)"
echo "  coscmd:   OK"
echo "  /opt:     $(find /opt -maxdepth 1 -name '*.sh' | wc -l) scripts"
echo "  Services: zt-auto, init-data, cos-backup, spot-watch enabled"
echo ""
echo "  Image behavior after first boot:"
echo "    - ZeroTier generates a fresh identity and auto-joins using /etc/image-template.env"
echo "    - Codex login state is preserved from this image"
echo "    - COS ${CHECKPOINT_DIR} are pulled into ${DATA_DIR} and owned by ubuntu"
echo "    - Shutdown/spot termination triggers COS backup"
echo ""
echo "  Before shutdown:"
echo "    - Run: sudo shutdown -h now"
echo "    - Create the custom image from the stopped instance in the cloud console"
