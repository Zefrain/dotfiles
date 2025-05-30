#!/usr/bin/env bash
set -euo pipefail

# ========= 配置 =========
readonly DEFAULT_PORT=22
readonly DEFAULT_DST="root@38.80.81.120:/root/$(basename "$PWD")/"
readonly DEFAULT_LOCAL_DIR="$PWD/"
readonly POLL_INTERVAL=10
declare RSYNC_OPTS="-azq --progress --exclude='.*/' --exclude='*~' --exclude='.git/' --exclude='.svn/' --exclude='.DS_Store' --exclude='*.swp' "
declare -a SYNC_MAPPINGS=()

# ========= 日志 =========
COLOR_RED='\033[0;31m'
COLOR_GREEN='\033[0;32m'
COLOR_RESET='\033[0m'
COLOR_YELLOW='\033[0;33m'

log() {
  local level="$1"
  local msg="$2"
  case "$level" in
  info) echo -e "${COLOR_GREEN}[INFO] $msg${COLOR_RESET}" ;;
  error) echo -e "${COLOR_RED}[ERROR] $msg${COLOR_RESET}" >&2 ;;
  tips) echo -e "${COLOR_YELLOW}[TIPS] $msg${COLOR_RESET}" ;;
  esac
}

# ========= 工具 =========
trim() {
  local str="$*"
  str="${str#"${str%%[![:space:]]*}"}"
  str="${str%"${str##*[![:space:]]}"}"
  printf '%s' "$str"
}

check_dependencies() {
  log info "检查依赖..."
  if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    if [ ! -f /etc/openwrt_release ]; then
      command -v rsync >/dev/null || sudo apt install -y rsync
      command -v fswatch >/dev/null || sudo apt install -y fswatch
    fi
  elif [[ "$OSTYPE" == "darwin"* ]]; then
    command -v fswatch >/dev/null || brew install fswatch
  fi
}

# ========= 输入与验证 =========
prompt_for_local_dirs() {
  while true; do
    log info "输入本地目录，'f' 结束添加"
    log tips "默认: $DEFAULT_LOCAL_DIR"
    read -r -p "> " local_dir
    local_dir=$(trim "$local_dir")

    [[ "$local_dir" == "f" ]] && break
    [[ -z "$local_dir" ]] && local_dir="$DEFAULT_LOCAL_DIR"

    if [ ! -d "$local_dir" ]; then
      log error "本地目录不存在: $local_dir"
      continue
    fi

    prompt_for_remote_targets "$local_dir"
  done
}

prompt_for_remote_targets() {
  local local_dir="$1"
  while true; do
    log info "为本地目录 $local_dir 添加远程目标，格式: user@host:/remote/path, 'f' 结束添加"
    log tips "默认: $DEFAULT_DST"
    read -r -p "> " dst
    dst=$(trim "$dst")

    [[ "$dst" == "f" ]] && break
    [[ -z "$dst" ]] && dst="$DEFAULT_DST"

    if [[ "$dst" != *:* ]]; then
      log error "格式错误，应包含 ':' 分隔远程路径"
      continue
    fi

    local user_host="${dst%%:*}"
    local remote_path="${dst#*:}"

    log info "为 $user_host 设置SSH端口"
    log tips "默认: 22"
    read -r -p "> " port
    port=$(trim "$port")
    [[ "$port" == "f" ]] && break
    [[ -z "$port" ]] && port="$DEFAULT_PORT"

    if ! [[ "$port" =~ ^[0-9]+$ ]]; then
      log error "无效端口号: $port"
      continue
    fi

    local mapping="$local_dir|$user_host|$remote_path|$port"

    # 避免重复
    if [[ ${#SYNC_MAPPINGS[@]} -gt 0 && " ${SYNC_MAPPINGS[*]} " == *" $mapping "* ]]; then
      log error "跳过重复映射: $mapping"
      continue
    fi

    SYNC_MAPPINGS+=("$mapping")
    log tips "添加映射: $local_dir => $user_host:$remote_path (port $port)"
  done

  prompt_for_rsync_opts
}

validate_remote_path() {
  local user_host="$1" remote_path="$2" port="$3"

  if ! ssh -p "$port" "$user_host" "echo 1" &>/dev/null; then
    log error "无法连接: $user_host (port $port)"
    return 1
  fi

  ssh -p "$port" "$user_host" "mkdir -p '$remote_path'" || {
    log error "远程目录创建失败: $remote_path"
    return 1
  }
}

# ========= 同步逻辑 =========
sync_loop() {
  if [[ "${#SYNC_MAPPINGS[@]}" -eq 0 ]]; then
    log error "没有设置任何同步映射"
    exit 1
  fi

  for mapping in "${SYNC_MAPPINGS[@]}"; do
    IFS="|" read -r local_dir user_host remote_path port <<<"$mapping"

    [[ ! -d "$local_dir" ]] && log error "本地目录不存在: $local_dir" && continue
    validate_remote_path "$user_host" "$remote_path" "$port" || continue

    if [[ "$OSTYPE" == "linux-gnu"* && -f /etc/openwrt_release ]]; then
      log info "OpenWrt轮询模式: $local_dir => $user_host:$remote_path, $port"
      while true; do
        rsync $RSYNC_OPTS -e "ssh -p $port" "$local_dir/" "$user_host:$remote_path/" ||
          log error "同步失败"
        sleep "$POLL_INTERVAL"
      done
    else
      log info "启动实时同步: $local_dir => $user_host:$remote_path, $port"
      rsync $RSYNC_OPTS -e "ssh -p $port" "$local_dir/" "$user_host:$remote_path/"
      fswatch -0 "$local_dir" | while read -r -d "" event; do
        rsync $RSYNC_OPTS -e "ssh -p $port" "$local_dir/" "$user_host:$remote_path/" &&
          log info "$(date '+%m-%d %H:%M:%S') 已同步: $event" ||
          log error "同步失败: $event"
      done
    fi
  done
}

prompt_for_rsync_opts() {
  log info "输入rsync选项: "
  log tips "默认: ${RSYNC_OPTS}"
  read -r -p "> " input_opts
  input_opts=$(trim "$input_opts")
  if [[ -n "$input_opts" ]]; then
    RSYNC_OPTS="$input_opts"
    log info "已设置rsync选项: $RSYNC_OPTS"
  else
    log info "使用默认rsync选项: $RSYNC_OPTS"
  fi
}

# ========= 主程序 =========
main() {
  check_dependencies
  prompt_for_local_dirs
  sync_loop
}

main
