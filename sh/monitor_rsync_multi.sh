#!/usr/bin/env bash
set -euo pipefail

# ================== 配置区 ==================
declare -a sources=() targets=()
readonly DEFAULT_SRC="$PWD"
readonly DEFAULT_DST="root@38.80.81.120:/root/$(basename $(pwd))"
readonly RSYNC_OPTS="-azq --progress --exclude='.*' --exclude='*~'"
readonly POLL_INTERVAL=10
declare PORT=22
COLOR_RED='\033[0;31m'
COLOR_GREEN='\033[0;32m'
COLOR_RESET='\033[0m'

# ================== 工具函数 ==================
trim() {
  local var="$*"
  var="${var#"${var%%[![:space:]]*}"}" # 清理前导空格[2](@ref)
  var="${var%"${var##*[![:space:]]}"}" # 清理尾部空格[2](@ref)
  printf '%s' "$var"
}

log() {
  local level=$1
  local message=$2
  case $level in
  error) echo -e "${COLOR_RED}[ERROR] $message${COLOR_RESET}" >&2 ;;
  info) echo -e "${COLOR_GREEN}[INFO] $message${COLOR_RESET}" ;;
  esac
}

# ================== 依赖检查 ==================
check_dependencies() {
  if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    if [ -f /etc/openwrt_release ]; then
      log info "OpenWrt环境使用轮询模式"
    else
      command -v rsync &>/dev/null || sudo apt install -y rsync
      command -v fswatch &>/dev/null || sudo apt install -y fswatch
    fi
  elif [[ "$OSTYPE" == "darwin"* ]]; then
    command -v fswatch &>/dev/null || brew install fswatch
  fi
}

# ================= 设置端口号 ==================
prompt_ssh_port() {
  read -r -p "请输入SSH端口号 (默认: $PORT): " input_port
  if [[ -n "$input_port" ]]; then
    PORT=$(trim "$input_port")
    if ! [[ "$PORT" =~ ^[0-9]+$ ]]; then
      log error "无效的端口号"
      exit 1
    fi
  fi
}

# ================== 输入处理 ==================
setup_default_mapping() {
  local default_src default_dst
  default_src=$(trim "${DEFAULT_SRC}") # 默认源目录[2](@ref)
  default_dst=$(trim "${DEFAULT_DST}") # 默认目标[2](@ref)

  if [[ ${#sources[@]} -eq 0 ]]; then
    sources+=("$default_src")
    targets+=("$default_dst")
    log info "已设置默认同步路径: $default_src => $default_dst"
  fi

  if [[ ${#sources[@]} -ne ${#targets[@]} ]]; then
    log error "源和目标路径数量不匹配"
    exit 1
  fi
}

prompt_custom_mappings() {
  log info "输入自定义路径 (格式: /local/path => user@host:/remote/path)"
  log info "直接回车使用默认路径: $DEFAULT_SRC => $DEFAULT_DST"
  log info "输入 0 重置所有映射"

  while read -r -p "> " mapping; do
    custom_mappings=$(trim "$mapping")
    [ -z "$custom_mappings" ] && break

    # 新增：处理重置逻辑
    if [[ "$custom_mappings" = "0" ]]; then
      sources=()
      targets=()
      log info "已重置所有映射"
      continue # 跳过后续处理
    fi

    IFS="=>" read -r src_raw dst_raw <<<"$custom_mappings"
    local src=$(trim "$src_raw")
    local dst=$(trim "${dst_raw/>/}")

    if [[ -n "$src" && -n "$dst" ]]; then
      sources+=("$src")
      targets+=("$dst")
      log info "已添加映射: $src => $dst"
    else
      log error "无效输入，请按格式输入"
    fi
  done
}

# ================== 路径验证 ==================
validate_path() {
  local path=$1
  if [[ "$path" == *":"* ]]; then # 远程路径检查
    local host=${path%%:*}
    local dir=${path#*:}
    local host_port="${path%%:*}"
    if ! ssh -p "$PORT" "$host_port" echo &>/dev/null; then
      log error "SSH 连接失败: $host_port"
      return 1
    fi
    if ! ssh -p "$PORT" "$host" "test -d '$dir'" &>/dev/null; then
      log info "远程目录不存在，正在创建: ${path/#*:/}"
      ssh -p "$PORT" "$host" "mkdir -p ${path/#*:/}" || {
        log error "远程目录创建失败: $path"
        return 1
      }
      return 0
    fi
  elif [ ! -d "$path" ]; then # 本地目录检查[2](@ref)
    log error "本地目录不存在: $path"
    return 1
  fi
}

# ================== 同步引擎 ==================
start_sync() {
  for i in "${!sources[@]}"; do
    local src="${sources[$i]}"
    local dst="${targets[$i]}"

    if ! validate_path "$src" || ! validate_path "$dst"; then
      continue
    fi

    if [[ "$OSTYPE" == "linux-gnu"* && -f /etc/openwrt_release ]]; then
      log info "OpenWrt轮询同步: $src => $dst"
      while true; do
        rsync $RSYNC_OPTS -e "ssh -p $PORT" "$src/" "$dst/" || log error "同步失败"
        sleep $POLL_INTERVAL
      done
    else
      log info "实时监控同步: $src => $dst"
      fswatch -0 "$src" | while read -r -d "" event; do
        rsync $RSYNC_OPTS -e "ssh -p $PORT" "$src/" "$dst/" &&
          log info "已同步: $event" ||
          log error "同步失败: $event"
      done
    fi
  done
}

# ================== 主流程 ==================
main() {
  check_dependencies
  prompt_custom_mappings
  prompt_ssh_port
  setup_default_mapping
  start_sync
}

main
trap 'pkill -P $$; exit' SIGINT
