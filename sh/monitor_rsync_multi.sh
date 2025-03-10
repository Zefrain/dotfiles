#!/usr/bin/env bash
set -euo pipefail

# ================== 配置区 ==================
declare -ga sources=() targets=()
readonly DEFAULT_SRC="$PWD"
readonly DEFAULT_DST="root@192.168.168.161:/home/ubuntu/dogecoin"
readonly RSYNC_OPTS="-avhq --progress --exclude=".*" --exclude="*~""
readonly POLL_INTERVAL=10
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

# ================== 输入处理 ==================
setup_default_mapping() {
  local default_src default_dst
  default_src=$(trim "${DEFAULT_SRC}") # 默认源目录[2](@ref)
  default_dst=$(trim "${DEFAULT_DST}") # 默认目标[2](@ref)

  sources+=("$default_src")
  targets+=("$default_dst")
  log info "已设置默认同步路径: $default_src => $default_dst"
}

prompt_custom_mappings() {
  log info "输入自定义路径 (格式: /local/path => user@host:/remote/path)"
  log info "直接回车使用默认路径"

  while read -r -p "> " mapping; do
    mapping=$(trim "$mapping") # 清理输入空格[2](@ref)
    [ -z "$mapping" ] && break

    IFS="=>" read -r src_raw dst_raw <<<"$mapping"
    local src=$(trim "$src_raw")
    local dst=$(trim "${dst_raw/>/}") # 处理残留的">"符号[2](@ref)

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
    if ! ssh "$host" "test -d '$dir'" &>/dev/null; then
      log error "远程目录不可访问: $path"
      return 1
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
        rsync $RSYNC_OPTS "$src/" "$dst/" || log error "同步失败"
        sleep $POLL_INTERVAL
      done &
    else
      log info "实时监控同步: $src => $dst"
      fswatch -0 "$src" | while read -d "" event; do
        rsync $RSYNC_OPTS "$src/" "$dst/" &&
          log info "已同步: $event" ||
          log error "同步失败: $event"
      done &
    fi
  done
  wait
}

# ================== 主流程 ==================
main() {
  check_dependencies
  setup_default_mapping
  prompt_custom_mappings
  start_sync
}

trap 'pkill -P $$; exit' SIGINT
main
