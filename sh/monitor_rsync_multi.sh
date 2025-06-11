#!/usr/bin/env bash
set -euo pipefail

# ========= 配置 =========
readonly DEFAULT_PORT=22
readonly DEFAULT_REMOTE="root@38.80.81.120:/root/$(basename "$PWD")"
readonly POLL_INTERVAL=1
readonly DEFAULT_EXCLUDES=".* *~ .git .svn .DS_Store *.swp __pycache__ node_modules venv .venv"
declare -a SYNC_MAPPINGS=()
declare -a CHILD_PIDS=()

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

# ========= 清理函数 =========
cleanup() {
  log info "收到退出信号，清理子进程..."
  for pid in "${CHILD_PIDS[@]}"; do
    if kill -0 "$pid" 2>/dev/null; then
      # 向整个进程组发送SIGKILL
      kill -KILL -- -$pid 2>/dev/null &&
        log info "已终止进程组 $pid"
    fi
  done

  # 确保所有子进程真正退出
  wait "${CHILD_PIDS[@]}" 2>/dev/null
  log info "所有子进程已终止"
  exit 0
}
trap 'cleanup' SIGINT SIGTERM EXIT

# ========= 工具函数 =========
trim() {
  local str="$*"
  str="${str#"${str%%[![:space:]]*}"}"
  str="${str%"${str##*[![:space:]]}"}"
  printf '%s' "$str"
}

get_default_excludes() {
  local excludes=$DEFAULT_EXCLUDES

  # 添加.gitignore内容（如果存在）
  if [[ -f ".gitignore" ]]; then
    while IFS= read -r line; do
      [[ -n "$line" && ! "$line" =~ ^# ]] && excludes+=" $line"
    done <".gitignore"
  fi

  echo "$excludes"
}

prompt_with_default() {
  local prompt="$1"
  local default="$2"
  local var_name="$3"

  read -r -p "$prompt [默认: $default] " input
  input=$(trim "$input")
  [[ -z "$input" ]] && input="$default"

  printf -v "$var_name" '%s' "$input"
}

# ========= 输入函数 =========
prompt_sync_direction() {
  while true; do
    log info "选择同步方向:"
    log info "  1) 推送 (本地 → 远程)"
    log info "  2) 拉取 (远程 → 本地)"
    log info "  输入 'f' 完成配置"
    read -r -p "> " choice

    case "$choice" in
    1) prompt_push_mapping ;;
    2) prompt_pull_mapping ;;
    f) return ;;
    *) log error "无效选择" ;;
    esac
  done
}

prompt_push_mapping() {
  local source_dir
  prompt_with_default "输入本地源目录" "$PWD" source_dir

  if [[ ! -d "$source_dir" ]]; then
    log error "目录不存在: $source_dir"
    return
  fi

  local remote_dest
  prompt_with_default "输入远程目标" "$DEFAULT_REMOTE" remote_dest

  if [[ "$remote_dest" != *:* ]]; then
    log error "格式错误，应包含 ':' 分隔远程路径"
    return
  fi

  local user_host="${remote_dest%%:*}"
  local remote_path="${remote_dest#*:}"
  local port
  prompt_with_default "SSH端口" "$DEFAULT_PORT" port

  if ! [[ "$port" =~ ^[0-9]+$ ]]; then
    log error "无效端口号: $port"
    return
  fi

  # 排除规则处理
  local exclude_rules
  echo "输入排除规则（空格分隔，回车使用默认，输入空格表示不排除）"
  echo "[默认: $DEFAULT_EXCLUDES]"
  read -r -p "> " exclude_input

  if [[ -z "$exclude_input" ]]; then # 直接回车（空输入）
    exclude_rules=$(get_default_excludes)
  elif [[ "$(trim "$exclude_input")" == "" ]]; then # 输入空格
    exclude_rules=""                                # 显式设为空字符串（不排除）
  else
    exclude_rules="$exclude_input" # 正常输入
  fi

  local mapping="local_to_remote|$source_dir|$user_host|$remote_path|$port|$exclude_rules"
  SYNC_MAPPINGS+=("$mapping")
  log tips "添加推送映射: $source_dir → $user_host:$remote_path (端口 $port)"
}

prompt_pull_mapping() {
  local remote_source
  prompt_with_default "输入远程源 (user@host:/path)" "$DEFAULT_REMOTE" remote_source

  if [[ "$remote_source" != *:* ]]; then
    log error "格式错误，应包含 ':' 分隔远程路径"
    return
  fi

  local user_host="${remote_source%%:*}"
  local remote_path="${remote_source#*:}"
  local port
  prompt_with_default "SSH端口" "$DEFAULT_PORT" port

  if ! [[ "$port" =~ ^[0-9]+$ ]]; then
    log error "无效端口号: $port"
    return
  fi

  local local_dest
  prompt_with_default "输入本地目标目录" "$PWD" local_dest
  mkdir -p "$local_dest" || {
    log error "无法创建目录: $local_dest"
    return
  }

  # 排除规则处理
  local exclude_rules
  echo "输入排除规则（空格分隔，回车使用默认，输入空格表示不排除）"
  echo "[默认: $DEFAULT_EXCLUDES]"
  read -r -p "> " exclude_input

  if [[ -z "$exclude_input" ]]; then # 直接回车（空输入）
    exclude_rules=$(get_default_excludes)
  elif [[ "$(trim "$exclude_input")" == "" ]]; then # 输入空格
    exclude_rules=""                                # 显式设为空字符串（不排除）
  else
    exclude_rules="$exclude_input" # 正常输入
  fi

  local mapping="remote_to_local|$user_host|$remote_path|$port|$local_dest|$exclude_rules"
  SYNC_MAPPINGS+=("$mapping")
  log tips "添加拉取映射: $user_host:$remote_path → $local_dest (端口 $port)"
}

# ========= 同步逻辑 =========
build_rsync_opts() {
  local exclude_rules="$1"
  local -a opts=(-azq --progress --compress-level=9)

  [[ -n "$exclude_rules" ]] && {
    IFS=' ' read -ra rules <<<"$exclude_rules"
    for pattern in "${rules[@]}"; do
      opts+=(--exclude="$pattern")
    done
  }

  echo "${opts[@]}"
}

sync_remote_to_local() {
  local user_host="$1" remote_path="$2" port="$3" local_target="$4" exclude_rules="$5"

  log info "启动远程到本地同步: $user_host:$remote_path → $local_target (每${POLL_INTERVAL}秒轮询)"
  local rsync_opts
  rsync_opts=$(build_rsync_opts "$exclude_rules")

  # 子进程自己的清理函数
  child_cleanup() {
    log info "子进程 $$ 收到终止信号，结束同步任务"
    kill -KILL 0 2>/dev/null
    exit 0
  }
  trap 'child_cleanup' SIGTERM SIGINT EXIT

  while true; do
    rsync $rsync_opts -e "ssh -p $port" "$user_host:$remote_path/" "$local_target/" &&
      log info "$(date '+%m-%d %H:%M:%S') 同步完成: 远程 → 本地" ||
      log error "同步失败: $user_host:$remote_path → $local_target"
    sleep "$POLL_INTERVAL"
  done
}

sync_local_to_remote() {
  local source_dir="$1" user_host="$2" remote_path="$3" port="$4" exclude_rules="$5"

  # 验证远程路径
  if ! ssh -p "$port" "$user_host" "mkdir -p '$remote_path'" 2>/dev/null; then
    log error "远程目录创建失败: $remote_path"
    return
  fi

  local rsync_opts
  rsync_opts=$(build_rsync_opts "$exclude_rules")

  # 子进程自己的清理函数
  child_cleanup() {
    log info "子进程 $$ 收到终止信号，结束同步任务"
    kill -KILL 0 2>/dev/null
    exit 0
  }
  trap 'child_cleanup' SIGTERM SIGINT EXIT

  if [[ "$OSTYPE" == "linux-gnu"* && -f /etc/openwrt_release ]]; then
    log info "轮询模式: $source_dir → $user_host:$remote_path (每${POLL_INTERVAL}秒)"
    while true; do
      rsync $rsync_opts -e "ssh -p $port" "$source_dir/" "$user_host:$remote_path/" ||
        log error "同步失败"
      sleep "$POLL_INTERVAL"
    done
  else
    log info "启动实时同步: $source_dir → $user_host:$remote_path"
    rsync $rsync_opts -e "ssh -p $port" "$source_dir/" "$user_host:$remote_path/"
    fswatch -0 "$source_dir" | while read -r -d "" event; do
      rsync $rsync_opts -e "ssh -p $port" "$source_dir/" "$user_host:$remote_path/" &&
        log info "$(date '+%m-%d %H:%M:%S') 已同步: $event" ||
        log error "同步失败: $event"
    done
  fi
}

# ========= 主同步循环 =========
sync_loop() {
  [[ "${#SYNC_MAPPINGS[@]}" -eq 0 ]] && {
    log error "未配置任何同步任务"
    exit 1
  }

  for mapping in "${SYNC_MAPPINGS[@]}"; do
    IFS="|" read -r direction arg1 arg2 arg3 arg4 arg5 <<<"$mapping"

    (
      case "$direction" in
      local_to_remote) sync_local_to_remote "$arg1" "$arg2" "$arg3" "$arg4" "$arg5" ;;
      remote_to_local) sync_remote_to_local "$arg1" "$arg2" "$arg3" "$arg4" "$arg5" ;;
      esac
    ) &

    CHILD_PIDS+=("$!")
    log info "启动子进程 PID: $! ($direction)"
  done

  wait
}

# ========= 主程序 =========
main() {
  prompt_sync_direction
  sync_loop
}

main
