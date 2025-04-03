#!/bin/bash
# 功能：跨平台批量主机密钥验证工具（支持HOST:PORT格式）
# 版本：v2.6
#
# CHANGELOG:
#   v2.6: 完善主机列表的HOST:PORT解析，动态支持端口配置
#   v2.5: 主机列表文件内容格式完善
#   v2.4: 连接时不保存known_hosts
#   v2.3: 修复getopts 缺失 -r 读取
#   v2.2: 修正验证逻辑
#   v2.1: 解决macbook上xargs的兼容问题
#   v2.0: 为可读性和可维护性进行重构

#######################################
# 初始化配置
#######################################
declare -r DEFAULT_HOSTS_FILE="hostlist"
declare -r COLOR_SUCC='\033[32m'
declare -r COLOR_FAIL='\033[31m'
declare -r COLOR_WARN='\033[33m'
declare -r COLOR_RESET='\033[0m'
declare -r DEFAULT_SSH_PORT=22

#######################################
# 参数解析
#######################################
parse_arguments() {
  local usage="Usage: $0 [-H 主机列表] [-r ROOT密钥] [-i 用户密钥] [-u 测试用户] [-l 日志文件] [-h]
    Options:
    -H  主机列表文件 (默认: ./hostlist)
    -r  ROOT私钥路径 (必需)
    -i  用户私钥路径 (必需)
    -u  测试用户名 (必需)
    -l  日志文件路径 (可选)
    -h  显示帮助信息"

  if [[ $# -eq 0 ]]; then
    echo -e "$usage"
    exit 0
  fi

  while getopts ":H:i:u:l:r:h" opt; do
    case $opt in
    H) host_lst="$OPTARG" ;;
    i) priv_key="$OPTARG" ;;
    u) test_user="$OPTARG" ;;
    l) log_file="$OPTARG" ;;
    r) root_key="$OPTARG" ;;
    h)
      echo -e "$usage"
      exit 0
      ;;
    \?) fail "无效参数: -$OPTARG\n$usage" ;;
    :) fail "选项 -$OPTARG 需要参数\n$usage" ;;
    esac
  done

  : ${host_lst:=$DEFAULT_HOSTS_FILE}
}

#######################################
# 前置检查
#######################################
precheck() {
  [ ! -f "$host_lst" ] && fail "主机列表文件不存在: $host_lst"
  [ ! -f "$root_key" ] && fail "ROOT密钥文件不存在: $root_key"
  [ ! -f "$priv_key" ] && fail "用户密钥文件不存在: $priv_key"
  [ -z "$test_user" ] && fail "测试用户未指定"
  chmod 600 "$priv_key" 2>/dev/null || fail "私钥权限设置失败"
}

#######################################
# 清空用户密钥操作（新增端口支持）
#######################################
clear_authorized_keys() {
  local host=$1
  local port=$2
  ssh -p "$port" -i "$root_key" -o ConnectTimeout=5 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null root@$host "
    for user_home in /home/*; do
      user=\$(basename \$user_home)
      auth_file=\"\$user_home/.ssh/authorized_keys\"
      [ -f \"\$auth_file\" ] && truncate -s 0 \"\$auth_file\" && echo \"\${user}@${host}: 清空成功\"
    done
  " 2>/dev/null
}

#######################################
# 连接性验证（新增端口支持）
#######################################
verify_connection() {
  local host=$1
  local port=$2
  local ssh_output
  local exit_code

  ssh_output=$(ssh -p "$port" -i "$priv_key" \
    -o ConnectTimeout=5 \
    -o identitiesOnly=yes \
    -o StrictHostKeyChecking=no \
    -o UserKnownHostsFile=/dev/null \
    -o PasswordAuthentication=no \
    $test_user@$host "echo '探测连接'" 2>&1)
  exit_code=$?

  if [ $exit_code -eq 0 ]; then
    log "${COLOR_FAIL}× ${test_user}@${host}:${port} 验证失败（仍能连接，密钥未清理）${COLOR_RESET}"
    return 1
  else
    if [[ "$ssh_output" == *"Permission denied"* ]]; then
      log "${COLOR_SUCC}√ ${test_user}@${host}:${port} 验证成功（密钥已清理）${COLOR_RESET}"
      return 0
    else
      log "${COLOR_WARN}! ${test_user}@${host}:${port} 异常失败（原因：${ssh_output//$'\n'/ })${COLOR_RESET}"
      return 2
    fi
  fi
}

#######################################
# 主机行处理（优化格式解析）
#######################################
process_line() {
  local line=$1
  # 去除注释和空白字符
  line=$(echo "$line" | sed -e 's/#.*//' -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')
  [ -z "$line" ] && return

  # 解析主机和端口
  local host port
  IFS=: read host port <<< "$line"
  port=${port:-$DEFAULT_SSH_PORT}

  log "${COLOR_WARN}>> 处理主机: ${host}:${port}${COLOR_RESET}"
  
  # 清空操作
  if clear_result=$(clear_authorized_keys "$host" "$port"); then
    while IFS= read -r result_line; do
      log "$result_line"
    done <<<"$clear_result"
  else
    log "${COLOR_FAIL}SSH连接失败: ${host}:${port}${COLOR_RESET}"
  fi

  # 验证操作
  if verify_connection "$host" "$port"; then
    echo "${host}:${port}" >>"$SUCCESS_FILE"
  else
    echo "${host}:${port}" >>"$FAILURE_FILE"
  fi
}

#######################################
# 主执行流程（优化并发处理）
#######################################
main() {
  export SUCCESS_FILE=$(mktemp)
  export FAILURE_FILE=$(mktemp)
  trap cleanup EXIT

  parse_arguments "$@"
  precheck
  init_logger

  log "==== 开始批量处理 ===="
  log "目标主机列表: $host_lst"
  log "测试用户账户: $test_user"
  log "使用私钥文件: $priv_key\n"

  # 高效并发处理（兼容MacOS）
  while IFS= read -r line || [ -n "$line" ]; do
    # 打印注释行（支持带缩进）
    if [[ "$line" =~ ^[[:space:]]*# ]]; then
      log "$line"
      continue
    fi
    process_line "$line" &
    [ $(jobs -r | wc -l) -ge 4 ] && wait -n
  done < "$host_lst"  # 移除过滤注释的grep
  wait

  show_summary
}

# 其余函数保持不变（略）
