#!/usr/bin/env bash
set -euo pipefail

DEFAULT_IGNORE="*.bak,*.bak_*,*.backup,*.backup_*,*_backup_*,*_backups_*,*_backups_*/*,*.old,*.orig,*.tmp,*~,.*.swp,*.swp,.DS_Store,.git/*,__pycache__/*"

usage() {
  cat <<'USAGE'
Usage: cos_download.sh <src_cos_dir> <dest_dir>

Download files from COS with the current coscmd config.

Arguments:
  src_cos_dir  COS source directory, for example checkpoints/.
  dest_dir     Local destination directory.

Environment:
  COS_IGNORE        Override comma-separated ignore patterns.
  COS_EXTRA_IGNORE  Append comma-separated ignore patterns.

Examples:
  cos_download.sh checkpoints/ /data
  COS_EXTRA_IGNORE='*.log,cache/*' cos_download.sh checkpoints/ .
USAGE
}

build_ignore_rules() {
  local ignore="${COS_IGNORE:-$DEFAULT_IGNORE}"
  if [[ -n "${COS_EXTRA_IGNORE:-}" ]]; then
    ignore="${ignore},${COS_EXTRA_IGNORE}"
  fi
  printf '%s' "$ignore"
}

main() {
  if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
    usage
    return 0
  fi

  if (($# != 2)); then
    usage >&2
    return 2
  fi

  command -v coscmd >/dev/null 2>&1 || {
    echo "coscmd not found" >&2
    return 127
  }

  local src_cos_dir="$1"
  local dest_dir="$2"
  local ignore_rules
  ignore_rules="$(build_ignore_rules)"

  mkdir -p "$dest_dir"
  coscmd download -r -s --ignore "$ignore_rules" "$src_cos_dir" "$dest_dir"
}

main "$@"
