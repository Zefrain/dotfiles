#!/usr/bin/env bash
set -euo pipefail

if (($# > 0)); then
  pattern="$*"
  if pkill -- "$pattern"; then
    echo "success"
    exit 0
  fi

  mapfile -t pids < <(pgrep -- "$pattern" || true)
  if ((${#pids[@]} > 0)) && kill -9 "${pids[@]}"; then
    echo "success"
    exit 0
  fi
else
  echo "please give argument to kill" >&2
  exit 2
fi

echo "failed" >&2
exit 1
