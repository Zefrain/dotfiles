#!/bin/env bash

set -euxo

build_dir=$PWD
coin_dir="$1"
coin_name="$2"
install_dir="/root/COINS"

config() {
  CONFIG_SITE="/root/dogecoin-1.14.9/depends/$HOST/share/config.site" ./configure --disable-shared --prefix="$install_dir/$coin_name/$HOST" --host="$HOST" --with-incompatible-bdb &&
    make clean && make install -j 8
}

config_fallback() {
  make -C depends
  CONFIG_SITE="$PWD/depends/$HOST/share/config.site" ./configure --disable-shared --prefix="$install_dir/$coin_name/$HOST" --host="$HOST" --with-incompatible-bdb &&
    make clean && make install -j 8
}

for HOST in x86_64-pc-linux-gnu x86_64-w64-mingw32; do
  cd "$coin_dir"
  ./autogen.sh

  if ! config; then
    config_fallback
  fi

  make clean && make install -j 8

  cd "$build_dir"
done

cd "$install_dir"
7z a "${coin_name}.zip" "$coin_name"
tar czf "${coin_name}.tar.gz" "$coin_name"
scp "${coin_name}.zip" "${coin_name}.tar.gz" zhou@192.168.131.47:/Volumes/bfyf_share/zefrain/COINS/
cd "$build_dir"
