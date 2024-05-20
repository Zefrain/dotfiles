#!/bin/env bash

for inter in $(ls /sys/class/net); do
if [[ $inter != 'lo' ]] && [[ ! -e /etc/netplan/99-$inter.yaml ]]; then
cat << EOF > /etc/netplan/99-$inter.yaml
network:
  ethernets:
    $inter:
      dhcp4: true
  version: 2
EOF
ip link set dev $inter up
fi
done
netplan apply
