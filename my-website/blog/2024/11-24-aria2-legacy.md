---
title: Aria2 OSSL_PROVIDER_load 'legacy' failed
Tags: [openwrt, openssl, aria2] 
---

# Openwrt Legacy OpenSsl error #2152

> /etc/init.d/aria2 add BOLD line.
> procd_add_jail "$NAME.$section" log
> **procd_add_jail_mount "/usr/lib" #fix "errorCode=1 OSSL_PROVIDER_load 'legacy' failed"**
> procd_add_jail_mount "$ca_certificate" "$certificate" "$rpc_certificate" "$rpc_private_key"
> procd_add_jail_mount_rw "$dir" "$config_dir" "$log"
> procd_close_instance

- Solution:

```bash
opkg update && opkg install openssl-legacy
```



# Reference:

- [Openwrt Legacy OpenSsl error · Issue #2152 · aria2/aria2](https://github.com/aria2/aria2/issues/2152)
