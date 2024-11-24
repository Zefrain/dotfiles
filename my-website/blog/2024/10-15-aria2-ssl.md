---
title: aria2 SSL/TLS error on openwrt
---
# aria2 SSL/TLS error on openwrt

> aria2 -> [SocketCore.cc:1019] errorCode=1 SSL/TLS handshake failure: protocol error
<!-- truncate -->
根据文档：

> --ca-certificate= \<FILE\> Use the certificate authorities in FILE to verify the peers. The certificate file must be in PEM format and can contain multiple CA certificates. Use --check-certificate option to enable verification. **Note** If you build with OpenSSL or the recent version of GnuTLS which has gnutls_certificate_set_x509_system_trust() function and the library is properly configured to locate the system-wide CA certificates store, aria2 will automatically load those certificates at the startup. **Note** WinTLS and AppleTLS do not support this option. Instead you will have to import the certificate into the OS trust store.

- Solution:

```
ca-certificate=/etc/ssl/certs/ca-certificates.crt
```

