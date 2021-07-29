
test/tls/
|-- certs        -- ^ copies of all of the certificates that we issue with our CA
|-- cient
|-- intermediate -- ^ intermediate CA
|   |-- certs
|   |-- csr      -- ^ CSRs
|   `-- private
|-- private      -- ^ a copy of the CA certificate’s private key
|-- server


password (the only one): welcome

# CA

common name: ca01

``` bash
; gen key
# openssl genrsa -des3 -out private/cakey.pem 4096
; verify key
# openssl rsa -noout -text -in private/cakey.pem
; generate cert
# openssl req -new -x509 -days 3650 -config openssl.cnf -extensions v3_ca -key private/cakey.pem -out certs/cacert.pem
; convert to PEM
# openssl x509 -in certs/cacert.pem -out certs/cacert.pem -outform PEM
; verify cert
# openssl x509 -noout -text -in certs/cacert.pem
```

# intermediate CA

common name: ca02

```bash
# cd intermediate
; gen intermediate key
# openssl genrsa -des3 -out intermediate/private/intermediate.cakey.pem 4096
; gen csr
# openssl req -new -sha256 -config openssl.cnf -key private/intermediate.cakey.pem -out csr/intermediate.csr.pem
; sign and generate intermediate CA cert
# cd ../
# openssl ca -config openssl.cnf -extensions v3_intermediate_ca -days 2650 -notext -batch -in intermediate/csr/intermediate.csr.pem -out intermediate/certs/intermediate.cacert.pem
; verify certificate
# openssl x509 -noout -text -in intermediate/certs/intermediate.cacert.pem
; verify against the root certificate
# openssl verify -CAfile certs/cacert.pem intermediate/certs/intermediate.cacert.pem
; convert to PEM format
# openssl x509 -in intermediate/certs/intermediate.cacert.pem -out intermediate/certs/intermediate.cacert.pem -outform PEM
```

# certificate bundle chain

```bash
; create chain bundle
# cat intermediate/certs/intermediate.cacert.pem certs/cacert.pem > intermediate/certs/ca-chain-bundle.cert.pem
; verify bundle
# openssl verify -CAfile certs/cacert.pem intermediate/certs/ca-chain-bundle.cert.pem
```

# client

common name: client01

```bash
# cd client
; gen key
# openssl genrsa -out client.key.pem 4096
; csr
# openssl req -new -key client.key.pem -out client.csr -config ../openssl.cnf
; sign & cert
# openssl x509 -req -in client.csr -CA ../intermediate/certs/ca-chain-bundle.cert.pem -CAkey ../intermediate/private/intermediate.cakey.pem -out client.cert.pem -CAcreateserial -days 365 -sha256 -extfile client_cert_ext.cnf
; pack into pkcs12
# openssl pkcs12 -export -inkey client.key.pem -in client.cert.pem -out cert_key.p12
```

Verify client's stuff:

```bash
# openssl rsa -noout -text -in client.key.pem
# openssl req -noout -text -in client.csr
# openssl x509 -noout -text -in client.cert.pem
```

# server

common name: server01

```bash
# cd ../server
# openssl genrsa -out server.key.pem 4096
# openssl req -new -key server.key.pem -out server.csr -config ../openssl.cnf
# openssl x509 -req -in server.csr -CA ../intermediate/certs/ca-chain-bundle.cert.pem -CAkey ../intermediate/private/intermediate.cakey.pem -out server.cert.pem -CAcreateserial -days 365 -sha256 -extfile server_cert_ext.cnf
```

Verify server's stuff:

```bash
# openssl rsa -noout -text -in server.key.pem
# openssl req -noout -text -in server.csr
# openssl x509 -noout -text -in server.cert.pem
```

