How those stuff were generated:

``` bash
$ openssl genpkey -out sample.key -algorithm RSA -pkeyopt rsa_keygen_bits:2048
$ openssl req -new -key sample1.key -out sample1.csr
$ openssl req -text -in sample1.csr -noout
$ openssl x509 -req -days 365 -in sample1.csr -signkey sample1.key -out sample1.crt
```