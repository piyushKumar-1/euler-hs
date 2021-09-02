FROM httpd:2.4-alpine

COPY server/server.cert.pem /usr/local/apache2/conf/server.crt
COPY server/server.key.pem /usr/local/apache2/conf/server.key
COPY server/httpd.conf /usr/local/apache2/conf/httpd.conf
COPY server/httpd-ssl.conf /usr/local/apache2/conf/extra/httpd-ssl.conf
COPY intermediate/certs/ca-chain-bundle.cert.pem /usr/local/apache2/conf/ca-chain-bundle.cert.pem