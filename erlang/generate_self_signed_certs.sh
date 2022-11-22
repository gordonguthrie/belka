#!/bin/bash
# first create a certificate authority
echo "create a certificate authority..."
openssl req -x509 \
            -sha256 -days 356 \
            -nodes \
            -newkey rsa:2048 \
            -subj "/CN=localhost/C=UK/L=Edinburgh" \
            -keyout ./priv/keys/rootCA.key -out ./priv/keys/rootCA.crt

# now create a server certificate
echo "generate a server certificate..."
openssl genrsa -out ./priv/keys/server.key 2048

# create a conf file to handle the signing
echo "create a conf file to handle the signing..."
cat > ./priv/keys/csr.conf <<EOF
[ req ]
default_bits = 2048
prompt = no
default_md = sha256
req_extensions = req_ext
distinguished_name = dn

[ dn ]
C = UK
ST = Scotland
L = Edinburgh
O = GordonGuthrie
OU = GordonGuthrie
CN = localhost

[ req_ext ]
subjectAltName = @alt_names

[ alt_names ]
DNS.1 = localhost

EOF

# generate Certificate Signing Request (CSR)
echo "create a CSR..."
openssl req -new -key ./priv/keys/server.key -out ./priv/keys/server.csr -config ./priv/keys/csr.conf

# create an external file
echo "create a certificate conf file..."
cat > ./priv/keys/cert.conf <<EOF

authorityKeyIdentifier=keyid,issuer
basicConstraints=CA:FALSE
keyUsage = digitalSignature, nonRepudiation, keyEncipherment, dataEncipherment
subjectAltName = @alt_names

[alt_names]
DNS.1 = localhost

EOF

# generate SSL certificate with the self signed CA
echo "generate an SSL certificate..."
openssl x509 -req \
    -in ./priv/keys/server.csr \
    -CA ./priv/keys/rootCA.crt -CAkey ./priv/keys/rootCA.key \
    -CAcreateserial -out ./priv/keys/server.crt \
    -days 365 \
    -sha256 -extfile ./priv/keys/cert.conf