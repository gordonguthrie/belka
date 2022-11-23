valentina
=====

An OTP application

Build
-----

    $ rebar3 compile


Configure
---------

Needs a self signed cerficate:
```
mkdir ./priv/keys
cd ./priv/keys
openssl req -newkey rsa:2048 -new -nodes -x509 -days 3650 -keyout key.pem -out cert.pem
```
