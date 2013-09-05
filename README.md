
memcache-conduit
================

Installation
------------

    > git clone https://github.com/gree/memcache-haskell.git
    > git clone https://github.com/gree/memcache-conduit.git
    > cabal install memcache-haskell/ memcache-conduit/

Sample code
-----------

### hemcached

hemcached is a simple memcached server to demonstrate how to use the memcache conduit functions.
(expiration and eviction are not implemented.)

1) Launch hemcached

    > ~/.cabal/bin/hemcached

2) Open another terminal window

    > telnet localhost 11211
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.

3) Send memcache commands

    set key 0 0 5
    value
    STORED
    get key
    VALUE key 0 5
    value
    end

If you want to know further information, please see the file "main/hemcached.hs" in this repository.

