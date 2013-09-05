memcache-conduit
================

How to use
----------

    > git clone ...
    > cabal install memcached-haskell/ memcached-conduit/

Sample code
-----------

### hemcached

hemcached is a simple memcached server to demonstrate how to use the memcache conduit functions.
(expiration and eviction are not implemented.)

Launch hemcached

    > ~/.cabal/bin/hemcached

Open another terminal window

    > telnet localhost 11211
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.

Send memcache commands

    set key 0 0 5
    value
    STORED
    get key
    VALUE key 0 5
    value
    end

