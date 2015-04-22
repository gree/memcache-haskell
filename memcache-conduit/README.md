memcache-conduit
================

This package provides conduit functions for memcache protocol.

For detail, please see '''hemcached''' sample code (sample/hemcached.hs).


Installation
------------

Install with sample(hemcached)

    > cabal install -f sample memcache-haskell memcache-conduit

Install without sample(hemcached)

    > cabal install memcache-haskell memcache-conduit

Sample code
-----------

### hemcached(memcache-sample-hemcached)

hemcached(memcache-sample-hemcached) is a simple memcached server to demonstrate how to use the memcache conduit functions.
(expiration and eviction are not implemented.)

1) Launch hemcached

    > ~/.cabal/bin/memcache-sample-hemcached

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

If you want to know further information, please see the file "sample/hemcached.hs" in this repository.

## Contributors

* Kiyoshi Ikehara
* Junji Hashimoto

## License

See [LICENSE](LICENSE).

Copyright © Gree, Inc. All Rights Reserved.
