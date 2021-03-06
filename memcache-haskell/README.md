memcache-haskell
================

This package is a memcache protocol library for server and client applications.

Main purpose of this library is making a server itself.

This does not support consistent hashing  and any other functions like ["libmemcached"](http://libmemcached.org/libMemcached.html).

## Getting started

Install from Hackage.

    cabal update && cabal install memcache-haskell

## Usage

1. Call `openClient` with `ip-address:port` of memcache-server to obtain a handle.
2. To set or get a data on the memcache server, call `set` or `get` with the handle.
3. Finally call `closeClient` after you are done.

Example:

```
main :: IO ()
main = do
  client <- openClient "127.0.0.0:11211"
  ret <- set client "key" "foo"
  print ret
  ret' <- get client "key" :: IO (Maybe String)
  print ret'
  closeClient client
```

## Contributors

* Kiyoshi Ikehara
* Yuji Kamiya
* Junji Hashimoto

## License

See [LICENSE](LICENSE).

Copyright © Gree, Inc. All Rights Reserved.
