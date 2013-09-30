
{- |
Module: Network.Memcache
Maintainer: Kiyoshi Ikehara <kiyoshi.ikehara@gree.net>

memcache-haskell is a memcache protocol library for server and client application.

If you want to implement a simple memcache client, please use functions in "Network.Memcache.Client" module.

For server application, please use "Network.Memcache.Op" and "Network.Memcache.Response" to parse and process each command.

If you are familiar with conduit library, please take a look at memcache-conduit library and you will
find that you can write a server taking memcache protocol very quickly.

> main :: IO ()
> main = do
>   runResourceT $ do
>     runTCPServer (serverSettings 13301 HostAny) $ \appData -> do
>       (appSource appData)
>         $$ getOpText
>         =$ process htVar
>         =$ putResponseText
>         =$ (appSink appData)

-}

module Network.Memcache (
    module Network.Memcache.Client
  , module Network.Memcache.Stats
  ) where

import Network.Memcache.Client
import Network.Memcache.Stats

