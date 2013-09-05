
{- |
Module: Network.Memcache
Maintainer: Kiyoshi Ikehara <kiyoshi.ikehara@gree.net>

memcache-haskell is a memcache protocol library for server and client application.

> main :: IO ()
> main = do
>   runResourceT $ do
>     runTCPServer (serverSettings 13301 HostAny) $ \appData -> do
>       (appSource appData)
>         $$ getOpText
>         =$ process htVar
>         =$ putResponseText
>         =$ (appSink appData)

For memcache server, please use "Network.Memcache.Op" and "Network.Memcache.Response" to parse and process each command.

-}

module Network.Memcache (
    module Network.Memcache.Client
  , module Network.Memcache.Stats
  ) where

import Network.Memcache.Client
import Network.Memcache.Stats

