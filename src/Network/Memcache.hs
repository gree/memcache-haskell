
{- |
Module: Network.Memcache
Maintainer: Kiyoshi Ikehara <kiyoshi.ikehara@gree.net>

memcache-haskell is a memcache protocol library for server and client application.

If you want to implement a simple memcache client, just import "Network.Memcache" module.

>  import Network.Memcache
>  
>  main = do
>    mValue <- withClient "127.0.0.1:11211" $ \client -> get client "key"
>    print mValue

If your application is more complex and should recognize memcache command and response directly,
you have to import more and use low-level functions.

For server application, please use "Network.Memcache.Op" and "Network.Memcache.Response" to parse and process each command.

If you are familiar with conduit library, please take a look at memcache-conduit library and you will
find that you can write a server talking memcache protocol very quickly.

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

