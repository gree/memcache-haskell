
{- |
Module: Network.Memcache
Maintainer: Kiyoshi Ikehara <kiyoshi.ikehara@gree.net>

memcache-haskell is a memcache protocol library for server and client application.

If you want to implement a simple memcache client, just import "Network.Memcache" module and use withClient function.

> import Network.Memcache
>   
> main = do
>   mValue <- withClient "127.0.0.1:11211" $ \client -> get client "key"
>   case mValue of
>     Nothing -> putStrLn "(no value)"
>     Just value -> putStrLn value

Or, use openClient and closeClient pair for more complex cases.

> import Control.Exception
> import Control.Monad.Trans.Resource (allocate, release, runResourceT)
> import Control.Monad.IO.Class
> import Network.Memcache
> import System.Environment
> 
> main :: IO ()
> main = do
>   args <- getArgs
>   case args of
>     [] -> main' "127.0.0.1:11211"
>     (nk:_) -> main' nk
> 
> main' :: Nodekey -> IO ()
> main' nodekey = runResourceT $ do
>   (rkeyClient , client) <- flip allocate closeClient $ do
>     c <- openClient nodekey
>     case c of
>       Just client -> return (client)
>       Nothing -> liftIO $ do
>         throwIO (userError "could not open.")
>   liftIO $ do
>     ret <- set client "key" "foo"
>     print ret
>     ret' <- get client "key" :: IO (Maybe String)
>     print ret'
>   release rkeyClient

If your application is more complex and should recognize memcache command and response directly,
you have to import more and use low-level functions.

For server application, please use "Network.Memcache.Op" and "Network.Memcache.Response" directly to parse and process each command.

If you are familiar with conduit library, please take a look at memcache-conduit library and you will
find that you can write a memcache protocol server very quickly.

> main :: IO ()
> main = do
>   runResourceT $ do
>     runTCPServer (serverSettings 13301 HostAny) $ \appData -> do
>       (appSource appData)
>         $$ getOpText
>         =$ process htVar
>         =$ putResponseText
>         =$ (appSink appData)
>
> process = ...

-}

module Network.Memcache (
    module Network.Memcache.Client
  , module Network.Memcache.Stats
  , module Network.Memcache.Class
  ) where

import Network.Memcache.Class (Key, Value, Message)
import Network.Memcache.Client
import Network.Memcache.Stats

