
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Conduit
import Data.Conduit.Network hiding (runTCPClient)
import Data.Conduit.Network.Internal
import Data.Conduit.Memcache
import Network.Socket

import Network.Memcache.Response

main :: IO ()
main = do
  let sSettings0 = (serverSettings 13302 HostAny) :: ServerSettings IO
      sSettings = sSettings0  { serverAfterBind = (\s -> setSocketOption s NoDelay 1) }
  runTCPServer sSettings $ \appData -> do
    runTCPClient (clientSettings 13301 "127.0.0.1") (proxyLoop appData)
    
proxyLoop :: AppData IO -> AppData IO -> IO ()
proxyLoop serverData clientData = do
  (serverSource, ()) <- (appSource serverData $= getOpText) $$+ return ()
  (clientSource, ()) <- (appSource clientData $= getResponseText) $$+ return ()
  loop serverSource (appSink serverData) clientSource (appSink clientData)
  where
    loop serverSource serverSink clientSource clientSink = do
      (serverSource', mop) <- serverSource $$++ await
      case mop of
        Nothing -> return ()
        Just (Left msg) -> do
          yield Error $$ (putResponseText =$ serverSink)
          loop serverSource' serverSink clientSource clientSink
        Just (Right op) -> do
          yield op $$ (putOpText =$ clientSink)
          (clientSource', mresp) <- clientSource $$++ await
          case mresp of
            Nothing -> return ()
            Just resp -> do
              yield resp $$ (putResponseText =$ serverSink)
              loop serverSource' serverSink clientSource' clientSink

runTCPClient :: (MonadIO m, MonadBaseControl IO m) => ClientSettings m -> Application m -> m ()
runTCPClient settings app = control $ \run -> bracket
    (getSocket (clientHost settings) (clientPort settings))
    (sClose . fst)
    (\(s, address) -> do
        setSocketOption s NoDelay 1
        run $ app AppData { appSource = sourceSocket s
                          , appSink = sinkSocket s
                          , appSockAddr = address
                          , appLocalAddr = Nothing
                          })
