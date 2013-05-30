
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Memcache


main :: IO ()
main = do
  runTCPServer (serverSettings 13302 HostAny) $ \appData -> do
    runTCPClient (clientSettings 13301 "ckvs1001") (proxyLoop appData)
    
proxyLoop :: AppData IO -> AppData IO -> IO ()
proxyLoop serverData clientData = do
  (serverSource, ()) <- (appSource serverData $= getOpText) $$+ return ()
  (clientSource, ()) <- (appSource clientData $= getResponseText) $$+ return ()
  loop serverSource (appSink serverData) clientSource (appSink clientData)
  where
    loop serverSource serverSink clientSource clientSink = do
      (serverSource', mop) <- serverSource $$++ await
      print mop
      case mop of
        Nothing -> return ()
        Just op -> do
          yield op $$ (putOpText =$ clientSink)
          (clientSource', mresp) <- clientSource $$++ await
          case mresp of
            Nothing -> return ()
            Just resp -> do
              yield resp $$ (putResponseText =$ serverSink)
              loop serverSource' serverSink clientSource' clientSink

