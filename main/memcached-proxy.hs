
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
import qualified Data.ByteString.Char8 as BS
import Control.Concurrent hiding (yield)

import Network.Memcache.Op
import Network.Memcache.Response

main :: IO ()
main = do
  setNumCapabilities 4
  let sSettings0 = (serverSettings 13302 HostAny) :: ServerSettings IO
      sSettings = sSettings0  { serverAfterBind = (\s -> setSocketOption s NoDelay 1) }
  runTCPServer sSettings $ \appData -> do
    runTCPClient (clientSettings 13301 "127.0.0.1") (proxyLoop appData)
    
proxyLoop :: AppData IO -> AppData IO -> IO ()
proxyLoop serverData clientData = do
  (serverSource, ()) <- (appSource serverData $= getOpText) $$+ return ()
  (clientSource, ()) <- (appSource clientData $= getResponseText) $$+ return ()
  proxyLoop' serverSource (appSink serverData) clientSource (appSink clientData)

proxyLoop' :: (MonadIO m) =>
              ResumableSource m (Either String Op)
              -> Sink BS.ByteString m a1
              -> ResumableSource m Response
              -> Sink BS.ByteString m a
              -> m ()
proxyLoop' serverSource serverSink clientSource clientSink = do
  (serverSource', mop) <- serverSource $$++ await
  case mop of
    Nothing -> return ()
    Just (Left msg) -> do
      yield Error $$ (putResponseText =$ serverSink)
      proxyLoop' serverSource' serverSink clientSource clientSink
    Just (Right op) -> case op of
      GetOp keys -> do
        yield op $$ (putOpText =$ clientSink)
        clientSource' <- proxyGet clientSource
        proxyLoop' serverSource' serverSink clientSource' clientSink
      _ -> do
        yield op $$ (putOpText =$ clientSink)
        (clientSource', mresp) <- clientSource $$++ await
        case mresp of
          Nothing -> return ()
          Just resp -> do
            yield resp $$ (putResponseText =$ serverSink)
            proxyLoop' serverSource' serverSink clientSource' clientSink
  where
    proxyGet clientSource = do
      (clientSource', mresp) <- clientSource $$++ await
      case mresp of
        Nothing -> return (clientSource')
        Just resp -> case resp of
          Value {} -> do
            yield resp $$ (putResponseText =$ serverSink)
            proxyGet clientSource'
          End -> do
            yield End $$ (putResponseText =$ serverSink)
            return (clientSource')


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
