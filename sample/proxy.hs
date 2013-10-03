
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}


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
import System.Console.GetOpt
import System.Environment (getArgs)

import Network.Memcache.Op
import Network.Memcache.Response

data ProgramOptions = ProgramOptions {
    optHost :: String
  , optPort :: Int
  , optListenPort :: Int
  } deriving (Show)

defaultOptions :: ProgramOptions
defaultOptions = ProgramOptions {
    optHost       = "127.0.0.1"
  , optPort       = 12121
  , optListenPort = 12122
  }

options :: [OptDescr (ProgramOptions -> ProgramOptions)]
options =
  [ Option ['d'] ["dest"] (ReqArg (\h opt -> opt { optHost = h })             "HOST") "destination host"
  , Option ['p'] ["dport"] (ReqArg (\h opt -> opt { optPort = read h })       "PORT") "destination port"
  , Option ['l'] ["lport"] (ReqArg (\h opt -> opt { optListenPort = read h }) "PORT") "listen port"
  ]

memcachedProxyOpts :: [String] -> IO (ProgramOptions, [String])
memcachedProxyOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: memcached-proxy [OPTION...]"

main :: IO ()
main = do
  args0 <- getArgs
  (opts, _args) <- memcachedProxyOpts args0
  setNumCapabilities 4
  let sSettings0 = (serverSettings (optListenPort opts) HostAny) :: ServerSettings IO
      sSettings = sSettings0  { serverAfterBind = (\s -> setSocketOption s NoDelay 1) }
  runTCPServer sSettings $ \appData -> do
    runTCPClient (clientSettings (optPort opts) (BS.pack $ optHost opts)) (proxyLoop appData)
    
proxyLoop :: AppData IO -> AppData IO -> IO ()
proxyLoop serverData clientData = do
  (serverSource, ()) <- (appSource serverData $= getOpText) $$+ return ()
  (clientSource, ()) <- (appSource clientData $= getResponseText) $$+ return ()
  proxyLoop' serverSource (appSink serverData) clientSource (appSink clientData)

proxyLoop' :: (MonadIO m) =>
              ResumableSource m (Either BS.ByteString Op)
              -> Sink BS.ByteString m a1
              -> ResumableSource m (Either BS.ByteString Response)
              -> Sink BS.ByteString m a
              -> m ()
proxyLoop' serverSource serverSink clientSource clientSink = do
  (serverSource', mop) <- serverSource $$++ await
  case mop of
    Nothing -> return ()
    Just (Left _msg) -> do
      yield Error $$ (putResponseText =$ serverSink)
      proxyLoop' serverSource' serverSink clientSource clientSink
    Just (Right op) -> case op of
      GetOp {} -> do
        yield op $$ (putOpText =$ clientSink)
        clientSource' <- proxyGet clientSource
        proxyLoop' serverSource' serverSink clientSource' clientSink
      _ -> do
        yield op $$ (putOpText =$ clientSink)
        (clientSource', mresp) <- clientSource $$++ await
        case mresp of
          Nothing -> return ()
          Just (Left _msg) -> do
            yield Error $$ (putResponseText =$ serverSink)
            return ()
          Just (Right resp) -> do
            yield resp $$ (putResponseText =$ serverSink)
            proxyLoop' serverSource' serverSink clientSource' clientSink
  where
    proxyGet source = do
      (source', mresp) <- source $$++ await
      case mresp of
        Nothing -> return (source')
        Just (Left _msg) -> do
          yield Error $$ (putResponseText =$ serverSink)
          return (source')
        Just (Right resp) -> case resp of
          Value {} -> do
            yield resp $$ (putResponseText =$ serverSink)
            proxyGet source'
          End -> do
            yield End $$ (putResponseText =$ serverSink)
            return (source')
          _ -> do
            yield Error $$ (putResponseText =$ serverSink)
            return (source')

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
