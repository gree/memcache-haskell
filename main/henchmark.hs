
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Memcache
import Network.Memcache.Op
import Network.Memcache.Response
import Control.Concurrent hiding (yield)


main = do
  runTCPClient (clientSettings 13302 "127.0.0.1") $ \appData -> do
    (appSource appData)
      $$ getResponseText
      =$ process
      =$ putOpText
      =$ (appSink appData)

process :: (MonadIO m) => ConduitM (Either BS.ByteString Response) Op m ()
process = loop 0
  where
    loop c = do
      if c < 1000000
        then do
        -- yield $ SetOp "key" 0 0 (fromIntegral $ BS.length value) value []
        yield $ PingOp
        mresp <- await
        case mresp of
          Just resp -> do
            liftIO $ putStr $ show resp ++ " "
            loop (c+1)
          Nothing -> return ()
        else do
        yield $ QuitOp
        return ()

