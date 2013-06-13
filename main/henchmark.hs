
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Memcache
import qualified Data.HashTable.IO as H
import Network.Memcache.Op
import Network.Memcache.Response
import Control.Monad
import Control.Concurrent hiding (yield)


main = do
  runTCPClient (clientSettings 13302 "127.0.0.1") $ \appData -> do
    (appSource appData)
      $$ getResponseText
      =$ process
      =$ putOpText
      =$ (appSink appData)

process :: (MonadIO m) => ConduitM (Either BS.ByteString Response) Op m ()
process = loop
  where
    loop = do
      yield $ SetOp "key" 0 0 4 "1234" []
      mresp <- await
      liftIO $ print mresp
      loop

