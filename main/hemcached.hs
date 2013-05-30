
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


type Key = BS.ByteString
type Value = BS.ByteString
type HashTable k v = H.BasicHashTable k v

main :: IO ()
main = do
  ht <- H.new
  runTCPServer (serverSettings 13301 HostAny) $ \appData -> do
    (appSource appData)
      $$ getOpText
      =$ process ht
      =$ putResponseText
      =$ (appSink appData)

process :: MonadIO m => HashTable Key Value -> ConduitM Op Response m ()
process ht = do
  mOpType <- await
  case mOpType of
    Nothing -> return ()
    Just op -> case op of
      QuitOp -> do
        return ()
      SetOp key flags exptime bytes value options -> do
        liftIO $ H.insert ht key value
        yield (Stored)
        process ht
      GetOp keys -> do
        processGet keys
        yield (End)
        process ht
      _ -> do
        yield (Error)
        process ht
  where
    processGet [] = return ()
    processGet (key:rest) = do
        mValue <- liftIO $ H.lookup ht key
        case mValue of
          Just value -> yield (Value key 0 (fromIntegral $ BS.length value) value (Just 0))
          Nothing -> return ()
        processGet rest
      
  
