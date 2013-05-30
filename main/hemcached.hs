
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
      $$ parseText
      =$ process (memStore ht)
      =$ response
      =$ (appSink appData)

memStore :: HashTable Key Value -> Op -> IO (Response)
memStore ht op = case op of
  SetOp key flags exptime bytes value options -> do
    H.insert ht key value
    return (Stored)
  GetOp (key:keys) -> do
    mValue <- H.lookup ht key
    case mValue of
      Just value -> return (Value key 0 (fromIntegral $ BS.length value) value (Just 0))
      Nothing -> return (Error)
  _ -> return (Error)

