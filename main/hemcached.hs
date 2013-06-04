
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
      SetOp key flags exptime bytes value options -> do
        liftIO $ H.insert ht key value
        yield' (Noreply `elem` options) Stored
        process ht
      CasOp key flags exptime bytes version value options -> do -- XXX
        liftIO $ H.insert ht key value
        yield' (Noreply `elem` options) Stored
        process ht
      AddOp key flags exptime bytes value options -> do
        resp <- liftIO $ do
          mValue <- H.lookup ht key
          case mValue of
            Nothing -> do
              liftIO $ H.insert ht key value
              return (Stored)
            Just value -> return (NotStored)
        yield' (Noreply `elem` options) resp
        process ht
      ReplaceOp key flags exptime bytes value options -> do
        resp <- liftIO $ do
          mValue <- H.lookup ht key
          case mValue of
            Nothing -> return (NotStored)
            Just value -> do
              liftIO $ H.insert ht key value
              return (Stored)
        yield' (Noreply `elem` options) resp
        process ht
      AppendOp key flags exptime bytes value options -> do
        append' True key flags exptime bytes value options
        process ht
      PrependOp key flags exptime bytes value options -> do
        append' False key flags exptime bytes value options
        process ht
      GetOp keys -> do
        processGet False keys
        yield End
        process ht
      GetsOp keys -> do
        processGet True keys
        yield End
        process ht
      DeleteOp key options -> do
        liftIO $ H.delete ht key
        yield' (Noreply `elem` options) Deleted
        process ht
      IncrOp key value options -> do
        incr' True key value options
        process ht
      DecrOp key value options -> do
        incr' False key value options
        process ht
      TouchOp key exptime options -> do
        resp <- liftIO $ do
          mValue <- H.lookup ht key
          case mValue of
            Nothing -> return (NotFound)
            Just value -> return (Touched) -- XXX
        yield' (Noreply `elem` options) resp
        process ht
      PingOp -> do
        yield Ok
        process ht
      FlushAllOp -> do
        ht' <- liftIO $ H.new
        yield Ok
        process ht'
      VersionOp -> do
        yield (Version "hemcached-0.0.1")
        process ht
      QuitOp -> return ()
      StatsOp args -> do
        yield' (isNoreplyOp op) Error
        process ht
  where
    incr' isIncr key value options = do
      resp <- liftIO $ do
        mValue <- H.lookup ht key
        case mValue of
          Nothing -> return (NotFound)
          Just value' -> do
            let r = if isIncr then read (BS.unpack value') + value else read (BS.unpack value') - value
            liftIO $ H.insert ht key (BS.pack $ show r)
            return (Code r)
      yield' (Noreply `elem` options) resp

    append' isAppend key flags exptime bytes value options = do
      resp <- liftIO $ do
        mValue <- H.lookup ht key
        case mValue of
          Nothing -> return (NotStored)
          Just value' -> do
            liftIO $ H.insert ht key (BS.concat $ if isAppend then [value', value] else [value, value'])
            return (Stored)
      yield' (Noreply `elem` options) resp

    yield' isNoreply resp = if isNoreply then return () else yield resp
    
    processGet _ [] = return ()
    processGet withVersion (key:rest) = do
        mValue <- liftIO $ H.lookup ht key
        case mValue of
          Just value -> yield (Value key 0 (fromIntegral $ BS.length value) value (if withVersion then Just 0 else Nothing))
          Nothing -> return ()
        processGet withVersion rest
      
  
