
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

type Key = BS.ByteString
type Value = BS.ByteString
type HashTable k v = H.BasicHashTable k v

main :: IO ()
main = do
  setNumCapabilities 4
  ht <- H.new
  htVar <- newMVar ht
  runTCPServer (serverSettings 13301 HostAny) $ \appData -> do
    (appSource appData)
      $$ getOpText
      =$ process htVar
      =$ putResponseText
      =$ (appSink appData)

process :: MonadIO m => MVar (HashTable Key Value) -> ConduitM (Either String Op) Response m ()
process htVar = do
  mOpType <- await
  case mOpType of
    Nothing -> return ()
    Just (Left msg) -> do
      yield Error
      process htVar
    Just (Right op) -> case op of
      SetOp key flags exptime bytes value options -> do
        ht <- liftIO $ takeMVar htVar
        liftIO $ H.insert ht key value
        liftIO $ putMVar htVar ht
        when (Noreply `notElem` options) $ yield Stored
        process htVar
      CasOp key flags exptime bytes version value options -> do -- XXX
        ht <- liftIO $ takeMVar htVar
        liftIO $ H.insert ht key value
        liftIO $ putMVar htVar ht
        when (Noreply `notElem` options) $ yield Stored
        process htVar
      AddOp key flags exptime bytes value options -> do
        ht <- liftIO $ takeMVar htVar
        resp <- liftIO $ do
          mValue <- H.lookup ht key
          case mValue of
            Nothing -> do
              liftIO $ H.insert ht key value
              return (Stored)
            Just value -> return (NotStored)
        liftIO $ putMVar htVar ht
        when (Noreply `notElem` options) $ yield resp
        process htVar
      ReplaceOp key flags exptime bytes value options -> do
        ht <- liftIO $ takeMVar htVar
        resp <- liftIO $ do
          mValue <- H.lookup ht key
          case mValue of
            Nothing -> return (NotStored)
            Just value -> do
              liftIO $ H.insert ht key value
              return (Stored)
        liftIO $ putMVar htVar ht
        when (Noreply `notElem` options) $ yield resp
        process htVar
      AppendOp key flags exptime bytes value options -> do
        append' True key flags exptime bytes value options
        process htVar
      PrependOp key flags exptime bytes value options -> do
        append' False key flags exptime bytes value options
        process htVar
      GetOp keys -> do
        processGet False keys
        yield End
        process htVar
      GetsOp keys -> do
        processGet True keys
        yield End
        process htVar
      DeleteOp key options -> do
        ht <- liftIO $ takeMVar htVar
        liftIO $ H.delete ht key
        liftIO $ putMVar htVar ht
        when (Noreply `notElem` options) $ yield Deleted
        process htVar
      IncrOp key value options -> do
        incr' True key value options
        process htVar
      DecrOp key value options -> do
        incr' False key value options
        process htVar
      TouchOp key exptime options -> do
        ht <- liftIO $ takeMVar htVar
        resp <- liftIO $ do
          mValue <- H.lookup ht key
          case mValue of
            Nothing -> return (NotFound)
            Just value -> return (Touched) -- XXX
        liftIO $ putMVar htVar ht
        when (Noreply `notElem` options) $ yield resp
        process htVar
      PingOp -> do
        yield Ok
        process htVar
      FlushAllOp -> do
        ht <- liftIO $ takeMVar htVar
        ht' <- liftIO $ H.new
        liftIO $ putMVar htVar ht'
        yield Ok
        process htVar
      VersionOp -> do
        yield (Version "hemcached-0.0.1")
        process htVar
      QuitOp -> return ()
      StatsOp args -> do
        yield End
        process htVar
  where
    incr' isIncr key value options = do
      ht <- liftIO $ takeMVar htVar
      resp <- liftIO $ do
        mValue <- H.lookup ht key
        case mValue of
          Nothing -> return (NotFound)
          Just value' -> do
            let r = if isIncr then read (BS.unpack value') + value else read (BS.unpack value') - value
            liftIO $ H.insert ht key (BS.pack $ show r)
            return (Code r)
      liftIO $ putMVar htVar ht
      when (Noreply `notElem` options) $ yield resp

    append' isAppend key flags exptime bytes value options = do
      ht <- liftIO $ takeMVar htVar
      resp <- liftIO $ do
        mValue <- H.lookup ht key
        case mValue of
          Nothing -> return (NotStored)
          Just value' -> do
            liftIO $ H.insert ht key (BS.concat $ if isAppend then [value', value] else [value, value'])
            return (Stored)
      liftIO $ putMVar htVar ht
      when (Noreply `notElem` options) $ yield resp

    processGet _ [] = return ()
    processGet withVersion (key:rest) = do
      ht <- liftIO $ takeMVar htVar
      mValue <- liftIO $ H.lookup ht key
      liftIO $ putMVar htVar ht
      case mValue of
        Just value -> yield (Value key 0 (fromIntegral $ BS.length value) value (if withVersion then Just 0 else Nothing))
        Nothing -> return ()
      processGet withVersion rest
      
  
