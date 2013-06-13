
{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Main where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Memcache
import qualified Data.HashTable.IO as H
import Network.Memcache.Class
import Network.Memcache.Op
import Network.Memcache.Response
import Control.Monad
import Control.Concurrent hiding (yield)
import Data.Word
import Control.Exception
import Control.Monad.Trans.Resource

type Key = BS.ByteString
type Version = Word64
type Value = (Version, BS.ByteString)
type HashTable k v = H.BasicHashTable k v

main :: IO ()
main = do
  setNumCapabilities 4
  ht <- H.new :: IO (HashTable Key Value)
  htVar <- newMVar ht
  runResourceT $ do
    runTCPServer (serverSettings 13301 HostAny) $ \appData -> do
      (appSource appData)
        $$ getOpText
        =$ process htVar
        =$ putResponseText
        =$ (appSink appData)

process :: (MonadResource m, MonadIO m) => MVar (HashTable Key Value) -> ConduitM (Either BS.ByteString Op) Response m ()
process htVar = loop
  where
    loop :: (MonadResource m, MonadIO m) => ConduitM (Either t Op) Response m ()
    loop = do
      meOp <- await
      case meOp of
        Nothing -> return ()
        Just (Left msg) -> do
          yield Error
          loop
        Just (Right op) -> case op of
          SetOp key flags exptime bytes value options -> do
            with $ \ht -> do
              mValue <- lookup ht key
              case mValue of
                Just (version', _) -> insert ht key (version' + 1, value)
                Nothing -> insert ht key (0, value)
              yield' options Stored
            loop
          CasOp key flags exptime bytes version value options -> do
            with $ \ht -> do
              mValue <- lookup ht key
              case mValue of
                Nothing -> yield' options NotFound
                Just (version', _) -> case version == version' of
                  True -> do
                    insert ht key (version' + 1, value)
                    yield' options Stored
                  False -> do
                    yield' options Exists
            loop
          AddOp key flags exptime bytes value options -> do
            with $ \ht -> do
              mValue <- lookup ht key
              case mValue of
                Nothing -> do
                  insert ht key (1, value)
                  yield' options Stored
                Just _ -> yield' options NotStored
            loop
          ReplaceOp key flags exptime bytes value options -> do
            with $ \ht -> do
              mValue <- lookup ht key
              case mValue of
                Nothing -> yield' options NotStored
                Just (version', _value') -> do
                  insert ht key (version' + 1, value)
                  yield' options Stored
            loop
          AppendOp key flags exptime bytes value options -> do
            append' True key flags exptime bytes value options
            loop
          PrependOp key flags exptime bytes value options -> do
            append' False key flags exptime bytes value options
            loop
          GetOp keys -> do
            processGet False keys
            yield End
            loop
          GetsOp keys -> do
            processGet True keys
            yield End
            loop
          DeleteOp key options -> do
            with $ \ht -> do
              delete ht key
              yield' options Deleted
            loop
          IncrOp key value options -> do
            incr' True key value options
            loop
          DecrOp key value options -> do
            incr' False key value options
            loop
          TouchOp key exptime options -> do
            ht <- liftIO $ takeMVar htVar
            resp <- liftIO $ do
              mValue <- H.lookup ht key
              case mValue of
                Nothing -> return (NotFound)
                Just (_, value) -> return (Touched) -- XXX
            liftIO $ putMVar htVar ht
            yield' options resp
            loop
          PingOp -> do
            yield Ok
            loop
          FlushAllOp -> do
            ht <- liftIO $ takeMVar htVar
            ht' <- liftIO $ H.new
            liftIO $ putMVar htVar ht'
            yield Ok
            loop
          VersionOp -> do
            yield (Version "hemcached-0.0.1")
            loop
          QuitOp -> return ()
          StatsOp args -> do
            yield End
            loop

    incr' isIncr key value options = do
      with $ \ht -> do
        mValue <- lookup ht key
        case mValue of
          Nothing -> yield' options NotFound
          Just (version', value') -> do
            let r = if isIncr then read (BS.unpack value') + value else read (BS.unpack value') - value
            insert ht key (version' + 1, (BS.pack $ show r))
            yield' options $ Code r

    append' isAppend key flags exptime bytes value options = do
      with $ \ht -> do
        mValue <- lookup ht key
        case mValue of
          Nothing -> yield' options NotStored
          Just (version', value') -> do
            insert ht key (version' + 1, BS.concat $ if isAppend then [value', value] else [value, value'])
            yield' options Stored

    processGet _ [] = return ()
    processGet withVersion (key:rest) = do
      with $ \ht -> do
        mValue <- lookup ht key
        case mValue of
          Just (version, value) -> do
            yield (Value key 0 (fromIntegral $ BS.length value) value (if withVersion then Just version else Nothing))
          Nothing -> return ()
      processGet withVersion rest


    yield' options resp = when (Noreply `notElem` options) $ yield resp
    
    delete ht key = liftIO $ H.delete ht key
    
    lookup ht key = liftIO $ H.lookup ht key

    insert ht key value = liftIO $ H.insert ht key value

    with act = bracketP lock unlock act

    lock = liftIO $ takeMVar htVar
    
    unlock ht= liftIO $ putMVar htVar ht

  
