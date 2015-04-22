-- Copyright (c) 2013, GREE, Inc. All rights reserved.
-- authors: Kiyoshi Ikehara <kiyoshi.ikehara@gree.net>

{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Main where

import Prelude hiding (lookup)
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Memcache
import qualified Data.HashTable.IO as H
import Control.Monad
import Control.Concurrent hiding (yield)
import Data.Word
import Control.Monad.Trans.Resource

import Network.Memcache.Op
import Network.Memcache.Response

type Key = BS.ByteString
type Version = Word64
type Value = (Version, BS.ByteString)
type HashTable k v = H.BasicHashTable k v

main :: IO ()
main = do
  ht <- H.new :: IO (HashTable Key Value)
  htVar <- newMVar ht
  runTCPServer (serverSettings 11211 "*") $ \appData -> do
    runResourceT $ do
      (appSource appData)
        $$ getOpText
        =$ process htVar
        =$ putResponseText
        =$ (appSink appData)

process :: MVar (HashTable Key Value) -> ConduitM (Either BS.ByteString Op) Response (ResourceT IO) ()
process htVar = loop
  where
    loop :: ConduitM (Either t Op) Response (ResourceT IO) ()
    loop = do
      meOp <- await
      case meOp of
        Nothing -> return ()
        Just (Left _msg) -> yield Error >> loop
        Just (Right op) -> case op of
          SetOp key _flags _exptime _bytes value options -> do
            with $ \ht -> do
              mValue <- lookup ht key
              case mValue of
                Just (version', _) -> insert ht key (version' + 1, value)
                Nothing -> insert ht key (0, value)
              yield' options Stored
            loop
          CasOp key _flags _exptime _bytes version value options -> do
            with $ \ht -> do
              mValue <- lookup ht key
              case mValue of
                Nothing -> yield' options NotFound
                Just (version', _) -> case version == version' of
                  True -> do
                    insert ht key (version' + 1, value)
                    yield' options Stored
                  False -> yield' options Exists
            loop
          AddOp key _flags _exptime _bytes value options -> do
            with $ \ht -> do
              mValue <- lookup ht key
              case mValue of
                Nothing -> do
                  insert ht key (1, value)
                  yield' options Stored
                Just _ -> yield' options NotStored
            loop
          ReplaceOp key _flags _exptime _bytes value options -> do
            with $ \ht -> do
              mValue <- lookup ht key
              case mValue of
                Nothing -> yield' options NotStored
                Just (version', _value') -> do
                  insert ht key (version' + 1, value)
                  yield' options Stored
            loop
          AppendOp key flags exptime bytes value options -> append' True key flags exptime bytes value options >> loop
          PrependOp key flags exptime bytes value options -> append' False key flags exptime bytes value options >> loop
          GetOp keys -> processGet False keys >> loop
          GetsOp keys -> processGet True keys >> loop
          DeleteOp key options -> do
            with $ \ht -> do
              delete ht key
              yield' options Deleted
            loop
          IncrOp key value options -> incr' True key value options >> loop
          DecrOp key value options -> incr' False key value options >> loop
          TouchOp key _exptime options -> do
            with $ \ht -> do
              mValue <- lookup ht key
              case mValue of
                Nothing -> yield' options NotFound
                Just (_, _value) -> yield' options Touched -- XXX
            loop
          PingOp -> yield Ok >> loop
          FlushAllOp -> do
            liftIO $ takeMVar htVar >> H.new >>= putMVar htVar
            yield Ok
            loop
          VersionOp -> yield (Version "hemcached-0.0.1") >> loop
          QuitOp -> return ()
          StatsOp _args -> yield End >> loop

    incr' isIncr key value options = do
      with $ \ht -> do
        mValue <- lookup ht key
        case mValue of
          Nothing -> yield' options NotFound
          Just (version', value') -> do
            let r = if isIncr then read (BS.unpack value') + value else read (BS.unpack value') - value
            insert ht key (version' + 1, BS.pack $ show r)
            yield' options $ Code r

    append' isAppend key _flags _exptime _bytes value options = do
      with $ \ht -> do
        mValue <- lookup ht key
        case mValue of
          Nothing -> yield' options NotStored
          Just (version', value') -> do
            insert ht key (version' + 1, BS.concat $ if isAppend then [value', value] else [value, value'])
            yield' options Stored

    processGet _ [] = yield End
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
    
    unlock ht = liftIO $ putMVar htVar ht

  
