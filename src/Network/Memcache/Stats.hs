
module Network.Memcache.Stats (
    getValue
  , getPid
  , getUptime
  , getTime
  , getVersion
  , getCurrItems
  , getBytes
  , getCmdGet
  , getCmdSet
  , getGetHits
  , getGetMisses
  , getBytesRead
  , getBytesWritten
  , getLimitMaxbytes
  ) where

import Data.Int
import Data.List.Split

import Network.Memcache.Client

getValue :: String -> StatsList -> Maybe String
getValue key statsList = lookup key statsList

--  pid
getPid :: StatsList -> Maybe Int
getPid = getValueAsInt "pid"

--  uptime
getUptime :: StatsList -> Maybe Int64
getUptime = getValueAsInt64 "uptime"

--  time
getTime :: StatsList -> Maybe Int64
getTime = getValueAsInt64 "time"

--  version
getVersion :: StatsList -> Maybe [Int]
getVersion statsList = do
  version <- getValue "version" statsList
  case splitOn "." version of
    (major:minor:rev:_) -> return [read major, read minor, read rev]
    _ -> Nothing

--  pointer_size

--  rusage_user

--  rusage_system

--  curr_items
getCurrItems :: StatsList -> Maybe Int64
getCurrItems = getValueAsInt64 "curr_items"

--  bytes
getBytes :: StatsList -> Maybe Int64
getBytes = getValueAsInt64 "bytes"

--  curr_connections

--  total_connections

--  connection_structures

--  cmd_get
getCmdGet :: StatsList -> Maybe Int64
getCmdGet = getValueAsInt64 "cmd_get"

--  cmd_set
getCmdSet :: StatsList -> Maybe Int64
getCmdSet = getValueAsInt64 "cmd_set"

--  get_hits
getGetHits :: StatsList -> Maybe Int64
getGetHits = getValueAsInt64 "get_hits"

--  get_misses
getGetMisses :: StatsList -> Maybe Int64
getGetMisses = getValueAsInt64 "get_misses"

--  evictions

--  bytes_read
getBytesRead :: StatsList -> Maybe Int64
getBytesRead = getValueAsInt64 "bytes_read"

--  bytes_written
getBytesWritten :: StatsList -> Maybe Int64
getBytesWritten = getValueAsInt64 "bytes_written"

--  limit_maxbytes
getLimitMaxbytes :: StatsList -> Maybe Int64
getLimitMaxbytes = getValueAsInt64 "limit_maxbytes"

--  threads

--  pool_threads

----------------------------------------------------------------

getValueAsInt64 :: String -> StatsList -> Maybe Int64
getValueAsInt64 key statsList = do
  value <- getValue key statsList
  return (read value)

getValueAsInt :: String -> StatsList -> Maybe Int
getValueAsInt key statsList = do
  value <- getValue key statsList
  return (read value)
