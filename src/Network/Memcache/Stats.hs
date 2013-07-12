
module Network.Memcache.Stats (
    getValue
  , getPid
  , getUptime
  , getTime
  , getVersion
  , getPointerSize
  , getRusageUser
  , getRusageSystem
  , getCurrItems
  , getTotalItems
  , getBytes
  , getCurrConnections
  , getTotalConnections
  , getConnectionStructures
  , getCmdGet
  , getCmdSet
  , getGetHits
  , getGetMisses
  , getEvictions
  , getBytesRead
  , getBytesWritten
  , getLimitMaxbytes
  , getThreads
  ) where

import Data.Int
import Data.Word
import Data.List.Split

import Network.Memcache.Client

{-|
  get any stats value as a string
-}
getValue :: String -> StatsList -> Maybe String
getValue key statsList = lookup key statsList

{-|
  pid (32u)
-}
getPid :: StatsList -> Maybe Word32
getPid = getValueAs "pid"

{-|
  uptime (32u)
-}
getUptime :: StatsList -> Maybe Word32
getUptime = getValueAs "uptime"

{-|
  time (32u)
  unix time
-}
getTime :: StatsList -> Maybe Word32
getTime = getValueAs "time"

{-|
  version (string)
-}
getVersion :: StatsList -> Maybe String
getVersion = getValue "version"

{-|
  pointer_size (32)
-}
getPointerSize :: StatsList -> Maybe Int32
getPointerSize = getValueAs "pointer_size"

{-|
  rusage_user (32u.32u)
-}
getRusageUser :: StatsList -> Maybe (Word32, Word32)
getRusageUser sl = do
  ru <- getValue "rusage_user" sl
  case splitOn "." ru of
    (second:microsec:[]) -> return (read second, read microsec)
    _ -> fail "invalid rusage_user"

{-|
  rusage_system (32u.32u)
-}
getRusageSystem :: StatsList -> Maybe (Word32, Word32)
getRusageSystem sl = do
  ru <- getValue "rusage_system" sl
  case splitOn "." ru of
    (second:microsec:[]) -> return (read second, read microsec)
    _ -> fail "invalid rusage_system"

{-|
  curr_items (32u)
-}
getCurrItems :: StatsList -> Maybe Word64
getCurrItems = getValueAs "curr_items"

{-|
  total_items (32u)
-}
getTotalItems :: StatsList -> Maybe Word64
getTotalItems = getValueAs "total_items"

{-|
  bytes (64u)
-}
getBytes :: StatsList -> Maybe Word64
getBytes = getValueAs "bytes"

{-|
  curr_connections (32u)
-}
getCurrConnections :: StatsList -> Maybe Word32
getCurrConnections = getValueAs "curr_connections"

{-|
  total_connections (32u)
-}
getTotalConnections :: StatsList -> Maybe Word32
getTotalConnections = getValueAs "total_connections"

{-|
  connection_structures (32u)
-}
getConnectionStructures :: StatsList -> Maybe Word32
getConnectionStructures = getValueAs "connection_structures"

-- reserved_fds

{-|
  cmd_get (64u)
-}
getCmdGet :: StatsList -> Maybe Word64
getCmdGet = getValueAs "cmd_get"

{-|
  cmd_set (64u)
-}
getCmdSet :: StatsList -> Maybe Word64
getCmdSet = getValueAs "cmd_set"

{-|
  get_hits (64u)
-}
getGetHits :: StatsList -> Maybe Word64
getGetHits = getValueAs "get_hits"

{-|
  get_misses (64u)
-}
getGetMisses :: StatsList -> Maybe Word64
getGetMisses = getValueAs "get_misses"

{-|
  evictions (64u)
-}
getEvictions :: StatsList -> Maybe Word64
getEvictions = getValueAs "evictions"

{-|
  bytes_read (64u)
-}
getBytesRead :: StatsList -> Maybe Word64
getBytesRead = getValueAs "bytes_read"

{-|
  bytes_written (64u)
-}
getBytesWritten :: StatsList -> Maybe Word64
getBytesWritten = getValueAs "bytes_written"

{-|
  limit_maxbytes (32u)
-}
getLimitMaxbytes :: StatsList -> Maybe Word64
getLimitMaxbytes = getValueAs "limit_maxbytes"

{-|
  threads (32u)
-}
getThreads :: StatsList -> Maybe Word32
getThreads = getValueAs "threads"


----------------------------------------------------------------

getValueAs :: (Read a) => String -> StatsList -> Maybe a
getValueAs key statsList = do
  value <- getValue key statsList
  return (read value)
