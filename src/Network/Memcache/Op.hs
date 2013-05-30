{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing #-}

-- authors: Kiyoshi Ikehara <kiyoshi.ikehara@gree.net>

-- see https://github.com/memcached/memcached/blob/master/doc/protocol.txt
--  storage - "set", "add", "replace", "append" or "prepend"
--  retrieval - "get" and "gets"
--  deletion - "delete"
--  increment decrement - "incr" and "decr"
--  touch - "touch"
--  stats
--  other commands - "flush_all", "version", "quit"

module Network.Memcache.Op (
    Option(..)
  , Op(PingOp
    , SetOp
    , CasOp
    , AddOp
    , ReplaceOp
    , AppendOp
    , PrependOp
    , GetOp
    , GetsOp
    , DeleteOp
    , IncrOp
    , DecrOp
    , TouchOp
    , FlushAllOp
    , VersionOp
    , QuitOp
    , StatsOp)
  , isWriteOp
  , isReadOp
  , isNoreplyOp
  , isStorageOp
  , toChunks
  , toOption
  , toOptions
  , keyOf
  , bytesOf
  , parseOpHeader
  , updateOpValue
  ) where

import qualified Data.ByteString.Char8 as C
import Data.Word
import Data.List
import Data.Maybe

type ValueT = C.ByteString
type BytesT = Word64
data Option = Noreply deriving (Eq)

instance Show Option where
  show Noreply = "noreply"

instance Read Option where
  readsPrec _d r = case r of
    "noreply" -> [(Noreply, "")]
    _ -> error "no parse"


data Op = 
  -- storage commands
    SetOp {
      key     :: C.ByteString
    , flags   :: Word16
    , exptime :: Word64
    , bytes   :: BytesT
    , value   :: ValueT
    , options :: [Option] }
  | CasOp {
      key     :: C.ByteString
    , flags   :: Word16
    , exptime :: Word64
    , bytes   :: BytesT
    , version :: Word64
    , value   :: ValueT
    , options :: [Option] }
  | AddOp {
      key     :: C.ByteString
    , flags   :: Word16
    , exptime :: Word64
    , bytes   :: BytesT
    , value   :: ValueT
    , options :: [Option] }
  | ReplaceOp {
      key     :: C.ByteString
    , flags   :: Word16
    , exptime :: Word64
    , bytes   :: BytesT
    , value   :: ValueT
    , options :: [Option] }
  | AppendOp {
      key     :: C.ByteString
    , flags   :: Word16
    , exptime :: Word64
    , bytes   :: BytesT
    , value   :: ValueT
    , options :: [Option] }
  | PrependOp {
      key     :: C.ByteString
    , flags   :: Word16
    , exptime :: Word64
    , bytes   :: BytesT
    , value   :: ValueT
    , options :: [Option] }
  -- retrieval commands
  | GetOp { keys :: [C.ByteString] }
  | GetsOp { keys :: [C.ByteString] }
  -- deletion commands
  | DeleteOp { key :: C.ByteString, options :: [Option] }
  -- increment and decrement commands
  | IncrOp { key :: C.ByteString, value' :: Word64, options :: [Option] }
  | DecrOp { key :: C.ByteString, value' :: Word64, options :: [Option] }
  -- touch commands
  | TouchOp { key :: C.ByteString, exptime :: Word64, options :: [Option] }
  -- stats commands
  -- other commands
  | PingOp
  | FlushAllOp
  | VersionOp
  | QuitOp
  | StatsOp { args :: [String] }
 deriving (Show, Read, Eq)

toOptions :: [C.ByteString] -> Maybe [Option]
toOptions opts = if elem Nothing converted
                 then Nothing
                 else Just $ concat $ map maybeToList converted
  where
    converted = map toOption opts

toOption :: C.ByteString -> Maybe Option
toOption option = case option of
  "noreply" -> Just Noreply
  _ -> Nothing

updateOpValue :: Op -> ValueT -> Op
updateOpValue op val
  | isStorageOp op = op { value = val }
  | otherwise = op

bytesOf :: Op -> Maybe BytesT
bytesOf op
  | isStorageOp op = Just $ bytes op
  | otherwise = Nothing

keyOf :: Op -> Maybe C.ByteString
keyOf op = case op of
  PingOp -> Nothing
  FlushAllOp -> Nothing
  VersionOp -> Nothing
  QuitOp -> Nothing
  StatsOp _ -> Nothing
  GetOp [] -> Nothing
  GetOp (k:_) -> Just k
  GetsOp [] -> Nothing
  GetsOp (k:_) -> Just k
  _ -> Just $ key op

isWriteOp :: Op -> Bool
isWriteOp op = case op of
  SetOp {} -> True
  CasOp {} -> True
  AddOp {} -> True
  ReplaceOp {} -> True
  AppendOp {} -> True
  PrependOp {} -> True
  DeleteOp {} -> True
  IncrOp {} -> True
  DecrOp {} -> True
  TouchOp {} -> True
  _ -> False

isStorageOp :: Op -> Bool
isStorageOp op = case op of
  SetOp {} -> True
  CasOp {} -> True
  AddOp {} -> True
  ReplaceOp {} -> True
  AppendOp {} -> True
  PrependOp {} -> True
  _ -> False

isReadOp :: Op -> Bool
isReadOp op = case op of
  GetOp {} -> True
  GetsOp {} -> True
  _ -> False

isNoreplyOp :: Op -> Bool
isNoreplyOp op = case op of
  SetOp { options = os }     -> elem Noreply os
  CasOp { options = os }     -> elem Noreply os
  AddOp { options = os }     -> elem Noreply os
  ReplaceOp { options = os } -> elem Noreply os
  AppendOp { options = os }  -> elem Noreply os
  PrependOp { options = os } -> elem Noreply os
  DeleteOp { options = os }  -> elem Noreply os
  IncrOp { options = os }    -> elem Noreply os
  DecrOp { options = os }    -> elem Noreply os
  TouchOp { options = os }   -> elem Noreply os
  _ -> False

toChunks :: Op -> [C.ByteString]
toChunks op = case op of
  PingOp -> ["ping", ln]
  -- storage commands
  SetOp key flags exptime bytes value options -> setop "set" key flags exptime value options
  CasOp key flags exptime bytes version value options ->
    [C.concat ["cas ", key, " ", show' flags, " ", show' exptime, " ", showlen value,
               " ", show' version, showopt options, ln], value, ln]
  AddOp key flags exptime bytes value options -> setop "add" key flags exptime value options
  ReplaceOp key flags exptime bytes value options -> setop "replace" key flags exptime value options
  AppendOp key flags exptime bytes value options -> setop "append" key flags exptime value options
  PrependOp key flags exptime bytes value options -> setop "prepend" key flags exptime value options
  -- retrieval commands
  GetOp keys -> [C.concat ["get ", C.intercalate " " keys, ln]]
  GetsOp keys -> [C.concat ["gets ", C.intercalate " " keys, ln]]
  -- deletion commands
  DeleteOp key options -> [C.concat ["delete ", key, showopt options, ln]]
  -- increment and decrement commands
  IncrOp key value' options -> incrdecrop "incr" key value' options
  DecrOp key value' options -> incrdecrop "decr" key value' options
  -- touch commands
  TouchOp key exptime options -> [C.concat ["touch ", key, " ", show' exptime, showopt options, ln]]
  -- stats commands
  -- other commands
  FlushAllOp -> ["flush_all", ln]
  VersionOp -> ["version", ln]
  QuitOp -> ["quit", ln]
  StatsOp args -> case args of
    [] -> ["stats", ln]
    _ -> ["stats ", C.pack $ intercalate " " args, ln]
  where
    ln = C.pack "\r\n"
    show' a = C.pack $ show a
    showlen a = show' $ C.length a
    showopt [] = ""
    showopt os = C.concat [" ", C.intercalate " " (map (C.pack . show) os)]
    setop cmd key flags exptime value options =
      [C.concat [cmd, " ", key, " ", show' flags, " ", show' exptime, " ", showlen value, showopt options, ln], value, ln]
    incrdecrop cmd key value' options =
      [C.concat [cmd, " ", key, " ", show' value', showopt options, ln]]

-- parse op header
parseOpHeader :: C.ByteString -> Maybe Op
parseOpHeader headerLine = 
  case filter (\x -> not (C.null x)) (C.split ' ' headerLine) of
    [] -> Nothing
    (cmd:args) -> parseOpHeader' cmd args

parseOpHeader' :: C.ByteString -> [C.ByteString] -> Maybe Op
parseOpHeader' cmd args
  | cmd == "get"       = op_get args
  | cmd == "gets"      = op_gets args
  | cmd == "set"       = op_set' SetOp args
  | cmd == "add"       = op_set' AddOp args
  | cmd == "replace"   = op_set' ReplaceOp args
  | cmd == "append"    = op_set' AppendOp args
  | cmd == "prepend"   = op_set' PrependOp args
  | cmd == "cas"       = op_cas args
  | cmd == "incr"      = op_incr args
  | cmd == "decr"      = op_decr args
  | cmd == "delete"    = op_delete args
  | cmd == "touch"     = op_touch args
  | cmd == "flush_all" = op_flush_all args
  | cmd == "version"   = op_version args
  | cmd == "quit"      = op_quit args
  | cmd == "ping"      = op_ping args
  | cmd == "stats"     = op_stats args
  | otherwise = Nothing
  where
    -- get <key1> <key2> ...
    op_get args
      | null args = Nothing
      | otherwise = Just $ GetOp args

    -- gets <key1> <key2> ...
    op_gets args
      | null args = Nothing
      | otherwise = Just $ GetsOp args

    -- set <key> <flags> <exptime> <size> [<options>] -> STORED
    op_set' op (key:args) = case parseStorageArgs args of
      Just (flag, expire, size, options) -> Just $ op key flag expire size "" options
      Nothing -> Nothing
    op_set' _ _ = Nothing

    parseStorageArgs :: [C.ByteString] -> Maybe (Word16, Word64, BytesT, [Option])
    parseStorageArgs (flag:expire:size:options) = do
      flag' <- readWord flag
      size' <- readWord64 size
      expire' <- readWord64 expire
      options' <- toOptions options
      return $ (fromIntegral flag', expire', size', options')
    parseStorageArgs _ = Nothing

    -- cas <key> <flags> <exptime> <size> <version> [<option>] -> STORED
    op_cas (key:flag:expire:size:version:options) =
      case parseStorageArgs (flag:expire:size:options) of
        Just (flag', expire', size', options') -> case readWord64 version of
          Just version' -> Just $ CasOp key flag' expire' size' version' "" options'
          Nothing -> Nothing
        Nothing-> Nothing
    op_cas _ = Nothing

    -- incr <key> <value> <options>
    op_incr = op_incr' IncrOp

    op_incr' opConstructor (key:value:options) = do
      case readWord64 value of
        Just value' ->
          case toOptions options of
            Just os -> Just $ opConstructor key value' os
            Nothing -> Nothing
        Nothing -> Nothing
    op_incr' _ _ = Nothing

    -- decr <key> <value> <options>
    op_decr = op_incr' DecrOp

    -- delete <key>
    op_delete (key:options) = case toOptions options of
      Just os -> if C.null key then Nothing else Just $ DeleteOp key os
      Nothing -> Nothing
    op_delete _ = Nothing

    -- touch <key> <expire> <options>
    op_touch (key:expire:options) = do
      case readWord64 expire of
        Just expire' ->
          case toOptions options of
            Just options' -> if C.null key then Nothing else Just $ TouchOp key expire' options'
            Nothing -> Nothing
        Nothing -> Nothing
    op_touch _ = Nothing

    -- flush_all
    op_flush_all [] = Just $ FlushAllOp
    op_flush_all _  = Nothing

    -- version
    op_version [] = Just $ VersionOp
    op_version _  = Nothing

    -- quit
    op_quit [] = Just $ QuitOp
    op_quit _  = Nothing

    -- ping
    op_ping [] = Just $ PingOp
    op_ping _ = Nothing

    -- stats
    op_stats args = Just $ StatsOp (map C.unpack args)

readInt :: C.ByteString -> Maybe Int
readInt x = case C.readInt x of
  Just (v, rest) -> if C.null rest then Just v else Nothing
  Nothing -> Nothing

readWord :: C.ByteString -> Maybe Word
readWord x = case C.readInteger x of
  Just (v, rest) -> if C.null rest then Just (fromIntegral v) else Nothing
  Nothing -> Nothing

readWord64 :: C.ByteString -> Maybe Word64
readWord64 x = case C.readInteger x of
  Just (v, rest) -> if C.null rest then Just (fromIntegral v) else Nothing
  Nothing -> Nothing
