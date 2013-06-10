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
  , toOption
  , toOptions
  , keyOf
  , bytesOf
  , parseOp
  , parseOpHeader
  , opParser
  , opHeaderParser
  , updateOpValue
  ) where

import Prelude hiding (take, takeWhile)
import qualified Data.ByteString.Char8 as BS
import Data.Word
import Data.Char
import Data.Maybe
import Data.Attoparsec.ByteString.Char8
import Control.Applicative

import Debug.Trace

import Network.Memcache.Class

type ValueT = BS.ByteString
type BytesT = Word64
data Option = Noreply deriving (Eq)

instance Show Option where
  show Noreply = "noreply"

instance Read Option where
  readsPrec _d r = case r of
    "noreply" -> [(Noreply, "")]
    _ -> error "no parse"

instance Message Network.Memcache.Op.Op where
  parseHeader = parseOpHeader

  toChunks = Network.Memcache.Op.toChunks

  recv handle = do
    l <- BS.hGetLine handle
    case parseHeader $ chompLine l of
      Just op -> recvContent handle op
      Nothing -> return Nothing

  recvContent handle op
    | isStorageOp op = case bytesOf op of
        Just bytes -> do
          content <- readBytes handle bytes
          _term <- BS.hGetLine handle
          return $ Just $ updateOpValue op content
        Nothing -> return $ Just op
    | otherwise = do
        return $ Just op

data Op = 
  -- storage commands
    SetOp {
      key     :: BS.ByteString
    , flags   :: Word32
    , exptime :: Word64
    , bytes   :: BytesT
    , value   :: ValueT
    , options :: [Option] }
  | CasOp {
      key     :: BS.ByteString
    , flags   :: Word32
    , exptime :: Word64
    , bytes   :: BytesT
    , version :: Word64
    , value   :: ValueT
    , options :: [Option] }
  | AddOp {
      key     :: BS.ByteString
    , flags   :: Word32
    , exptime :: Word64
    , bytes   :: BytesT
    , value   :: ValueT
    , options :: [Option] }
  | ReplaceOp {
      key     :: BS.ByteString
    , flags   :: Word32
    , exptime :: Word64
    , bytes   :: BytesT
    , value   :: ValueT
    , options :: [Option] }
  | AppendOp {
      key     :: BS.ByteString
    , flags   :: Word32
    , exptime :: Word64
    , bytes   :: BytesT
    , value   :: ValueT
    , options :: [Option] }
  | PrependOp {
      key     :: BS.ByteString
    , flags   :: Word32
    , exptime :: Word64
    , bytes   :: BytesT
    , value   :: ValueT
    , options :: [Option] }
  -- retrieval commands
  | GetOp { keys :: [BS.ByteString] }
  | GetsOp { keys :: [BS.ByteString] }
  -- deletion commands
  | DeleteOp { key :: BS.ByteString, options :: [Option] }
  -- increment and decrement commands
  | IncrOp { key :: BS.ByteString, value' :: Word64, options :: [Option] }
  | DecrOp { key :: BS.ByteString, value' :: Word64, options :: [Option] }
  -- touch commands
  | TouchOp { key :: BS.ByteString, exptime :: Word64, options :: [Option] }
  -- stats commands
  -- other commands
  | PingOp
  | FlushAllOp
  | VersionOp
  | QuitOp
  | StatsOp { args :: [BS.ByteString] }
 deriving (Show, Read, Eq)

toOptions :: [BS.ByteString] -> Maybe [Option]
toOptions opts = if elem Nothing converted
                 then Nothing
                 else Just $ concat $ map maybeToList converted
  where
    converted = map toOption opts

toOption :: BS.ByteString -> Maybe Option
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

keyOf :: Op -> Maybe BS.ByteString
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


-- parse op header
{-|
  parse an operation
-}
parseOp :: BS.ByteString -> Maybe Op
parseOp = parseOp' False

{-|
  parse an operation but only its header
-}
parseOpHeader :: BS.ByteString -> Maybe Op
parseOpHeader = parseOp' True

parseOp' :: Bool -> BS.ByteString -> Maybe Op
parseOp' onlyHeader input = let r = parse (opParser' onlyHeader) input in case r of
  Fail {} -> Nothing
  Partial parse' -> let r' = parse' "\r\n" in case r' of
    Done _ result -> Just result
    Fail {} -> Nothing
    Partial {} -> Nothing
  Done _ result -> Just result

opParser :: Parser Op
opParser = opParser' False

opHeaderParser :: Parser Op
opHeaderParser = opParser' True

opParser' :: Bool -> Parser Op
opParser' onlyHeader = parser
  where
    parser :: Parser Op
    parser = do
      cmd <- ws *> takeWhile1 (\c -> isAlphaNum c || c == '_') <* ws
      case cmd of
        "get"       -> GetOp <$> (keys <* endline)
        "gets"      -> GetsOp <$> (keys <* endline)
        "set"       -> op_set' SetOp
        "add"       -> op_set' AddOp
        "replace"   -> op_set' ReplaceOp
        "append"    -> op_set' AppendOp
        "prepend"   -> op_set' PrependOp
        "cas"       -> op_cas
        "incr"      -> IncrOp   <$> (key <* ws) <*> (decimal <* ws) <*> (options <* endline)
        "decr"      -> DecrOp   <$> (key <* ws) <*> (decimal <* ws) <*> (options <* endline)
        "delete"    -> DeleteOp <$> (key <* ws) <*> (options <* endline)
        "touch"     -> TouchOp  <$> (key <* ws) <*> (decimal <* ws) <*> (options <* endline)
        "flush_all" -> pure FlushAllOp <* endline
        "version"   -> pure VersionOp <* endline
        "quit"      -> pure QuitOp <* endline
        "ping"      -> pure PingOp <* endline
        "stats"     -> StatsOp <$> (words <* endline)
        _           -> fail ""

    keys = many1 (key <* ws)
    
    key = word

    words = many (word <* ws)
    
    word = takeWhile1 (\c -> c /= ' ' && c /= '\r' && c/= '\n')

    ws :: Parser ()
    ws = skipWhile (== ' ')

    endline :: Parser BS.ByteString
    endline = try (string "\r\n") <|> string "\n" <|> string "\r"

    options = do
      mopts <- toOptions <$> words
      case mopts of
        Just opts -> return (opts)
        Nothing -> fail "invalid options"

    -- set <key> <flags> <exptime> <size> [<options>] -> STORED
    op_set' op = do
      op'   <- op <$> (key <* ws) <*> (decimal <* ws) <*> (decimal <* ws)
      size  <- decimal <* ws :: Parser Word64
      opts  <- options <* endline
      value <- if onlyHeader then pure BS.empty else (take (fromIntegral size) <* ws <* endline)
      return (op' size value opts)

    -- cas <key> <flags> <exptime> <size> <version> [<option>] -> STORED
    op_cas = do
      op'   <- CasOp <$> (key <* ws) <*> (decimal <* ws) <*> (decimal <* ws)
      size  <- decimal <* ws :: Parser Word64
      ver   <- decimal <* ws
      opts  <- options <* endline
      value <- if onlyHeader then pure BS.empty else (take (fromIntegral size) <* ws <* endline)
      return (op' size ver value opts)


{-|
  convert a response to bytestring chunks
-}
toChunks :: Op -> [BS.ByteString]
toChunks op = case op of
  PingOp -> ["ping", ln]
  -- storage commands
  SetOp key flags exptime bytes value options -> setop "set" key flags exptime bytes value options
  CasOp key flags exptime bytes version value options ->
    [BS.concat ["cas ", key, " ", show' flags, " ", show' exptime, " ", show' bytes,
               " ", show' version, showopt options, ln], value, ln]
  AddOp key flags exptime bytes value options -> setop "add" key flags exptime bytes value options
  ReplaceOp key flags exptime bytes value options -> setop "replace" key flags exptime bytes value options
  AppendOp key flags exptime bytes value options -> setop "append" key flags exptime bytes value options
  PrependOp key flags exptime bytes value options -> setop "prepend" key flags exptime bytes value options
  -- retrieval commands
  GetOp keys -> [BS.concat ["get ", BS.intercalate " " keys, ln]]
  GetsOp keys -> [BS.concat ["gets ", BS.intercalate " " keys, ln]]
  -- deletion commands
  DeleteOp key options -> [BS.concat ["delete ", key, showopt options, ln]]
  -- increment and decrement commands
  IncrOp key value' options -> incrdecrop "incr" key value' options
  DecrOp key value' options -> incrdecrop "decr" key value' options
  -- touch commands
  TouchOp key exptime options -> [BS.concat ["touch ", key, " ", show' exptime, showopt options, ln]]
  -- stats commands
  -- other commands
  FlushAllOp -> ["flush_all", ln]
  VersionOp -> ["version", ln]
  QuitOp -> ["quit", ln]
  StatsOp args -> case args of
    [] -> ["stats", ln]
    _ -> ["stats ", BS.intercalate " " args, ln]
  where
    ln = BS.pack "\r\n"
    show' a = BS.pack $ show a
    showopt [] = ""
    showopt os = BS.concat [" ", BS.intercalate " " (map (BS.pack . show) os)]
    setop cmd key flags exptime len value options =
      [BS.concat [cmd, " ", key, " ", show' flags, " ", show' exptime, " ", show' len, showopt options, ln], value, ln]
    incrdecrop cmd key value' options =
      [BS.concat [cmd, " ", key, " ", show' value', showopt options, ln]]

