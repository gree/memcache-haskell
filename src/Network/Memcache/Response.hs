-- authors: Kiyoshi Ikehara <kiyoshi.ikehara@gree.net>

{-# LANGUAGE OverloadedStrings #-}

module Network.Memcache.Response (
    Response (
        Ok
      , Value
      , End
      , Stored
      , NotStored
      , Exists
      , NotFound
      , Deleted
      , Found
      , Touched
      , Error
      , ServerError
      , ClientError
      , Version
      , Code
      , Stat)
  , parseResponseHeader
  , responseParser
  , toChunks) where

import Prelude hiding (takeWhile, take)
import qualified Data.ByteString.Char8 as BS
import Data.Word
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Lazy as AL
import Control.Applicative
import Data.Char


data Response =
    Ok
  | Value {
      _resKey     :: BS.ByteString
    , _resFlag    :: Word32
    , _resLen     :: Word64
    , _resValue   :: BS.ByteString
    , _resVersion :: Maybe Word64 -- Value key flag value
    }
  | End
  | Stored
  | NotStored
  | Exists
  | NotFound
  | Deleted
  | Found
  | Touched
  | Error
  | ServerError String
  | ClientError String
  | Version BS.ByteString
  | Stat BS.ByteString BS.ByteString -- name and value pair
  | Code Word64
  deriving (Show, Eq)

-- parser by attoparsec

responseParser :: Parser Response
responseParser = do
  cmd <- skipWhile (== ' ') *> try (takeWhile (\c -> isAscii c && Data.Attoparsec.ByteString.Char8.isDigit c))
  case cmd of
    "VALUE"        -> do
        key     <- skipWhile (== ' ') *> AL.takeWhile (\c -> c /= 0x20 && not (isEndOfLine c)) 
        flags   <- skipWhile (== ' ') *> decimal
        len     <- skipWhile (== ' ') *> decimal
        version <- skipWhile (== ' ') *> (try (fmap Just decimal) <|> return (Nothing) <* skipWhile (== ' ')) <* endline
        value   <- take len <* endline
        return (Value key flags (fromIntegral len) value version)
    "END"          -> pure End
    "STORED"       -> pure Stored
    "NOT_STORED"   -> pure NotStored
    "EXISTS"       -> pure Exists
    "NOT_FOUND"    -> pure NotFound
    "DELETED"      -> pure Deleted
    "OK"           -> pure Ok
    "FOUND"        -> pure Found
    "TOUCHED"      -> pure Touched
    "ERROR"        -> pure Error
    "SERVER_ERROR" -> ServerError <$> (skipWhile (== ' ') *> fmap BS.unpack (AL.takeTill isEndOfLine))
    "CLIENT_ERROR" -> ClientError <$> (skipWhile (== ' ') *> fmap BS.unpack (AL.takeTill isEndOfLine))
    "STAT"         -> Stat
                      <$> (skipWhile (== ' ') *> takeWhile (/= ' '))
                      <*> AL.takeTill isEndOfLine
    "VERSION"      -> Version <$> (skipWhile (== ' ') *> AL.takeTill isEndOfLine)
    _              -> Code <$> decimal
  where
    endline :: Parser BS.ByteString
    endline = try (string "\r\n") <|> string "\n" <|> string "\r"

-- parse response header
parseResponseHeader :: BS.ByteString -> Maybe Response
parseResponseHeader headerLine = 
  case filter (\x -> not (BS.null x)) (BS.split ' ' headerLine) of
    [] -> Nothing
    (code:args) -> parseResponseHeader' code args

parseResponseHeader' :: BS.ByteString -> [BS.ByteString] -> Maybe Response
parseResponseHeader' code args = case code of
  "VALUE"        -> case parseValueArgs args of
    Just (key, flags, len, version) -> Just $ Value key flags len (BS.pack "") version
    Nothing -> Nothing
  "END"          -> Just End
  "STORED"       -> Just Stored
  "NOT_STORED"   -> Just NotStored
  "EXISTS"       -> Just Exists
  "NOT_FOUND"    -> Just NotFound
  "DELETED"      -> Just Deleted
  "OK"           -> Just Ok
  "FOUND"        -> Just Found
  "TOUCHED"      -> Just Touched
  "ERROR"        -> Just Error
  "SERVER_ERROR" -> Just $ ServerError $ BS.unpack $ BS.intercalate " " args
  "CLIENT_ERROR" -> Just $ ClientError $ BS.unpack $ BS.intercalate " " args
  "STAT"         -> case args of
    (statName:statValue) -> Just $ Stat (statName) (BS.intercalate " " statValue)
    _ -> Nothing
  "VERSION"      -> case args of
    (version:[]) -> Just $ Version (version)
    _ -> Nothing
  _ -> case readNum code of
    Just c -> Just $ Code c
    Nothing -> Nothing


parseValueArgs :: [BS.ByteString] -> Maybe (BS.ByteString, Word32, Word64, Maybe Word64)
parseValueArgs (key:f:l:rest) = do
  flags <- readNum f
  len <- readNum l
  case rest of
    []     -> return (key, flags, len, Nothing) 
    (v:[]) -> do
      version <- readNum v
      return (key, flags, len, Just version) 
    _      -> Nothing
parseValueArgs _ = Nothing

readNum :: (Num a) => BS.ByteString -> Maybe a
readNum x = case BS.readInteger x of
  Just (v, rest) -> if BS.null rest then Just $ fromInteger v else Nothing
  Nothing -> Nothing

toChunks :: Response -> [BS.ByteString]
toChunks result = case result of
  Ok        -> ["OK", ln]
  Value key flag len value version ->
    let header = BS.intercalate " " ["VALUE", key, show' flag, show' len] in
      case version of
      Nothing -> [header, ln, value, ln]
      Just version' -> [header, " ", show' version', ln, value, ln]
  End       -> ["END", ln]
  Stored    -> ["STORED", ln]
  NotStored -> ["NOT_STORED", ln]
  Exists    -> ["EXISTS", ln]
  NotFound  -> ["NOT_FOUND", ln]
  Deleted   -> ["DELETED", ln]
  Found     -> ["FOUND", ln]
  Touched   -> ["TOUCHED", ln]
  Error     -> ["ERROR", ln]
  ServerError msg -> [concatMsg "SERVER_ERROR" msg, ln]
  ClientError msg -> [concatMsg "CLIENT_ERROR" msg, ln]
  Version version -> [BS.intercalate " " ["VERSION", version], ln]
  Code code -> [BS.pack $ show code, ln]
  Stat name value -> ["STAT", " ", name, " ", value, ln]
  where
    show' :: (Show a) => a -> BS.ByteString
    show' = BS.pack . show

ln :: BS.ByteString
ln = BS.pack "\r\n"

concatMsg :: BS.ByteString -> String -> BS.ByteString
concatMsg code msg = if null msg then code else BS.intercalate " " [code, BS.pack msg]
