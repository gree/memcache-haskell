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
  , toChunks) where

import qualified Data.ByteString.Char8 as C
import Data.Word

data Response = Ok
  | Value {
      resKey     :: C.ByteString
    , resFlag    :: Word16
    , resLen     :: Word64
    , resValue   :: C.ByteString
    , resVersion :: (Maybe Word64) -- Value key flag value
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
  | Version String
  | Stat String String -- name and value pair
  | Code Word64
  deriving (Show, Eq)

-- parse response header
parseResponseHeader :: C.ByteString -> Maybe Response
parseResponseHeader headerLine = 
  case filter (\x -> not (C.null x)) (C.split ' ' headerLine) of
    [] -> Nothing
    (code:args) -> parseResponseHeader' code args

parseResponseHeader' :: C.ByteString -> [C.ByteString] -> Maybe Response
parseResponseHeader' code args = case code of
  "VALUE"        -> case parseValueArgs args of
    Just (key, flags, len, version) -> Just $ Value key flags len (C.pack "") version
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
  "SERVER_ERROR" -> Just $ ServerError $ C.unpack $ C.intercalate " " args
  "CLIENT_ERROR" -> Just $ ClientError $ C.unpack $ C.intercalate " " args
  "STAT"         -> case args of
    (statName:statValue) -> Just $ Stat (C.unpack statName) (C.unpack $ C.intercalate " " statValue)
    _ -> Nothing
  "VERSION"      -> case args of
    (version:[]) -> Just $ Version (C.unpack version)
    _ -> Nothing
  _ -> case readNum code of
    Just c -> Just $ Code c
    Nothing -> Nothing

parseValueArgs :: [C.ByteString] -> Maybe (C.ByteString, Word16, Word64, Maybe Word64)
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

readNum :: (Num a) => C.ByteString -> Maybe a
readNum x = case C.readInteger x of
  Just (v, rest) -> if C.null rest then Just $ fromInteger v else Nothing
  Nothing -> Nothing

toChunks :: Response -> [C.ByteString]
toChunks result = case result of
  Ok        -> ["OK", ln]
  Value key flag len value version ->
    let header = C.intercalate " " ["VALUE", key, showf flag, showlen value] in
      case version of
      Nothing -> [header, ln, value, ln]
      Just version' -> [header, " ", showv version', ln, value, ln]
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
  Version version -> [C.intercalate " " ["VERSION", C.pack version], ln]
  Code code -> [C.pack $ show code, ln]
  Stat name value -> ["STAT", " ", C.pack name, " ", C.pack value, ln]

ln :: C.ByteString
ln = C.pack "\r\n"

showlen :: C.ByteString -> C.ByteString
showlen value = C.pack (show $ C.length value)

showf :: Word16 -> C.ByteString
showf = C.pack . show

showv :: Word64 -> C.ByteString
showv = C.pack . show

concatMsg :: C.ByteString -> String -> C.ByteString
concatMsg code msg = if null msg then code else C.intercalate " " [code, C.pack msg]
