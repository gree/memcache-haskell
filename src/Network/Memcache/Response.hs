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
  , parseResponse
  , parseResponseHeader
  , responseParser
  , responseHeaderParser
  , toChunks) where

import Prelude hiding (takeWhile, take)
import qualified Data.ByteString.Char8 as BS
import Data.Word
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Lazy as AL
import Control.Applicative
import Data.Char

import Debug.Trace

{-|
  response messages from memcached server
-}
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

{-|
  response parser by attoparsec
-}
responseParser :: Parser Response
responseParser = responseParser' False

{-|
  response header parser by attoparsec
-}
responseHeaderParser :: Parser Response
responseHeaderParser = responseParser' True

responseParser' :: Bool -> Parser Response
responseParser' onlyHeader = try commandParser <|> codeParser
  where
    commandParser :: Parser Response
    commandParser = do
      cmd <- skipWhile (== ' ') *> takeWhile (\c -> isAlphaNum c || c == '_')
      resp <- case {- trace ("cmd: " ++ show cmd) -} cmd of
        "VALUE"        -> do
          key     <- skipWhile (== ' ') *> takeWhile (\c -> c /= ' ')
          flags   <- skipWhile (== ' ') *> decimal
          len     <- skipWhile (== ' ') *> decimal
          version <- skipWhile (== ' ') *> try (fmap Just decimal) <|> return (Nothing) <* skipWhile (== ' ')
          if onlyHeader
            then do
              return (Value key flags (fromIntegral len) "" version)
            else do
              value <- endline *> take len
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
        _              -> fail $ "unknown command " ++ BS.unpack cmd
      _ <- endline
      return (resp)

    codeParser :: Parser Response
    codeParser = Code <$> (skipWhile (== ' ') *> decimal <* skipWhile (== ' ') <* endline)
    
    endline :: Parser BS.ByteString
    endline = try (string "\r\n") <|> string "\n" <|> string "\r"

{-|
  parse a response
-}
parseResponse :: BS.ByteString -> Maybe Response
parseResponse = parseResponse' False

{-|
  parse a response but only its header
-}
parseResponseHeader :: BS.ByteString -> Maybe Response
parseResponseHeader = parseResponse' True

parseResponse' :: Bool -> BS.ByteString -> Maybe Response
parseResponse' onlyHeader input = let r = parse (responseParser' onlyHeader) input in case r of
  Fail {} -> Nothing
  Partial parse' -> let r' = parse' "\r\n" in case r' of
    Done _ result -> Just result
    Fail {} -> Nothing
    Partial {} -> Nothing
--      let r'' = feed r' "" in case trace (show r'') r'' of
--        Done _ result -> Just result
--        _ -> Nothing
  Done _ result -> Just result

{-|
  convert a response to bytestring chunks
-}
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