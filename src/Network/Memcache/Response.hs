-- authors: Kiyoshi Ikehara <kiyoshi.ikehara@gree.net>

{-# LANGUAGE OverloadedStrings #-}

{-|
  This module represents memcached response messages.
-}
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
  ) where

import Prelude hiding (takeWhile, take)
import qualified Data.ByteString.Char8 as BS
import Data.Word
import qualified Data.Attoparsec.ByteString as AB
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Lazy as AL
import Control.Applicative
import Control.Monad.IO.Class

-- import Debug.Trace

import Network.Memcache.Class
import Network.Memcache.IO

instance Message Response where
  parseHeader = parseResponseHeader

  toChunks = Network.Memcache.Response.toChunks

  recvContent handle resp = case resp of
    Value key flags len _ version -> liftIO $ do
      value <- readBytes handle len
      _term <- BS.hGetLine handle
      return $ Just $ Value key flags len value version
    _ -> return $ Just resp

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
    , _resVersion :: Maybe Word64
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
responseParser' onlyHeader = try parser <|> codeParser
  where
    parser :: Parser Response
    parser = do
      cmd <- ws *> word <* ws
      resp <- case {- trace ("cmd: " ++ show cmd) -} cmd of
        "VALUE"        -> do
          key     <- skipWhile (== ' ') *> takeWhile1 (\c -> c /= ' ')
          flags   <- skipWhile (== ' ') *> decimal
          len     <- skipWhile (== ' ') *> decimal :: Parser Word64
          version <- skipWhile (== ' ') *> try (Just <$> decimal) <|> return (Nothing) <* skipWhile (== ' ')
          if onlyHeader
            then do
              return (Value key flags len "" version)
            else do
              value <- endline *> take (fromIntegral len) -- XXX
              return (Value key flags len value version)
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
                          <$> (skipWhile (== ' ') *> takeWhile1 (/= ' '))
                          <*> (skipWhile (== ' ') *> AL.takeTill isEndOfLine)
        "VERSION"      -> Version <$> (skipWhile (== ' ') *> AL.takeTill isEndOfLine)
        _              -> fail $ "unknown response " ++ BS.unpack cmd
      _ <- endline
      return (resp)

    word = AB.takeWhile1 (\c -> c /= 32 && c /= 10 && c /= 13)

    ws = AB.skipWhile (== 32)

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
  Done _ result -> Just result

{-|
  convert a response to bytestring chunks
-}
toChunks :: Response -> [BS.ByteString]
toChunks result = case result of
  Ok        -> ["OK\r\n"]
  Value key flag len value version ->
    let header = BS.intercalate " " ["VALUE", key, show' flag, show' len] in
      case version of
      Nothing -> [header, "\r\n", value, "\r\n"]
      Just version' -> [header, " ", show' version', "\r\n", value, "\r\n"]
  End       -> ["END\r\n"]
  Stored    -> ["STORED\r\n"]
  NotStored -> ["NOT_STORED\r\n"]
  Exists    -> ["EXISTS\r\n"]
  NotFound  -> ["NOT_FOUND\r\n"]
  Deleted   -> ["DELETED\r\n"]
  Found     -> ["FOUND\r\n"]
  Touched   -> ["TOUCHED\r\n"]
  Error     -> ["ERROR\r\n"]
  ServerError msg -> [BS.concat ["SERVER_ERROR", BS.pack $ if null msg then "" else " " ++ msg, "\r\n"]]
  ClientError msg -> [BS.concat ["CLIENT_ERROR", BS.pack $ if null msg then "" else " " ++ msg, "\r\n"]]
  Version version -> [BS.concat $ [BS.intercalate " " ["VERSION", version], "\r\n"]]
  Code code -> [BS.pack $ show code ++ "\r\n"]
  Stat name value -> [BS.concat ["STAT", " ", name, " ", value, "\r\n"]]
  where
    show' :: (Show a) => a -> BS.ByteString
    show' = BS.pack . show
