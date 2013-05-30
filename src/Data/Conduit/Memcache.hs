{-# LANGUAGE OverloadedStrings #-}

module Data.Conduit.Memcache (parseText, parseResponse, opText, responseText) where

import Control.Monad.Trans
-- import Control.Monad.IO.Class
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
-- import Data.Attoparsec.Binary
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString.Char8 as C
import Data.Conduit
import Data.Conduit.Attoparsec

import Network.Memcache.Op
import Network.Memcache.Response

parseText :: (MonadIO m, MonadThrow m) => ConduitM C.ByteString Op m ()
parseText = parseText' =$= removePR

parseText' :: (MonadIO m, MonadThrow m) => ConduitM C.ByteString (PositionRange, Op) m ()
parseText' = conduitParser command
  where
    command :: Parser Op
    command = do
      header <- skipSpace' *> AL.takeTill isEndOfLine <* endline
      case parseOpHeader header of
        Nothing -> return (QuitOp)
        Just op -> do
          if isStorageOp op
            then do
            case bytesOf op of
              Just bytes -> do
                content <- AL.take (fromIntegral bytes) <* endline
                return (updateOpValue op content)
              Nothing -> return (op)
            else do
            return (op)

removePR :: (MonadIO m, MonadThrow m) => ConduitM (PositionRange, t) t m ()
removePR = do
  mdata <- await
  case mdata of
    Nothing -> return ()
    Just (_, r) -> do
      yield r
      removePR

parseResponse :: (MonadIO m, MonadThrow m) => ConduitM C.ByteString Response m ()
parseResponse = parseResponse' =$= removePR

parseResponse' :: (MonadIO m, MonadThrow m) => ConduitM C.ByteString (PositionRange, Response) m ()
parseResponse' = conduitParser response
  where
    response :: Parser Response
    response = do
      header <- skipSpace' *> AL.takeTill isEndOfLine <* endline
      case parseResponseHeader header of
        Just (Value key flag len _value version) -> do
          value' <- AL.take (fromIntegral len) <* endline
          return (Value key flag len value' version)
        Just resp -> do
          return (resp)
        Nothing -> return (Error)
  

responseText :: MonadIO m => ConduitM Response C.ByteString m ()
responseText = do
  mResp <- await
  case mResp of
    Nothing -> return ()
    Just resp -> do
      mapM_ yield $ Network.Memcache.Response.toChunks resp
      responseText

opText :: MonadIO m => ConduitM Op C.ByteString m ()
opText = do
  mOp <- await
  case mOp of
    Nothing -> return ()
    Just op -> do
      mapM_ yield $ Network.Memcache.Op.toChunks op
      opText

----

endline :: Parser C.ByteString
endline = try (string "\r\n") <|> string "\n" <|> string "\r"

skipSpace' :: Parser ()
skipSpace' = skipWhile (== ' ')

