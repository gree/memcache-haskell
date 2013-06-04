{-# LANGUAGE OverloadedStrings #-}

module Data.Conduit.Memcache (getOpText, getResponseText, putOpText, putResponseText) where

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

getOpText :: (MonadIO m, MonadThrow m) => ConduitM C.ByteString (Either String Op) m ()
getOpText = getOpText' =$= removePR

getOpText' :: (MonadIO m, MonadThrow m) => ConduitM C.ByteString (PositionRange, Either String Op) m ()
getOpText' = conduitParser command
  where
    command :: Parser (Either String Op)
    command = do
      header <- skipSpace' *> AL.takeTill isEndOfLine <* endline
      case parseOpHeader header of
        Nothing -> return (Left "invalid command")
        Just op -> do
          if isStorageOp op
            then do
            case bytesOf op of
              Just bytes -> do
                content <- AL.take (fromIntegral bytes) <* endline
                return (Right $ updateOpValue op content)
              Nothing -> return (Right op)
            else do
            return (Right op)

removePR :: (MonadIO m, MonadThrow m) => ConduitM (PositionRange, t) t m ()
removePR = do
  mdata <- await
  case mdata of
    Nothing -> return ()
    Just (_, r) -> do
      yield r
      removePR

getResponseText :: (MonadIO m, MonadThrow m) => ConduitM C.ByteString Response m ()
getResponseText = getResponseText' =$= removePR

getResponseText' :: (MonadIO m, MonadThrow m) => ConduitM C.ByteString (PositionRange, Response) m ()
getResponseText' = conduitParser responseParser

putResponseText :: MonadIO m => ConduitM Response C.ByteString m ()
putResponseText = do
  mResp <- await
  case mResp of
    Nothing -> return ()
    Just resp -> do
      mapM_ yield $ Network.Memcache.Response.toChunks resp
      putResponseText

putOpText :: MonadIO m => ConduitM Op C.ByteString m ()
putOpText = do
  mOp <- await
  case mOp of
    Nothing -> return ()
    Just op -> do
      mapM_ yield $ Network.Memcache.Op.toChunks op
      putOpText

----

endline :: Parser C.ByteString
endline = try (string "\r\n") <|> string "\n" <|> string "\r"

skipSpace' :: Parser ()
skipSpace' = skipWhile (== ' ')

