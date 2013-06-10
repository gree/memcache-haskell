{-# LANGUAGE OverloadedStrings #-}

module Data.Conduit.Memcache (getOpText, getResponseText, putOpText, putResponseText) where

import Prelude hiding (takeWhile)
import Control.Monad.Trans
-- import Control.Monad.IO.Class
import Control.Applicative
import qualified Data.Attoparsec.ByteString as AB
import Data.Attoparsec.ByteString.Char8
-- import Data.Attoparsec.Binary
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Conduit.Attoparsec

import Network.Memcache.Class
import Network.Memcache.Op
import Network.Memcache.Response

getOpText :: (MonadIO m, MonadThrow m) => ConduitM BS.ByteString (Either BS.ByteString Op) m ()
getOpText = getOpText' =$= removePR

getOpText' :: (MonadIO m, MonadThrow m) => ConduitM BS.ByteString (PositionRange, Either BS.ByteString Op) m ()
getOpText' = conduitParser command
  where
    command :: Parser (Either BS.ByteString Op)
    command = try (Right <$> opParser) <|> (Left <$> (AB.takeWhile (\c -> c /= 10 && c /= 13) <* endline))

    endline :: Parser BS.ByteString
    endline = try (string "\r\n") <|> string "\n" <|> string "\r"

removePR :: (MonadIO m, MonadThrow m) => ConduitM (PositionRange, t) t m ()
removePR = do
  mdata <- await
  case mdata of
    Nothing -> return ()
    Just (_, r) -> do
      yield r
      removePR

getResponseText :: (MonadIO m, MonadThrow m) => ConduitM BS.ByteString Response m ()
getResponseText = getResponseText' =$= removePR

getResponseText' :: (MonadIO m, MonadThrow m) => ConduitM BS.ByteString (PositionRange, Response) m ()
getResponseText' = conduitParser responseParser

putResponseText :: MonadIO m => ConduitM Response BS.ByteString m ()
putResponseText = do
  mResp <- await
  case mResp of
    Nothing -> return ()
    Just resp -> do
      mapM_ yield $ toChunks resp
      -- yield $ BS.concat $ Network.Memcache.Response.toChunks resp
      putResponseText

putOpText :: MonadIO m => ConduitM Op BS.ByteString m ()
putOpText = do
  mOp <- await
  case mOp of
    Nothing -> return ()
    Just op -> do
      mapM_ yield $ toChunks op
      -- yield $ BS.concat $ Network.Memcache.Op.toChunks op
      putOpText

