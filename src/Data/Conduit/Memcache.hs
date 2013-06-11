{-# LANGUAGE OverloadedStrings #-}

module Data.Conduit.Memcache (getOpText, getResponseText, putOpText, putResponseText) where

import Prelude hiding (takeWhile)
import Control.Monad.Trans
import Control.Applicative
import qualified Data.Attoparsec.ByteString as AB
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Conduit.Attoparsec

import Network.Memcache.Class
import Network.Memcache.Op
import Network.Memcache.Response

getOpText :: (MonadIO m, MonadThrow m) => ConduitM BS.ByteString (Either BS.ByteString Op) m ()
getOpText = conduitParser command =$= removePR
  where
    command :: Parser (Either BS.ByteString Op)
    command = try (Right <$> opParser) <|> (Left <$> (AB.takeWhile (\c -> c /= 10 && c /= 13) <* endline))

    endline :: Parser BS.ByteString
    endline = try (string "\r\n") <|> string "\n" <|> string "\r"

getResponseText :: (MonadIO m, MonadThrow m) => ConduitM BS.ByteString Response m ()
getResponseText = conduitParser responseParser =$= removePR

putResponseText :: MonadIO m => ConduitM Response BS.ByteString m ()
putResponseText = do
  mResp <- await
  case mResp of
    Nothing -> return ()
    Just resp -> do
      yield $ BS.concat $ toChunks resp
      putResponseText

putOpText :: MonadIO m => ConduitM Op BS.ByteString m ()
putOpText = do
  mOp <- await
  case mOp of
    Nothing -> return ()
    Just op -> do
      yield $ BS.concat $ toChunks op
      putOpText

----------------------------------------------------------------

removePR :: (MonadIO m, MonadThrow m) => ConduitM (PositionRange, t) t m ()
removePR = do
  mdata <- await
  case mdata of
    Nothing -> return ()
    Just (_, r) -> do
      yield r
      removePR
