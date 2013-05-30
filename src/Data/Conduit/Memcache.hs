{-# LANGUAGE OverloadedStrings #-}

module Data.Conduit.Memcache (parseText, process, response) where

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
parseText = parseText' =$= parseText''

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

parseText'' :: (MonadIO m, MonadThrow m) => ConduitM (PositionRange, Op) Op m ()
parseText'' = do
  mOp <- await
  case mOp of
    Nothing -> return ()
    Just (_, op) -> do
      yield op
      parseText''
      
process :: MonadIO m => (Op -> IO Response) -> ConduitM Op Response m ()
process exec = do
  mOpType <- await
  case mOpType of
    Nothing -> return ()
    Just op -> case op of
      QuitOp -> do
        yield Ok
        return ()
      _ -> do
        resp <- liftIO $ exec op
        yield resp
        process exec

response :: MonadIO m => ConduitM Response C.ByteString m ()
response = do
  mResp <- await
  case mResp of
    Nothing -> return ()
    Just resp -> do
      mapM_ yield $ Network.Memcache.Response.toChunks resp
      response

----

endline :: Parser C.ByteString
endline = try (string "\r\n") <|> string "\n" <|> string "\r"

skipSpace' :: Parser ()
skipSpace' = skipWhile (== ' ')

