{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Data.Conduit.Memcache (getOpText, getResponseText, putOpText, putResponseText) where

import Prelude hiding (takeWhile)
import Control.Monad.Trans
import Control.Applicative
import qualified Data.Attoparsec.ByteString as AB
import Data.Attoparsec.ByteString.Char8
-- import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Conduit.Attoparsec

import Network.Memcache.Class
import Network.Memcache.Op
import Network.Memcache.Response

getOpText :: (MonadIO m, MonadThrow m) => ConduitM BS.ByteString (Either BS.ByteString Op) m ()
getOpText = conduitParser opParser' =$= removePR
  where
    opParser' :: Parser (Either BS.ByteString Op)
    opParser' = try (Right <$> opParser)
                <|> (Left <$> (AB.takeWhile (\c -> c /= 10 && c /= 13) <* endline))

{-
    loop = do
      mbs <- await
      case mbs of
        Just bs -> do
          case parseOp bs of
            Just op -> yield $ Right op
            Nothing -> yield $ Left bs
          loop
        Nothing -> return ()
-}

getResponseText :: (MonadIO m, MonadThrow m) => ConduitM BS.ByteString (Either BS.ByteString Response) m ()
getResponseText = conduitParser responseParser' =$= removePR
  where
    responseParser' :: Parser (Either BS.ByteString Response)
    responseParser' = try (Right <$> responseParser)
                      <|> (Left <$> (AB.takeWhile (\c -> c /= 10 && c /= 13) <* endline))
{-
    loop = do
      mbs <- await
      case mbs of
        Just bs -> do
          case parseResponse bs of
            Just op -> yield $ Right op
            Nothing -> yield $ Left bs
          loop
        Nothing -> return ()
-}

putResponseText :: MonadIO m => ConduitM Response BS.ByteString m ()
putResponseText = loop
  where
    loop = await >>= maybe (return ()) (\resp -> (yield $ BS.concat $ toChunks resp) >> loop)

putOpText :: MonadIO m => ConduitM Op BS.ByteString m ()
putOpText = loop
  where
    loop = await >>= maybe (return ()) (\op -> (yield $ BS.concat $ toChunks op) >> loop)

----------------------------------------------------------------

endline :: Parser BS.ByteString
endline = try (string "\r\n") <|> string "\n" <|> string "\r"

removePR :: (MonadIO m, MonadThrow m, Show t) => ConduitM (PositionRange, t) t m ()
removePR = loop
  where
    loop = await >>= maybe (return ()) (\(_, r) -> (yield r) >> loop)
