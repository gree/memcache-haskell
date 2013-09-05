{-# LANGUAGE OverloadedStrings #-}


{- | This is a conduit library for memcache protocol.

> type Key = BS.ByteString
> type Version = Word64
> type Value = (Version, BS.ByteString)
> type HashTable k v = H.BasicHashTable k v
> 
> 
> main :: IO ()
> main = do
>   ht <- H.new :: IO (HashTable Key Value)
>   htVar <- newMVar ht
>   runResourceT $ do
>     runTCPServer (serverSettings 11211 HostAny) $ \appData -> do
>       (appSource appData)
>         $$ getOpText
>         =$ process htVar
>         =$ putResponseText
>         =$ (appSink appData)
> 
> process :: (MonadResource m, MonadIO m) => MVar (HashTable Key Value) -> ConduitM (Either BS.ByteString Op) Response m ()
> process = ...

-}
module Data.Conduit.Memcache (getOpText, getResponseText, putOpText, putResponseText) where

import Prelude hiding (takeWhile)
import Control.Monad.Trans
import Control.Applicative
import qualified Data.Attoparsec.ByteString as AB
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Conduit.Attoparsec

import Network.Memcache.Class
import Network.Memcache.Op
import Network.Memcache.Response

{- | This conduit parses command messages of memcache's text protocol and generates "Network.Memcache.Op".

  [@input@] "Data.ByteString.Char8.ByteString"

  [@output@] "Network.Memcache.Op"

-}
getOpText :: (MonadIO m, MonadThrow m) => ConduitM BS.ByteString (Either BS.ByteString Op) m ()
getOpText = conduitParser opParser' =$= removePR
  where
    opParser' :: Parser (Either BS.ByteString Op)
    opParser' = try (Right <$> opParser)
                <|> (Left <$> (AB.takeWhile (\c -> c /= 10 && c /= 13) <* endline))

{- | This conduit parses response messages of memcache's text protocol and generates "Network.Memcache.Response".

  [@input@] "Data.ByteString.Char8.ByteString"

  [@output@] "Network.Memcache.Response"

-}
getResponseText :: (MonadIO m, MonadThrow m) => ConduitM BS.ByteString (Either BS.ByteString Response) m ()
getResponseText = conduitParser responseParser' =$= removePR
  where
    responseParser' :: Parser (Either BS.ByteString Response)
    responseParser' = try (Right <$> responseParser)
                      <|> (Left <$> (AB.takeWhile (\c -> c /= 10 && c /= 13) <* endline))

{- | This generates command messages of memcache's text protocol from "Network.Memcache.Op"

  [@input@] "Network.Memcache.Op"

  [@output@] "Data.ByteString.Char8.ByteString"

-}
putOpText :: MonadIO m => ConduitM Op BS.ByteString m ()
putOpText = loop
  where
    loop = await >>= maybe (return ()) (\op -> (yield $ BS.concat $ toChunks op) >> loop)

{- | This generates response messages of memcache's text protocol from "Network.Memcache.Response"

  [@input@] "Network.Memcache.Response"

  [@output@] "Data.ByteString.Char8.ByteString"

-}
putResponseText :: MonadIO m => ConduitM Response BS.ByteString m ()
putResponseText = loop
  where
    loop = await >>= maybe (return ()) (\resp -> (yield $ BS.concat $ toChunks resp) >> loop)


----------------------------------------------------------------

endline :: Parser BS.ByteString
endline = try (string "\r\n") <|> string "\n" <|> string "\r"

removePR :: (MonadIO m, MonadThrow m, Show t) => ConduitM (PositionRange, t) t m ()
removePR = loop
  where
    loop = await >>= maybe (return ()) (\(_, r) -> (yield r) >> loop)
