
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{- | Class definitions

This module is mainly for internal use.

-}
module Network.Memcache.Class (Message(..), Key(..), Value(..)) where

import Control.Monad.IO.Class
import System.IO
import qualified Data.ByteString.Char8 as BS
import Data.Hashable

{- | Key class
-}
class (Hashable a) => Key a where
  toBS :: a -> BS.ByteString

instance Key String where
  toBS = BS.pack

instance Key BS.ByteString where
  toBS k = k

{- | Value class
-}
class Value a where
  serializeValue :: a -> BS.ByteString
  deserializeValue :: BS.ByteString -> Either String a

instance Value String where
  serializeValue = BS.pack
  deserializeValue v = Right (BS.unpack v)

instance Value BS.ByteString where
  serializeValue v = v
  deserializeValue v = Right v

class Message a where
  -- parse message header
  parseHeader :: BS.ByteString -> Maybe a

  -- convert a message to chunks
  toChunks :: a -> [BS.ByteString]

  -- receive content of a message after receiving its header
  recvContent :: (MonadIO m) => Handle -> a -> m (Maybe a)

  -- no content (default impl.)
  recvContent _h msg = return $ Just msg


