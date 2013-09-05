
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Network.Memcache.Class (Message(..), Key(..)) where

import Control.Monad.IO.Class
import System.IO
import qualified Data.ByteString.Char8 as BS
import Data.Hashable
import Data.Serialize

class (Hashable a) => Key a where
  toBS :: a -> BS.ByteString

instance Key String where
  toBS = BS.pack
  
class Message a where
  -- parse message header
  parseHeader :: BS.ByteString -> Maybe a

  -- convert a message to chunks
  toChunks :: a -> [BS.ByteString]

  -- receive content of a message after receiving its header
  recvContent :: (MonadIO m) => Handle -> a -> m (Maybe a)

  -- no content (default impl.)
  recvContent _h msg = return $ Just msg


