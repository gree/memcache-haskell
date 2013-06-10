
module Network.Memcache.IO (send, recv, readBytes) where

import Control.Monad.IO.Class
import System.IO
import qualified Data.ByteString.Char8 as BS
-- import qualified Data.ByteString.Lazy as LBS
import Data.Word

import Network.Memcache.Class

-- send a message
send :: (MonadIO m, Message a) => Handle -> a -> m ()
send handle msg = liftIO $ do
  -- TODO this should be done by writev() system call (vector I/O).
  BS.hPutStr handle $ BS.concat (toChunks msg)
  hFlush handle

-- receive a message
recv :: (MonadIO m, Message a) => Handle -> m (Maybe a)
recv handle = liftIO $ do
  l <- BS.hGetLine handle
  case parseHeader l of
    Just msg -> recvContent handle msg
    Nothing -> return Nothing

readBytes :: Handle -> Word64 -> IO (BS.ByteString)
readBytes handle len = BS.hGet handle (fromIntegral len)

