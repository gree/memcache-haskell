
{- | This is a utility module for handle-based IO opearations
-}
module Network.Memcache.IO (send, recv, readBytes) where

import Control.Monad.IO.Class
import System.IO
import qualified Data.ByteString.Char8 as BS
import Data.Word

import Network.Memcache.Class

{- | Send a message.
-}
send :: (MonadIO m, Message a) => Handle -> a -> m ()
send handle msg = liftIO $ do
  -- TODO: this should be done by writev() system call (vector I/O).
  BS.hPutStr handle $ BS.concat (toChunks msg)
  hFlush handle

{- | Receive a message.
-}
recv :: (MonadIO m, Message a) => Handle -> m (Maybe a)
recv handle = liftIO $ do
  l <- BS.hGetLine handle
  case parseHeader l of
    Just msg -> recvContent handle msg
    Nothing -> return Nothing

{- | Read data from a handle
-}
readBytes :: Handle
             -> Word64 -- ^ data length
             -> IO (BS.ByteString)
readBytes handle len = BS.hGet handle (fromIntegral len)

