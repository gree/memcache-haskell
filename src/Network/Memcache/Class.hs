
module Network.Memcache.Class (Message(..), readBytes, chompLine) where

import Control.Monad.IO.Class
import System.IO
import qualified Data.ByteString.Char8 as C
import Data.Word

class Message a where
  -- parse message header
  parseHeader :: C.ByteString -> Maybe a
  -- convert a message to chunks
  toChunks :: a -> [C.ByteString]
  -- send a message
  send :: (MonadIO m) => Handle -> a -> m ()
  -- receive a message
  recv :: (MonadIO m) => Handle -> m (Maybe a)
  -- receive content of a message after receiving its header
  recvContent :: (MonadIO m) => Handle -> a -> m (Maybe a)

  -- send a message using toChunks (default impl.)
  send handle msg = liftIO $ do
    -- TODO this should be done by writev() system call (vector I/O).
    C.hPutStr handle $ C.concat (Network.Memcache.Class.toChunks msg)
    hFlush handle

  recv handle = liftIO $ do
    l <- C.hGetLine handle
    case parseHeader $ chompLine l of
      Just op -> recvContent handle op
      Nothing -> return Nothing

  -- no content (default impl.)
  recvContent _h msg = return $ Just msg

----------------------------------------------------------------

readBytes :: Handle -> Word64 -> IO (C.ByteString)
readBytes handle len = C.hGet handle (fromIntegral len)

chompLine :: C.ByteString -> C.ByteString
chompLine str
  | C.null str = str
  | C.isSuffixOf (C.pack "\r\n") str = C.take (C.length str - 2) str
  | C.isSuffixOf (C.pack "\n") str || C.isSuffixOf (C.pack "\r") str = C.take (C.length str - 1) str
  | otherwise = str
