-- Copyright (c) 2013, GREE, Inc. All rights reserved.
-- authors: Kiyoshi Ikehara <kiyoshi.ikehara@gree.net>

{- | This is a utility module for handle-based IO opearations
-}
module Network.Memcache.IO (send, recv) where

import Control.Monad.IO.Class
import System.IO
import qualified Data.ByteString.Char8 as BS

import Network.Memcache.Class

{- | Send a message

> send socket FlushAllOp

-}
send :: (MonadIO m, Message a) => Handle -> a -> m ()
send handle msg = liftIO $ do
  -- TODO: this should be done by writev() system call (vector I/O).
  BS.hPutStr handle $ BS.concat (toChunks msg)
  hFlush handle

{- | Receive a message

You may need to specify the return type when you call this function like the code shown below.

> mresp <- recv socket :: IO (Maybe Response)

-}
recv :: (MonadIO m, Message a) => Handle -> m (Maybe a)
recv handle = liftIO $ do
  l <- BS.hGetLine handle
  case parseHeader l of
    Just msg -> recvContent handle msg
    Nothing -> return Nothing

