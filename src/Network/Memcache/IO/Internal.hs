-- Copyright (c) 2013, GREE, Inc. All rights reserved.
-- authors: Kiyoshi Ikehara <kiyoshi.ikehara@gree.net>

module Network.Memcache.IO.Internal where

import System.IO
import qualified Data.ByteString.Char8 as BS
import Data.Word

{- | Read data from a handle
-}
readBytes :: Handle    -- ^ a I/O handler
             -> Word64 -- ^ data length
             -> IO (BS.ByteString)
readBytes handle len = BS.hGet handle (fromIntegral len)

