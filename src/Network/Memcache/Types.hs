{-# LANGUAGE ExistentialQuantification #-}

module Network.Memcache.Types (Key) where

import Data.Hashable
import Data.Serialize

class (Hashable a, Serialize a) => Key a where

