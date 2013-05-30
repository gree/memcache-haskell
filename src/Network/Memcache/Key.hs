

module Network.Memcache.Key () where

import Data.Hashable

class (Hashable a) => Key a where
