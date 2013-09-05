
{- |
Module: Network.Memcache
Maintainer: Kiyoshi Ikehara <kiyoshi.ikehara@gree.net>

memcache-haskell is a memcache protocol library for server and client application.

-}

module Network.Memcache (
    module Network.Memcache.Client
  , module Network.Memcache.Stats
  ) where

import Network.Memcache.Client
import Network.Memcache.Stats

