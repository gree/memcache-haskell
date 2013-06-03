{-# LANGUAGE ExistentialQuantification #-}

module Network.Memcache.Types () where

data MemcacheMessage = forall m. Message m => Message m

data Response =
    Ok
  | Value {
      resKey     :: C.ByteString
    , resFlag    :: Word32
    , resLen     :: Word64
    , resValue   :: C.ByteString
    , resVersion :: Maybe Word64 -- Value key flag value
    }
  | End
  | Stored
  | NotStored
  | Exists
  | NotFound
  | Deleted
  | Found
  | Touched
  | Error
  | ServerError String
  | ClientError String
  | Version String
  | Stat String String -- name and value pair
  | Code Word64
  deriving (Show, Eq)

