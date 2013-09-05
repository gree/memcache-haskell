
{- | This is a utility module for client application.
-}
module Network.Memcache.Client (
    Client
  , StatsList
  , Nodekey
  , openClient
  , closeClient
  , clientNodekey
  , clientSocket
  -- Utility
  , withClient
  , withClients
  , forEachClient
  -- Query Operations
  , set
  , cas
  , add
  , replace
  , get
  , gets
  , delete
  , incr
  , decr
  -- Other Operations
  , flushAll
  , stats
  , statsWithArgs
  ) where

import Prelude hiding (catch)
import System.IO
import Network
import Data.List.Split
import Control.Exception
import qualified Data.ByteString.Char8 as BS
import Control.Monad.IO.Class
import Data.Word

import Network.Memcache.Types
import Network.Memcache.Class
import Network.Memcache.Op
import Network.Memcache.Response
import Network.Memcache.IO

{- | Client is a handler for a memcached session.
-}
data Client = MemcachedClient {
    clientNodekey :: String
  , clientSocket :: Handle
  }

{- | Open a client session and return a client handler.
-}
openClient :: (MonadIO m)
              => Nodekey          -- ^ node key (eg. \"127.0.0.1:11211\")
              -> m (Maybe Client) -- ^ client handler
openClient nodekey = case hostnameAndPort nodekey of
  Just (hostname, port) -> do
    let portNumber = PortNumber (fromIntegral port)
    socket <- liftIO $ connectTo hostname portNumber
    return $ Just $ MemcachedClient nodekey socket
  Nothing -> return Nothing

{- | Close a client session.
-}
closeClient :: (MonadIO m)
               => Client -- ^ client handler
               -> m ()
closeClient client = do
  let hSocket = clientSocket client
  liftIO $ do
    closeClient' `catch` (\(SomeException _e) -> return ())
    hClose hSocket
  where
    closeClient' = do
      let hSocket = clientSocket client
      BS.hPutStr hSocket $ BS.pack "quit\r\n"
      hFlush hSocket
      return ()

{- | Connect and execute an action.
-}
withClient :: Nodekey -> (Client -> IO (Maybe a)) -> IO (Maybe a)
withClient nodekey = withClients [nodekey]

{- | Connect to one of given hosts and execute an action.

>  main = do
>    ret <- withClient ["127.0.0.1:11211"] $ \client -> get client "key"
>    print ret

-}
withClients :: [Nodekey] -> (Client -> IO (Maybe a)) -> IO (Maybe a)
withClients nodekeys act = bracket (allocate nodekeys) release invoke
  where
    allocate :: [Nodekey] -> IO (Maybe Client)
    allocate [] = return Nothing
    allocate (n:ns) = do
      r <- openClient n `catch` (\(SomeException _e) -> return Nothing)
      case r of
        Nothing -> allocate ns
        client -> return client
    release client = case client of
      Just c -> closeClient c
      Nothing -> return ()
    invoke client = case client of
      Just c -> act c
      Nothing -> return Nothing

{- | Connect to the given hosts one by one and execute the action for each client.

>  main = do
>    ret <- forEachClient ["192.168.0.1:11211", "192.168.0.2:11211"] $ flushAll
>    print ret

-}
forEachClient :: [Nodekey] -> (Client -> IO (Maybe a)) -> IO ([Maybe a])
forEachClient clients act = do
  mapM (\c -> withClient c act) clients


-------------------------------- Query Operations

{- | Set an item
-}
set :: (MonadIO m, Key k, Value v) => Client -> k -> v -> m Bool
set = set' SetOp

set' :: (MonadIO m, Key k, Value v) => (BS.ByteString -> Word32 -> Word64 -> Word64 -> BS.ByteString -> [Option] -> Op) -> Client -> k -> v -> m Bool
set' op client key0 value0 = do
  let socket = clientSocket client
      key = toBS key0
      value = serializeValue value0
  resp <- liftIO $ do
    send socket $ op key 0 0 (fromIntegral $ BS.length value) value []
    recv socket :: IO (Maybe Response)
  return (resp == Just Stored)

{- | Cas an item
-}
cas :: (MonadIO m, Key k, Value v) => Client -> k -> v -> Word64 -> m Bool
cas client key value version = set' (\k f e b v o -> CasOp k f e b version v o) client key value

{- | Add an item
-}
add :: (MonadIO m, Key k, Value v) => Client -> k -> v -> m Bool
add = set' AddOp

{- | Replace an item
-}
replace :: (MonadIO m, Key k, Value v) => Client -> k -> v -> m Bool
replace = set' ReplaceOp

{- | Get an item
-}
get :: (MonadIO m, Key k, Value v) => Client -> k -> m (Maybe v)
get client key0 = do
  let socket = clientSocket client
      key = toBS key0
      op = GetOp [key]
  resp <- liftIO $ do
    send socket op
    values <- retrieve socket
    case values of
      ((Value _ _ _ value _):_) -> case deserializeValue value of
        Right v -> return (Just v)
        Left _ -> return (Nothing)
      _ -> return (Nothing)
  return (resp)

retrieve :: Handle -> IO ([Response])
retrieve h = do
  ret <- retrieve'
  return (reverse ret)
  where
    retrieve' = do
      resp <- recv h :: IO (Maybe Response)
      case resp of
        Just value@(Value {}) -> do
          values <- retrieve h
          return (value:values)
        Just End -> return ([])
        _ -> return ([])

{- | Get an item and its version
-}
gets :: (MonadIO m, Key k, Value v) => Client -> k -> m (Maybe (v, Word64))
gets client key0 = do
  let socket = clientSocket client
      key = toBS key0
      op = GetsOp [key]
  resp <- liftIO $ do
    send socket op
    values <- retrieve socket
    case values of
      ((Value _ _ _ value (Just version)):_) -> case deserializeValue value of
        Right v -> return (Just (v, version))
        Left _ -> return (Nothing)
      _ -> return (Nothing)
  return (resp)

{- | Delete an item
-}
delete :: (MonadIO m, Key k) => Client -> k -> m Bool
delete client key0 = do
  let socket = clientSocket client
      key = toBS key0
  resp <- liftIO $ do
    send socket $ DeleteOp key []
    recv socket :: IO (Maybe Response)
  return (resp == Just Deleted)

{- | Increment an item
-}
incr :: (MonadIO m, Key k) => Client -> k -> Int -> m (Maybe Int)
incr client key0 value = do
  let socket = clientSocket client
      key = toBS key0
  resp <- liftIO $ do
    send socket $ IncrOp key (fromIntegral value) []
    recv socket :: IO (Maybe Response)
  case resp of
    Just (Code value') -> return (Just $ fromIntegral value')
    _ -> return (Nothing)

{- | Decrement an item
-}
decr :: (MonadIO m, Key k) => Client -> k -> Int -> m (Maybe Int)
decr client key0 value = do
  let socket = clientSocket client
      key = toBS key0
  resp <- liftIO $ do
    send socket $ DecrOp key (fromIntegral value) []
    recv socket :: IO (Maybe Response)
  case resp of
    Just (Code value') -> return (Just $ fromIntegral value')
    _ -> return (Nothing)

-- Other Operations

{- | Flush all items
-}
flushAll :: (MonadIO m) => Client -> m (Maybe Response)
flushAll client = do
  let socket = clientSocket client
      op = FlushAllOp
  liftIO $ send socket op
  resp <- liftIO $ do
    recv socket :: IO (Maybe Response)
  return (resp)

{- | Acquire statistic information
-}
stats :: (MonadIO m) => Client -> m (StatsList)
stats client = statsWithArgs client []

{- | Acquire statistic information with arguments
-}
statsWithArgs :: (MonadIO m) => Client -> [String] -> m ([(String, String)])
statsWithArgs client args = do
  let socket = clientSocket client
  liftIO $ send socket $ StatsOp (map BS.pack args)
  resp <- getResponse socket []    
  return (Prelude.reverse resp)
  where
    getResponse sock result = do
      resp <- liftIO $ recv sock
      case resp of
        Just (Stat statName statValue) -> getResponse sock ((BS.unpack statName, BS.unpack statValue):result)
        Just End -> return (result)
        _ -> getResponse sock result

----------------------------------------------------------------

hostnameAndPort :: String -> Maybe (String, Int)
hostnameAndPort nk = case Data.List.Split.splitOn ":" nk of
  (hostname:port:[]) -> Just (hostname, (read port :: Int))
  _ -> Nothing

