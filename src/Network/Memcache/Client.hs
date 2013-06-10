
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
  , add
  , replace
  , get              
  , delete           
  , incr
  , decr       
  -- Other Operations
  , ping
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
import Data.Serialize hiding (get, put)
import Data.Word

import Network.Memcache.Types
import Network.Memcache.Op
import Network.Memcache.Response
import Network.Memcache.Class

data Client = MemcachedClient {
    clientNodekey :: String
  , clientSocket :: Handle
  }

type StatsList = [(String, String)]
type Nodekey = String

openClient :: (MonadIO m) => Nodekey -> m (Maybe Client)
openClient nodekey = case hostnameAndPort nodekey of
  Just (hostname, port) -> do
    let portNumber = PortNumber (fromIntegral port)
    socket <- liftIO $ connectTo hostname portNumber
    return $ Just $ MemcachedClient nodekey socket
  Nothing -> return Nothing

closeClient :: (MonadIO m) => Client -> m ()
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

-- Utility

withClient :: Nodekey -> (Client -> IO (Maybe a)) -> IO (Maybe a)
withClient nodekey = withClients [nodekey]

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

forEachClient :: [Nodekey] -> (Client -> IO (Maybe a)) -> IO ([Maybe a])
forEachClient clients act = do
  mapM (\c -> withClient c act) clients


-- Query Operations

set :: (MonadIO m, Key k, Serialize v) => Client -> k -> v -> m Bool
set = set' SetOp

set' :: (MonadIO m, Key k, Serialize v) => (BS.ByteString -> Word32 -> Word64 -> Word64 -> BS.ByteString -> [Option] -> Op) -> Client -> k -> v -> m Bool
set' op client key0 value0 = do
  let socket = clientSocket client
      key = encode key0
      value = encode value0
  resp <- liftIO $ do
    send socket $ op key 0 0 (fromIntegral $ BS.length value) value []
    recv socket :: IO (Maybe Response)
  return (resp == Just Stored)

add :: (MonadIO m, Key k, Serialize v) => Client -> k -> v -> m Bool
add = set' AddOp

replace :: (MonadIO m, Key k, Serialize v) => Client -> k -> v -> m Bool
replace = set' ReplaceOp

get :: (MonadIO m, Key k, Serialize v) => Client -> k -> m (Maybe v)
get client key0 = do
  let socket = clientSocket client
      key = encode key0
      op = GetOp [key]
  resp <- liftIO $ do
    send socket op
    recv socket :: IO (Maybe Response)
  case resp of
    Just (Value _ _ _ value _) -> do
      end <- liftIO $ do
        recv socket :: IO (Maybe Response)
      case (end, decode value) of
        (Just End, Right value) -> return (Just value)
        (_, _) -> return (Nothing)
    Just End -> return (Nothing)
    _ -> return (Nothing)

delete :: (MonadIO m, Key k) => Client -> k -> m Bool
delete client key0 = do
  let socket = clientSocket client
      key = encode key0
  resp <- liftIO $ do
    send socket $ DeleteOp key []
    recv socket :: IO (Maybe Response)
  return (resp == Just Deleted)

incr :: (MonadIO m, Key k) => Client -> k -> Int -> m (Maybe Int)
incr client key0 value = do
  let socket = clientSocket client
      key = encode key0
  resp <- liftIO $ do
    send socket $ IncrOp key (fromIntegral value) []
    recv socket :: IO (Maybe Response)
  case resp of
    Just (Code value') -> return (Just $ fromIntegral value')
    _ -> return (Nothing)

decr :: (MonadIO m, Key k) => Client -> k -> Int -> m (Maybe Int)
decr client key0 value = do
  let socket = clientSocket client
      key = encode key0
  resp <- liftIO $ do
    send socket $ DecrOp key (fromIntegral value) []
    recv socket :: IO (Maybe Response)
  case resp of
    Just (Code value') -> return (Just $ fromIntegral value')
    _ -> return (Nothing)

-- Other Operations

ping :: (MonadIO m) => Client -> m (Maybe Response)
ping client = do
  let socket = clientSocket client
      op = PingOp
  liftIO $ send socket op
  resp <- liftIO $ do
    recv socket :: IO (Maybe Response)
  return (resp)

flushAll :: (MonadIO m) => Client -> m (Maybe Response)
flushAll client = do
  let socket = clientSocket client
      op = FlushAllOp
  liftIO $ send socket op
  resp <- liftIO $ do
    recv socket :: IO (Maybe Response)
  return (resp)

stats :: (MonadIO m) => Client -> m (StatsList)
stats client = statsWithArgs client []

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

