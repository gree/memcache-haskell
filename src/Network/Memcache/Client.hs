
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
  -- Operation
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
import qualified Data.ByteString.Char8 as C

import Network.Memcache.Op
import Network.Memcache.Response
import Network.Memcache.Message

data Client = MemcachedClient {
    clientNodekey :: String
  , clientSocket :: Handle
  }

type StatsList = [(String, String)]
type Nodekey = String

openClient :: Nodekey -> IO (Maybe Client)
openClient nodekey = case hostnameAndPort nodekey of
  Just (hostname, port) -> do
    let portNumber = PortNumber (fromIntegral port)
    socket <- connectTo hostname portNumber
    return $ Just $ MemcachedClient nodekey socket
  Nothing -> return Nothing

closeClient :: Client -> IO ()
closeClient client = do
  let hSocket = clientSocket client
  closeClient' `catch` (\(SomeException _e) -> return ())
  hClose hSocket
  where
    closeClient' = do
      let hSocket = clientSocket client
      C.hPutStr hSocket $ C.pack "quit\r\n"
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

-- Operation

ping :: Client -> IO (Maybe Response)
ping client = do
  let socket = clientSocket client
      op = PingOp
  send socket op
  resp <- recv socket :: IO (Maybe Response)
  return (resp)

flushAll :: Client -> IO (Maybe Response)
flushAll client = do
  let socket = clientSocket client
      op = FlushAllOp
  send socket op
  resp <- recv socket :: IO (Maybe Response)
  return (resp)

stats :: Client -> IO (StatsList)
stats client = statsWithArgs client []

statsWithArgs :: Client -> [String] -> IO ([(String, String)])
statsWithArgs client args = do
  let socket = clientSocket client
  send socket $ StatsOp args
  resp <- getResponse socket []    
  return (Prelude.reverse resp)
  where
    getResponse sock result = do
      resp <- recv sock
      case resp of
        Just (Stat statName statValue) -> getResponse sock ((statName, statValue):result)
        Just End -> return (result)
        _ ->  getResponse sock result

----------------------------------------------------------------

hostnameAndPort :: String -> Maybe (String, Int)
hostnameAndPort nk = case Data.List.Split.splitOn ":" nk of
  (hostname:port:[]) -> Just (hostname, (read port :: Int))
  _ -> Nothing

