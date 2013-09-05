
import Control.Exception
import Control.Monad.Trans.Resource (allocate, release, runResourceT)
import Control.Monad.IO.Class
import Network.Memcache
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> main' "127.0.0.1:11211"
    (nk:_) -> main' nk

main' :: Nodekey -> IO ()
main' nodekey = runResourceT $ do
  (rkeyClient , client) <- flip allocate closeClient $ do
    c <- openClient nodekey
    case c of
      Just client -> return (client)
      Nothing -> liftIO $ do
        throwIO (userError "could not open.")
  liftIO $ do
    ret <- set client "key" "foo"
    print ret
    ret' <- get client "key" :: IO (Maybe String)
    print ret'
  release rkeyClient
