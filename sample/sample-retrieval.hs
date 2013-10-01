
import Network.Memcache
  
main = do
  mValue <- withClient "127.0.0.1:11211" $ \client -> get client "key"
  case mValue of
    Nothing -> putStrLn "(no value)"
    Just value -> putStrLn value
  
