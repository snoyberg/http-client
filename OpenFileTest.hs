import Prelude hiding (catch)

import Network.HTTP.Enumerator
import Control.Concurrent
import Control.Exception
import qualified Data.ByteString.Lazy as BSL

main =
    do mapM_ (\_ -> simpleHttp "http://localhost/index.html" `catch` handle) [0..100]
       threadDelay 1000000000000000
       putStrLn "Done"
    where
      handle :: SomeException -> IO BSL.ByteString
      handle e =
          do putStrLn (show e)
             return BSL.empty
