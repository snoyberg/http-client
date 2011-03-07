{-# LANGUAGE OverloadedStrings #-}
import Network.Wai.Handler.Warp (runEx)
import qualified Network.Wai as W
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Enumerator
import Data.Enumerator (run_)
import Data.ByteString.Lazy.Char8 ()

app _ = do
    liftIO $ putStrLn "Received request"
    return $ W.responseLBS W.status200 [] ""

main = do
    forkIO $ runEx print 3000 app
    req <- parseUrl "http://localhost:3000"
    withManager $ \m -> do
        sequence_ $ replicate 15 $ run_ $ http req (\_ _ -> liftIO (putStrLn "got a response")) m
    threadDelay 1000000
