{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.Hspec.HUnit
import Test.HUnit

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Control.Concurrent (forkIO, killThread)
import Control.Exception (finally)
import Network.HTTP.Enumerator
import Control.Monad (replicateM_)
import Data.Enumerator (run_)
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as S8
import Warp

main = hspec $ describe "http-enumerator"
    [ it "doesn't fail with unconsumed data, chunked" unconsumedChunked
    , it "doesn't fail with unconsumed data, unchunked" unconsumedUnchunked
    , it "handles closed connections properly" closedConnections
    ]

unconsumedChunked = do
    tid <- forkIO $ run 3002 $ const $ return $ responseLBS status200 [] $ L.fromChunks $ replicate 10000 "this is completely ignored"
    flip finally (killThread tid) $ do
        req <- parseUrl "http://localhost:3002"
        withManager $ \m -> run_ $ replicateM_ 10 $ http req (\s h -> do
            liftIO $ s @?= status200
            liftIO $ lookup "transfer-encoding" h @?= Just "chunked"
            ) m

unconsumedUnchunked = do
    let lbs = L.fromChunks $ replicate 10000 "this is completely ignored"
    tid <- forkIO $ run 3001 $ const $ return $ responseLBS status200 [("Content-Length", S8.pack $ show $ L.length lbs)] lbs
    flip finally (killThread tid) $ do
        req <- parseUrl "http://localhost:3001"
        withManager $ \m -> run_ $ replicateM_ 10 $ http req (\s h -> do
            liftIO $ s @?= status200
            liftIO $ lookup "transfer-encoding" h @?= Nothing
            ) m

closedConnections = do
    tid <- forkIO noKeepAlive
    flip finally (killThread tid) $ do
        req <- parseUrl "http://localhost:3000"
        withManager $ \m -> run_ $ do
            res <- httpLbs req m
            replicateM_ 10 $ do
                res' <- httpLbs req m
                liftIO $ res @?= res'
