{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.Hspec.HUnit
import Test.HUnit

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Control.Concurrent (forkIO)
import Network.HTTP.Enumerator
import Control.Monad (replicateM_)
import Data.Enumerator (run_)
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class (liftIO)

main = hspec $ describe "http-enumerator"
    [ it "doesn't fail with unconsumed data" unconsumed
    ]

unconsumed = do
    forkIO $ run 3000 $ const $ return $ responseLBS status200 [] $ L.fromChunks $ replicate 10000 "this is completely ignored"
    req <- parseUrl "http://localhost:3000"
    withManager $ \m -> run_ $ replicateM_ 10 $ http req (\s _ -> do
        liftIO $ s @?= status200) m
