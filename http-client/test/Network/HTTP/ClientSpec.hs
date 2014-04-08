{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.ClientSpec where

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Concurrent.Async  (withAsync)
import           Control.Exception         (bracket)
import           Control.Monad             (forever, replicateM_)
import           Network                   (PortID (PortNumber), listenOn)
import           Network.HTTP.Client
import           Network.HTTP.Types        (status200)
import           Network.Socket            (accept, sClose)
import           Network.Socket.ByteString (recv, sendAll)
import           Test.Hspec
import qualified Data.Streaming.Network    as N

main :: IO ()
main = hspec spec

redirectServer :: (Int -> IO a) -> IO a
redirectServer inner = bracket
    (N.bindRandomPortTCP "*4")
    (sClose . snd)
    $ \(port, lsocket) -> withAsync
        (N.runTCPServer (N.serverSettingsTCPSocket lsocket) app)
        (const $ inner port)
  where
    app ad = do
        forkIO $ forever $ N.appRead ad
        forever $ do
            N.appWrite ad "HTTP/1.1 301 Redirect\r\nLocation: /\r\ncontent-length: 5\r\n\r\n"
            threadDelay 10000
            N.appWrite ad "hello\r\n"
            threadDelay 10000

spec :: Spec
spec = describe "Client" $ do
    it "works" $ do
        req <- parseUrl "http://www.yesodweb.com/"
        man <- newManager defaultManagerSettings
        res <- httpLbs req man
        responseStatus res `shouldBe` status200
    describe "fails on empty hostnames #40" $ do
        let test url = it url $ do
                req <- parseUrl url
                man <- newManager defaultManagerSettings
                _ <- httpLbs req man `shouldThrow` \e ->
                    case e of
                        InvalidDestinationHost "" -> True
                        _ -> False
                return ()
        mapM_ test ["http://", "https://", "http://:8000", "https://:8001"]
    it "redirecting #41" $ redirectServer $ \port -> do
        req' <- parseUrl $ "http://127.0.0.1:" ++ show port
        let req = req' { redirectCount = 1 }
        withManager defaultManagerSettings $ \man -> replicateM_ 10 $ do
            httpLbs req man `shouldThrow` \e ->
                case e of
                    TooManyRedirects _ -> True
                    _ -> False
    it "redirectCount=0" $ redirectServer $ \port -> do
        req' <- parseUrl $ "http://127.0.0.1:" ++ show port
        let req = req' { redirectCount = 0 }
        withManager defaultManagerSettings $ \man -> replicateM_ 10 $ do
            httpLbs req man `shouldThrow` \e ->
                case e of
                    StatusCodeException{} -> True
                    _ -> False
