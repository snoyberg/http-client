{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.ClientSpec where

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Concurrent.Async  (withAsync)
import           Control.Exception         (bracket)
import           Control.Monad             (forever, replicateM_)
import           Network.HTTP.Client
import           Network.HTTP.Types        (status413)
import           Network.Socket            (sClose)
import           Test.Hspec
import qualified Data.Streaming.Network    as N
import qualified Data.ByteString           as S
import           Data.ByteString.Lazy.Char8 () -- orphan instance

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

bad100Server :: Bool -- ^ include extra headers?
             -> (Int -> IO a) -> IO a
bad100Server extraHeaders inner = bracket
    (N.bindRandomPortTCP "*4")
    (sClose . snd)
    $ \(port, lsocket) -> withAsync
        (N.runTCPServer (N.serverSettingsTCPSocket lsocket) app)
        (const $ inner port)
  where
    app ad = do
        forkIO $ forever $ N.appRead ad
        forever $ do
            N.appWrite ad $ S.concat
                [ "HTTP/1.1 100 Continue\r\n"
                , if extraHeaders then "foo:bar\r\nbaz: bin\r\n" else ""
                , "\r\nHTTP/1.1 200 OK\r\ncontent-length: 5\r\n\r\nhello\r\n"
                ]
            threadDelay 10000

earlyClose413 :: (Int -> IO a) -> IO a
earlyClose413 inner = bracket
    (N.bindRandomPortTCP "*4")
    (sClose . snd)
    $ \(port, lsocket) -> withAsync
        (N.runTCPServer (N.serverSettingsTCPSocket lsocket) app)
        (const $ inner port)
  where
    app ad = do
        let readHeaders front = do
                newBS <- N.appRead ad
                let bs = S.append front newBS
                if "\r\n\r\n" `S.isInfixOf` bs
                    then return ()
                    else readHeaders bs
        readHeaders S.empty
        N.appWrite ad "HTTP/1.1 413 Too Large\r\ncontent-length: 7\r\n\r\ngoodbye"

spec :: Spec
spec = describe "Client" $ do
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
    it "connecting to missing server gives nice error message" $ do
        (port, socket) <- N.bindRandomPortTCP "*4"
        sClose socket
        req <- parseUrl $ "http://127.0.0.1:" ++ show port
        withManager defaultManagerSettings $ \man ->
            httpLbs req man `shouldThrow` \e ->
                case e of
                    FailedConnectionException2 "127.0.0.1" port' False _ -> port == port'
                    _ -> False

    describe "extra headers after 100 #49" $ do
        let test x = it (show x) $ bad100Server x $ \port -> do
                req <- parseUrl $ "http://127.0.0.1:" ++ show port
                withManager defaultManagerSettings $ \man -> replicateM_ 10 $ do
                    x <- httpLbs req man
                    responseBody x `shouldBe` "hello"
        test False
        test True

    it "early close on a 413" $ earlyClose413 $ \port' -> do
        withManager defaultManagerSettings $ \man -> do
            res <- flip httpLbs man "http://localhost"
                { port = port'
                , checkStatus = \_ _ _ -> Nothing
                , requestBody = RequestBodyStreamChunked
                    ($ return (S.replicate 100000 65))
                }
            responseBody res `shouldBe` "goodbye"
            responseStatus res `shouldBe` status413
