{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.ClientSpec where

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Concurrent.Async  (withAsync)
import           Control.Exception         (bracket)
import           Control.Monad             (forever, replicateM_, void)
import           Network.HTTP.Client
import           Network.HTTP.Types        (status413)
import           Network.Socket            (sClose)
import           Test.Hspec
import qualified Data.Streaming.Network    as N
import qualified Data.ByteString           as S
import qualified Data.ByteString.Lazy      as SL
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

-- Make sure we detect bad situations like
-- https://github.com/yesodweb/wai/issues/346 better than we did previously, so
-- that misreporting like https://github.com/snoyberg/http-client/issues/108
-- doesn't occur.
lengthAndChunked :: (Int -> IO a) -> IO a
lengthAndChunked = serveWith "HTTP/1.1 200 OK\r\ncontent-length: 24\r\ntransfer-encoding: chunked\r\n\r\n4\r\nWiki\r\n5\r\npedia\r\ne\r\n in\r\n\r\nchunks.\r\n0\r\n\r\n"

lengthZeroAndChunked :: (Int -> IO a) -> IO a
lengthZeroAndChunked = serveWith "HTTP/1.1 200 OK\r\ncontent-length: 0\r\ntransfer-encoding: chunked\r\n\r\n4\r\nWiki\r\n5\r\npedia\r\ne\r\n in\r\n\r\nchunks.\r\n0\r\n\r\n"

lengthZeroAndChunkZero :: (Int -> IO a) -> IO a
lengthZeroAndChunkZero = serveWith "HTTP/1.1 200 OK\r\ncontent-length: 0\r\ntransfer-encoding: chunked\r\n\r\n0\r\n\r\n"

serveWith :: S.ByteString -> (Int -> IO a) -> IO a
serveWith resp inner = bracket
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
        N.appWrite ad resp

getChunkedResponse :: Int -> Manager -> IO (Response SL.ByteString)
getChunkedResponse port' man = flip httpLbs man "http://localhost"
  { port        = port'
  , checkStatus = \_ _ _ -> Nothing
  , requestBody = RequestBodyStreamChunked ($ return (S.replicate 100000 65))
  }

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
            res <- getChunkedResponse port' man
            responseBody res `shouldBe` "goodbye"
            responseStatus res `shouldBe` status413

    it "length zero and chunking zero #108" $ lengthZeroAndChunkZero $ \port' -> do
        withManager defaultManagerSettings $ \man -> do
            res <- getChunkedResponse port' man
            responseBody res `shouldBe` ""

    it "length zero and chunking" $ lengthZeroAndChunked $ \port' -> do
        withManager defaultManagerSettings $ \man -> do
            res <- getChunkedResponse port' man
            responseBody res `shouldBe` "Wikipedia in\r\n\r\nchunks."

    it "length and chunking" $ lengthAndChunked $ \port' -> do
        withManager defaultManagerSettings $ \man -> do
            res <- getChunkedResponse port' man
            responseBody res `shouldBe` "Wikipedia in\r\n\r\nchunks."
