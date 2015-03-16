{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.RequestBodySpec where

import Control.Monad
import Test.Hspec
import Control.Exception
import System.IO
import Data.IORef
import qualified Data.ByteString as BS
import Network.HTTP.Client (streamFile, parseUrl, requestBody)
import Network.HTTP.Client.Internal (dummyConnection, Connection, connectionWrite, requestBuilder)
import System.Directory (getTemporaryDirectory)

spec :: Spec
spec = describe "streamFile" $ it "works" $ withTmpFile $ \(path, h) -> do
    replicateM_ 5000 $ BS.hPut h "Hello, world!\r\n"
    hClose h

    withBinaryFile path ReadMode $ \h' -> do
        conn <- verifyFileConnection h'

        req0 <- parseUrl "http://example.com"
        body <- streamFile path
        let req = req0 { requestBody = body }

        requestBuilder req conn
        hIsEOF h' `shouldReturn` True
  where
    withTmpFile = bracket getTmpFile closeTmpFile
    getTmpFile = do
        tmp <- getTemporaryDirectory
        openBinaryTempFile tmp "request-body-stream-file"
    closeTmpFile (_, h) = hClose h

    firstReadBS = "GET / HTTP/1.1\r\nHost: example.com\r\nAccept-Encoding: gzip\r\nContent-Length: 75000\r\n\r\n"

    verifyFileConnection h = do
        (conn, _, _) <- dummyConnection []
        isFirstReadRef <- newIORef True
        return conn
            { connectionWrite = \bs -> do
                isFirstRead <- readIORef isFirstReadRef
                if isFirstRead
                then do
                    bs `shouldBe` firstReadBS
                    writeIORef isFirstReadRef False
                else do
                    bs' <- BS.hGet h (BS.length bs)
                    bs `shouldBe` bs'
            }
