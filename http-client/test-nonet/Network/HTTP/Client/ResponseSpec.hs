{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.HTTP.Client.ResponseSpec where

import Test.Hspec
import Network.HTTP.Client
import Network.HTTP.Client.Internal
import Network.HTTP.Types
import Codec.Compression.GZip (compress)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString as S

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "ResponseSpec" $ do
    let getResponse' conn = getResponse (const $ return ()) Nothing req conn Nothing
        Just req = parseUrl "http://localhost"
    it "basic" $ do
        (conn, _, _) <- dummyConnection
            [ "HTTP/1.1 200 OK\r\n"
            , "Key1: Value1\r\n"
            , "Content-length: 11\r\n\r\n"
            , "Hello"
            , " W"
            , "orld\r\nHTTP/1.1"
            ]
        Response {..} <- getResponse' conn
        responseStatus `shouldBe` status200
        responseVersion `shouldBe` HttpVersion 1 1
        responseHeaders `shouldBe`
            [ ("Key1", "Value1")
            , ("Content-length", "11")
            ]
        pieces <- brConsume responseBody
        pieces `shouldBe` ["Hello", " W", "orld"]
    it "no length" $ do
        (conn, _, _) <- dummyConnection
            [ "HTTP/1.1 200 OK\r\n"
            , "Key1: Value1\r\n\r\n"
            , "Hello"
            , " W"
            , "orld\r\nHTTP/1.1"
            ]
        Response {..} <- getResponse' conn
        responseStatus `shouldBe` status200
        responseVersion `shouldBe` HttpVersion 1 1
        responseHeaders `shouldBe`
            [ ("Key1", "Value1")
            ]
        pieces <- brConsume responseBody
        pieces `shouldBe` ["Hello", " W", "orld\r\nHTTP/1.1"]
    it "chunked" $ do
        (conn, _, _) <- dummyConnection
            [ "HTTP/1.1 200 OK\r\n"
            , "Key1: Value1\r\n"
            , "Transfer-encoding: chunked\r\n\r\n"
            , "5\r\nHello\r"
            , "\n2\r\n W"
            , "\r\n4  ignored\r\norld\r\n0\r\nHTTP/1.1"
            ]
        Response {..} <- getResponse' conn
        responseStatus `shouldBe` status200
        responseVersion `shouldBe` HttpVersion 1 1
        responseHeaders `shouldBe`
            [ ("Key1", "Value1")
            , ("Transfer-encoding", "chunked")
            ]
        pieces <- brConsume responseBody
        pieces `shouldBe` ["Hello", " W", "orld"]
    it "gzip" $ do
        (conn, _, _) <- dummyConnection
            $ "HTTP/1.1 200 OK\r\n"
            : "Key1: Value1\r\n"
            : "Content-Encoding: gzip\r\n\r\n"
            : L.toChunks (compress "Compressed Hello World")
        Response {..} <- getResponse' conn
        responseStatus `shouldBe` status200
        responseVersion `shouldBe` HttpVersion 1 1
        responseHeaders `shouldBe`
            [ ("Key1", "Value1")
            , ("Content-Encoding", "gzip")
            ]
        pieces <- brConsume responseBody
        S.concat pieces `shouldBe` "Compressed Hello World"
