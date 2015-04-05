{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.HeadersSpec where

import           Network.HTTP.Client.Internal
import           Network.HTTP.Types
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "HeadersSpec" $ do
    it "simple response" $ do
        let input =
                [ "HTTP/"
                , "1.1 200"
                , " OK\r\nfoo"
                , ": bar\r\n"
                , "baz:bin\r\n\r"
                , "\nignored"
                ]
        (connection, _, _) <- dummyConnection input
        statusHeaders <- parseStatusHeaders connection Nothing Nothing
        statusHeaders `shouldBe` StatusHeaders status200 (HttpVersion 1 1)
            [ ("foo", "bar")
            , ("baz", "bin")
            ]

    it "Expect: 100-continue (success)" $ do
        let input =
                [ "HTTP/1.1 100 Continue\r\n\r\n"
                , "HTTP/1.1 200 OK\r\n"
                , "foo: bar\r\n\r\n"
                ]
        (conn, out, _) <- dummyConnection input
        let sendBody = connectionWrite conn "data"
        statusHeaders <- parseStatusHeaders conn Nothing (Just sendBody)
        statusHeaders `shouldBe` StatusHeaders status200 (HttpVersion 1 1) [ ("foo", "bar") ]
        out >>= (`shouldBe` ["data"])

    it "Expect: 100-continue (failure)" $ do
        let input =
                [ "HTTP/1.1 417 Expectation Failed\r\n\r\n"
                ]
        (conn, out, _) <- dummyConnection input
        let sendBody = connectionWrite conn "data"
        statusHeaders <- parseStatusHeaders conn Nothing (Just sendBody)
        statusHeaders `shouldBe` StatusHeaders status417 (HttpVersion 1 1) []
        out >>= (`shouldBe` [])

    it "100 Continue without expectation is OK" $ do
        let input =
                [ "HTTP/1.1 100 Continue\r\n\r\n"
                , "HTTP/1.1 200 OK\r\n"
                , "foo: bar\r\n\r\n"
                , "result"
                ]
        (conn, out, inp) <- dummyConnection input
        statusHeaders <- parseStatusHeaders conn Nothing Nothing
        statusHeaders `shouldBe` StatusHeaders status200 (HttpVersion 1 1) [ ("foo", "bar") ]
        out >>= (`shouldBe` [])
        inp >>= (`shouldBe` ["result"])
