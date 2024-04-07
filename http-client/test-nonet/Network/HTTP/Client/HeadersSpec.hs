{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.HTTP.Client.HeadersSpec where

import           Control.Concurrent.MVar
import qualified Data.Sequence as Seq
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
        statusHeaders <- parseStatusHeaders Nothing Nothing connection Nothing (\_ -> return ()) Nothing
        statusHeaders `shouldBe` StatusHeaders status200 (HttpVersion 1 1) mempty
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
        statusHeaders <- parseStatusHeaders Nothing Nothing conn Nothing (\_ -> return ()) (Just sendBody)
        statusHeaders `shouldBe` StatusHeaders status200 (HttpVersion 1 1) [] [ ("foo", "bar") ]
        out >>= (`shouldBe` ["data"])

    it "Expect: 100-continue (failure)" $ do
        let input =
                [ "HTTP/1.1 417 Expectation Failed\r\n\r\n"
                ]
        (conn, out, _) <- dummyConnection input
        let sendBody = connectionWrite conn "data"
        statusHeaders <- parseStatusHeaders Nothing Nothing conn Nothing (\_ -> return ()) (Just sendBody)
        statusHeaders `shouldBe` StatusHeaders status417 (HttpVersion 1 1) [] []
        out >>= (`shouldBe` [])

    it "100 Continue without expectation is OK" $ do
        let input =
                [ "HTTP/1.1 100 Continue\r\n\r\n"
                , "HTTP/1.1 200 OK\r\n"
                , "foo: bar\r\n\r\n"
                , "result"
                ]
        (conn, out, inp) <- dummyConnection input
        statusHeaders <- parseStatusHeaders Nothing Nothing conn Nothing (\_ -> return ()) Nothing
        statusHeaders `shouldBe` StatusHeaders status200 (HttpVersion 1 1) [] [ ("foo", "bar") ]
        out >>= (`shouldBe` [])
        inp >>= (`shouldBe` ["result"])

    it "103 early hints" $ do
        let input =
                [ "HTTP/1.1 103 Early Hints\r\n"
                , "Link: </foo.js>\r\n"
                , "Link: </bar.js>\r\n\r\n"
                , "HTTP/1.1 200 OK\r\n"
                , "Content-Type: text/html\r\n\r\n"
                , "<div></div>"
                ]
        (conn, _, inp) <- dummyConnection input

        callbackResults :: MVar (Seq.Seq [Header]) <- newMVar mempty
        let onEarlyHintHeader h = modifyMVar_ callbackResults (return . (Seq.|> h))

        statusHeaders <- parseStatusHeaders Nothing Nothing conn Nothing onEarlyHintHeader Nothing
        statusHeaders `shouldBe` StatusHeaders status200 (HttpVersion 1 1)
            [("Link", "</foo.js>")
            , ("Link", "</bar.js>")
            ]
            [("Content-Type", "text/html")
            ]

        inp >>= (`shouldBe` ["<div></div>"])

        readMVar callbackResults
          >>= (`shouldBe` Seq.fromList [
                  [("Link", "</foo.js>")
                  , ("Link", "</bar.js>")
                  ]])

    it "103 early hints (multiple sections)" $ do
        let input =
                [ "HTTP/1.1 103 Early Hints\r\n"
                , "Link: </foo.js>\r\n"
                , "Link: </bar.js>\r\n\r\n"
                , "HTTP/1.1 103 Early Hints\r\n"
                , "Link: </baz.js>\r\n\r\n"
                , "HTTP/1.1 200 OK\r\n"
                , "Content-Type: text/html\r\n\r\n"
                , "<div></div>"
                ]
        (conn, _, inp) <- dummyConnection input

        callbackResults :: MVar (Seq.Seq [Header]) <- newMVar mempty
        let onEarlyHintHeader h = modifyMVar_ callbackResults (return . (Seq.|> h))

        statusHeaders <- parseStatusHeaders Nothing Nothing conn Nothing onEarlyHintHeader Nothing
        statusHeaders `shouldBe` StatusHeaders status200 (HttpVersion 1 1)
            [("Link", "</foo.js>")
            , ("Link", "</bar.js>")
            , ("Link", "</baz.js>")
            ]
            [("Content-Type", "text/html")
            ]

        inp >>= (`shouldBe` ["<div></div>"])

        readMVar callbackResults
          >>= (`shouldBe` Seq.fromList [
                  [("Link", "</foo.js>")
                  , ("Link", "</bar.js>")
                  ]
                  , [("Link", "</baz.js>")]
                  ])
