{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.BodySpec where

import Test.Hspec
import Network.HTTP.Client.Body
import Network.HTTP.Client.Connection
import qualified Data.ByteString as S

main :: IO ()
main = hspec spec

consumeReader :: BodyReader -> IO [S.ByteString]
consumeReader f =
    go id
  where
    go front = do
        x <- brRead f
        if S.null x
            then return $ front []
            else go (front . (x:))

spec :: Spec
spec = describe "BodySpec" $ do
    it "chunked, single" $ do
        (conn, _, input) <- dummyConnection
            [ "5\r\nhello\r\n6\r\n world\r\n0\r\nnot consumed"
            ]
        reader <- makeChunkedReader False conn
        complete1 <- brComplete reader
        complete1 `shouldBe` False
        body <- consumeReader reader
        S.concat body `shouldBe` "hello world"
        input' <- input
        S.concat input' `shouldBe` "not consumed"
        complete2 <- brComplete reader
        complete2 `shouldBe` True
    it "chunked, pieces" $ do
        (conn, _, input) <- dummyConnection $ map S.singleton $ S.unpack
            "5\r\nhello\r\n6\r\n world\r\n0\r\nnot consumed"
        reader <- makeChunkedReader False conn
        complete1 <- brComplete reader
        complete1 `shouldBe` False
        body <- consumeReader reader
        S.concat body `shouldBe` "hello world"
        input' <- input
        S.concat input' `shouldBe` "not consumed"
        complete2 <- brComplete reader
        complete2 `shouldBe` True
