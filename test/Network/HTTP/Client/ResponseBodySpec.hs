{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.ResponseBodySpec where

import Test.Hspec
import Network.HTTP.Client.ResponseBody
import Network.HTTP.Client.Connection
import qualified Data.ByteString as S

main :: IO ()
main = hspec spec

consumeReader :: BodyReader -> IO [S.ByteString]
consumeReader f =
    go id
  where
    go front = do
        x <- f
        if S.null x
            then return $ front []
            else go (front . (x:))

spec :: Spec
spec = describe "Network.HTTP.Client.ResponseBodySpec" $ do
    it "chunked, single" $ do
        (conn, _, input) <- dummyConnection
            [ "5\r\nhello\r\n6\r\n world\r\n0\r\nnot consumed"
            ]
        reader <- makeChunkedReader False conn
        body <- consumeReader reader
        S.concat body `shouldBe` "hello world"
        input' <- input
        S.concat input' `shouldBe` "not consumed"
    it "chunked, pieces" $ do
        (conn, _, input) <- dummyConnection $ map S.singleton $ S.unpack
            "5\r\nhello\r\n6\r\n world\r\n0\r\nnot consumed"
        reader <- makeChunkedReader False conn
        body <- consumeReader reader
        S.concat body `shouldBe` "hello world"
        input' <- input
        S.concat input' `shouldBe` "not consumed"