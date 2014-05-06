{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.BodySpec where

import Test.Hspec

import Control.Applicative
import Data.IORef
import Network.HTTP.Client
import Network.HTTP.Client.Internal
import qualified Data.ByteString as S
import Codec.Compression.GZip (compress)
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = hspec spec

brComplete :: BodyReader -> IO Bool
brComplete bodyReader = do
  xs <- bodyReader
  return (xs == "")

spec :: Spec
spec = do
    describe "brAddCleanup" $ do
        it "adds a cleanup action" $ do
            ref <- newIORef False
            (conn, _, _) <- dummyConnection ["hello world done"]
            _ <- makeLengthReader 11 conn >>= brAddCleanup (writeIORef ref True) >>= brConsume
            readIORef ref `shouldReturn` True

        it "runs cleanup action at most once" $ do
            ref <- newIORef (0 :: Int)
            (conn, _, _) <- dummyConnection ["hello world done"]
            reader <- makeLengthReader 11 conn >>= brAddCleanup (modifyIORef ref succ)
            _ <- brConsume reader
            _ <- reader
            readIORef ref `shouldReturn` 1

    describe "makeChunkedReader" $ do
        it "chunked, single" $ do
            (conn, _, input) <- dummyConnection
                [ "5\r\nhello\r\n6\r\n world\r\n0\r\nnot consumed"
                ]
            reader <- makeChunkedReader False conn
            body <- brConsume reader
            S.concat body `shouldBe` "hello world"
            input' <- input
            S.concat input' `shouldBe` "not consumed"
            brComplete reader `shouldReturn` True

        it "chunked, pieces" $ do
            (conn, _, input) <- dummyConnection $ map S.singleton $ S.unpack
                "5\r\nhello\r\n6\r\n world\r\n0\r\nnot consumed"
            reader <- makeChunkedReader False conn
            body <- brConsume reader
            S.concat body `shouldBe` "hello world"
            input' <- input
            S.concat input' `shouldBe` "not consumed"
            brComplete reader `shouldReturn` True

        it "chunked, raw" $ do
            (conn, _, input) <- dummyConnection
                [ "5\r\nhello\r\n6\r\n world\r\n0\r\nnot consumed"
                ]
            reader <- makeChunkedReader True conn
            body <- brConsume reader
            S.concat body `shouldBe` "5\r\nhello\r\n6\r\n world\r\n0\r\n"
            input' <- input
            S.concat input' `shouldBe` "not consumed"
            brComplete reader `shouldReturn` True

        it "chunked, pieces, raw" $ do
            (conn, _, input) <- dummyConnection $ map S.singleton $ S.unpack
                "5\r\nhello\r\n6\r\n world\r\n0\r\nnot consumed"
            reader <- makeChunkedReader True conn
            body <- brConsume reader
            S.concat body `shouldBe` "5\r\nhello\r\n6\r\n world\r\n0\r\n"
            input' <- input
            S.concat input' `shouldBe` "not consumed"
            brComplete reader `shouldReturn` True

    describe "makeLengthReader" $ do
        it "length, single" $ do
            (conn, _, input) <- dummyConnection
                [ "hello world done"
                ]
            reader <- makeLengthReader 11 conn
            body <- brConsume reader
            S.concat body `shouldBe` "hello world"
            input' <- input
            S.concat input' `shouldBe` " done"
            brComplete reader `shouldReturn` True

        it "length, pieces" $ do
            (conn, _, input) <- dummyConnection $ map S.singleton $ S.unpack
                "hello world done"
            reader <- makeLengthReader 11 conn
            body <- brConsume reader
            S.concat body `shouldBe` "hello world"
            input' <- input
            S.concat input' `shouldBe` " done"
            brComplete reader `shouldReturn` True

        it "gzip" $ do
            let orig = L.fromChunks $ replicate 5000 "Hello world!"
                origZ = compress orig
            (conn, _, input) <- dummyConnection $ L.toChunks origZ ++ ["ignored"]
            reader' <- makeLengthReader (fromIntegral $ L.length origZ) conn
            reader <- makeGzipReader reader'
            body <- brConsume reader
            L.fromChunks body `shouldBe` orig
            input' <- input
            S.concat input' `shouldBe` "ignored"
            brComplete reader `shouldReturn` True
