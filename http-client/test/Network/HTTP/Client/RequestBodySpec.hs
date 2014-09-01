module Network.HTTP.Client.RequestBodySpec (spec) where

import Network.HTTP.Client
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Blaze.ByteString.Builder (toLazyByteString)
import Data.IORef

consumeBody :: RequestBody -> IO L.ByteString
consumeBody (RequestBodyLBS lbs) = return lbs
consumeBody (RequestBodyBS bs) = return $ L.fromChunks [bs]
consumeBody (RequestBodyBuilder _ b) = return $ toLazyByteString b
consumeBody (RequestBodyStream _ x) = consumeBody $ RequestBodyStreamChunked x
consumeBody (RequestBodyStreamChunked f) = do
    ref <- newIORef undefined
    f $ \popper -> do
        let loop front = do
                bs <- popper
                if S.null bs
                    then writeIORef ref $ L.fromChunks $ front []
                    else loop $ front . (bs:)
        loop id
    readIORef ref

-- Purposely designed to only be callable once!
stream :: [S.ByteString] -> IO RequestBody
stream bss0 = do
    ref <- newIORef bss0
    return $ RequestBodyStreamChunked $ \needs -> needs $ do
        atomicModifyIORef ref $ \bss ->
            case bss of
                [] -> ([], S.empty)
                bs:bss' -> (bss', bs)

spec :: Spec
spec = do
    prop "consumeBody works" $ \wss -> do
        let bss = map S.pack $ filter (not . null) wss
        body <- stream bss
        lbs <- consumeBody body
        lbs `shouldBe` L.fromChunks bss

        lbs2 <- consumeBody body
        lbs2 `shouldBe` L.empty
    prop "cacheRequestBody" $ \wss i -> do
        let bss = map S.pack $ filter (not . null) wss
        body <- stream bss >>= cacheRequestBody (abs i `mod` 5)

        lbs <- consumeBody body
        lbs `shouldBe` L.fromChunks bss

        lbs2 <- consumeBody body
        lbs2 `shouldBe` L.fromChunks bss

        lbs3 <- consumeBody body
        lbs3 `shouldBe` L.fromChunks bss
