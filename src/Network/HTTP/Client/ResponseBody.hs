{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.ResponseBody where

import Network.HTTP.Client.Connection
import Network.HTTP.Client.Types
import Control.Exception (throwIO, assert)
import Data.ByteString (ByteString, empty, uncons)
import Data.IORef
import qualified Data.ByteString as S
import Control.Monad (unless)

type BodyReader = IO ByteString

makeChunkedReader :: Bool -- ^ send headers
                  -> Connection
                  -> IO BodyReader
makeChunkedReader sendHeaders conn@Connection {..} = do
    icount <- newIORef 0
    return $ go icount
  where
    go icount = do
        count0 <- readIORef icount
        count <-
            if count0 > 0
                then return count0
                else readHeader
        (bs, count') <- sendChunk count
        writeIORef icount count'
        return bs

    sendChunk 0 = return (empty, 0)
    sendChunk remainder = do
        bs <- connectionRead
        case compare remainder $ S.length bs of
            LT -> do
                let (x, y) = S.splitAt remainder bs
                assert (not $ S.null y) $ connectionUnread y
                requireNewline
                return (x, 0)
            EQ -> do
                requireNewline
                return (bs, 0)
            GT -> return (bs, remainder - S.length bs)

    requireNewline = do
        bs <- connectionReadLine conn
        unless (S.null bs) $ throwIO InvalidChunkHeaders

    readHeader = do
        bs <- connectionReadLine conn
        case parseHex bs of
            Nothing -> throwIO InvalidChunkHeaders
            Just hex -> return hex

    parseHex bs0 =
        case uncons bs0 of
            Just (w0, bs')
                | Just i0 <- toI w0 -> Just $ parseHex' i0 bs'
            _ -> Nothing
    parseHex' i bs =
        case uncons bs of
            Just (w, bs)
                | Just i' <- toI w -> parseHex' (i * 16 + i') bs
            _ -> i

    toI w
        | 48 <= w && w <= 57  = Just $ fromIntegral w - 48
        | 65 <= w && w <= 70  = Just $ fromIntegral w - 55
        | 97 <= w && w <= 102 = Just $ fromIntegral w - 87
        | otherwise = Nothing
