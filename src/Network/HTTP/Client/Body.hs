{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.Body where

import Network.HTTP.Client.Connection
import Network.HTTP.Client.Types
import Control.Exception (throwIO, assert)
import Data.ByteString (ByteString, empty, uncons)
import Data.IORef
import qualified Data.ByteString as S
import Control.Monad (unless)
import qualified Codec.Zlib as Z

data BodyReader = BodyReader
    { brRead :: !(IO ByteString)
    , brComplete :: !(IO Bool)
    }

makeGzipReader :: BodyReader -> IO BodyReader
makeGzipReader br = do
    inf <- Z.initInflate $ Z.WindowBits 31
    istate <- newIORef Nothing
    let goPopper popper = do
            mbs <- popper
            case mbs of
                Just bs -> do
                    writeIORef istate $ Just popper
                    return bs
                Nothing -> do
                    bs <- Z.flushInflate inf
                    if S.null bs
                        then start
                        else do
                            writeIORef istate Nothing
                            return bs
        start = do
            bs <- brRead br
            if S.null bs
                then return S.empty
                else do
                    popper <- Z.feedInflate inf bs
                    goPopper popper
    return BodyReader
        { brRead = do
            state <- readIORef istate
            case state of
                Nothing -> start
                Just popper -> goPopper popper
        , brComplete = brComplete br
        }

makeLengthReader :: Int -> Connection -> IO BodyReader
makeLengthReader count0 Connection {..} = do
    icount <- newIORef count0
    return $! BodyReader
        { brRead = do
            count <- readIORef icount
            if count <= 0
                then return empty
                else do
                    bs <- connectionRead
                    case compare count $ S.length bs of
                        LT -> do
                            let (x, y) = S.splitAt count bs
                            connectionUnread y
                            writeIORef icount (-1)
                            return x
                        EQ -> do
                            writeIORef icount (-1)
                            return bs
                        GT -> do
                            writeIORef icount (count - S.length bs)
                            return bs
        , brComplete = fmap (== -1) $ readIORef icount
        }

makeChunkedReader :: Bool -- ^ send headers
                  -> Connection
                  -> IO BodyReader
makeChunkedReader sendHeaders conn@Connection {..} = do
    icount <- newIORef 0
    return $! BodyReader
        { brRead = go icount
        , brComplete = do
            count <- readIORef icount
            return $! count == -1
        }
  where
    go icount = do
        count0 <- readIORef icount
        count <-
            if count0 == 0
                then readHeader
                else return count0
        if count <= 0
            then do
                writeIORef icount (-1)
                return empty
            else do
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
