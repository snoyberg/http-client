{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.Body
    ( makeChunkedReader
    , makeLengthReader
    , makeGzipReader
    , makeUnlimitedReader
    , brConsume
    , brEmpty
    , brAddCleanup
    , brReadSome
    , brRead
    ) where

import Network.HTTP.Client.Connection
import Network.HTTP.Client.Types
import Control.Exception (throwIO, assert)
import Data.ByteString (ByteString, empty, uncons)
import Data.IORef
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Control.Monad (unless, when)
import qualified Data.Streaming.Zlib as Z

-- ^ Get a single chunk of data from the response body, or an empty
-- bytestring if no more data is available.
--
-- Note that in order to consume the entire request body, you will need to
-- repeatedly call this function until you receive an empty @ByteString@ as a
-- result.
--
-- Since 0.1.0
brRead :: BodyReader -> IO S.ByteString
brRead = id

-- | Continuously call 'brRead', building up a lazy ByteString until a chunk is
-- constructed that is at least as many bytes as requested.
--
-- Since 0.4.20
brReadSome :: BodyReader -> Int -> IO L.ByteString
brReadSome brRead =
    loop id
  where
    loop front rem
        | rem <= 0 = return $ L.fromChunks $ front []
        | otherwise = do
            bs <- brRead
            if S.null bs
                then return $ L.fromChunks $ front []
                else loop (front . (bs:)) (rem - S.length bs)

brEmpty :: BodyReader
brEmpty = return S.empty

brAddCleanup :: IO () -> BodyReader -> BodyReader
brAddCleanup cleanup brRead = do
    bs <- brRead
    when (S.null bs) cleanup
    return bs

-- | Strictly consume all remaining chunks of data from the stream.
--
-- Since 0.1.0
brConsume :: BodyReader -> IO [S.ByteString]
brConsume brRead =
    go id
  where
    go front = do
        x <- brRead
        if S.null x
            then return $ front []
            else go (front . (x:))

makeGzipReader :: BodyReader -> IO BodyReader
makeGzipReader brRead = do
    inf <- Z.initInflate $ Z.WindowBits 31
    istate <- newIORef Nothing
    let goPopper popper = do
            res <- popper
            case res of
                Z.PRNext bs -> do
                    writeIORef istate $ Just popper
                    return bs
                Z.PRDone -> do
                    bs <- Z.flushInflate inf
                    if S.null bs
                        then start
                        else do
                            writeIORef istate Nothing
                            return bs
                Z.PRError e -> throwIO $ HttpZlibException e
        start = do
            bs <- brRead
            if S.null bs
                then return S.empty
                else do
                    popper <- Z.feedInflate inf bs
                    goPopper popper
    return $ do
        state <- readIORef istate
        case state of
            Nothing -> start
            Just popper -> goPopper popper

makeUnlimitedReader :: Connection -> IO BodyReader
makeUnlimitedReader Connection {..} = do
    icomplete <- newIORef False
    return $ do
        bs <- connectionRead
        when (S.null bs) $ writeIORef icomplete True
        return bs

makeLengthReader :: Int -> Connection -> IO BodyReader
makeLengthReader count0 Connection {..} = do
    icount <- newIORef count0
    return $ do
        count <- readIORef icount
        if count <= 0
            then return empty
            else do
                bs <- connectionRead
                when (S.null bs) $ throwIO $ ResponseBodyTooShort (fromIntegral count0) (fromIntegral $ count0 - count)
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

makeChunkedReader :: Bool -- ^ raw
                  -> Connection
                  -> IO BodyReader
makeChunkedReader raw conn@Connection {..} = do
    icount <- newIORef 0
    return $ go icount
  where
    go icount = do
        count0 <- readIORef icount
        (rawCount, count) <-
            if count0 == 0
                then readHeader
                else return (empty, count0)
        if count <= 0
            then do
                writeIORef icount (-1)
                return $ if count /= (-1) && raw then rawCount else empty
            else do
                (bs, count') <- readChunk count
                writeIORef icount count'
                return $ appendHeader rawCount bs

    appendHeader
      | raw = S.append
      | otherwise = flip const

    readChunk 0 = return (empty, 0)
    readChunk remainder = do
        bs <- connectionRead
        when (S.null bs) $ throwIO InvalidChunkHeaders
        case compare remainder $ S.length bs of
            LT -> do
                let (x, y) = S.splitAt remainder bs
                assert (not $ S.null y) $ connectionUnread y
                requireNewline
                done x
            EQ -> do
                requireNewline
                done bs
            GT -> return (bs, remainder - S.length bs)
      where
        done x
          | raw = return (x `S.append` "\r\n", 0)
          | otherwise = return (x, 0)

    requireNewline = do
        bs <- connectionReadLine conn
        unless (S.null bs) $ throwIO InvalidChunkHeaders

    readHeader = do
        bs <- connectionReadLine conn
        case parseHex bs of
            Nothing -> throwIO InvalidChunkHeaders
            Just hex -> return (bs `S.append` "\r\n", hex)

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
