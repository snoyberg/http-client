{-# LANGUAGE FlexibleContexts #-}
module Network.HTTP.Conduit.Chunk
    ( chunkedConduit
    , chunkIt
    ) where

import Numeric (showHex)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

import Blaze.ByteString.Builder.HTTP
import qualified Blaze.ByteString.Builder as Blaze

import Data.Conduit hiding (Source, Sink, Conduit)
import qualified Data.Conduit.Binary as CB

import Control.Monad (when, unless)
import Control.Exception (assert)

chunkedConduit :: MonadThrow m
               => Bool -- ^ send the headers as well, necessary for a proxy
               -> Pipe S.ByteString S.ByteString S.ByteString u m ()
chunkedConduit sendHeaders = do
    i <- getLen
    when sendHeaders $ yield $ S8.pack $ showHex i "\r\n"
    unless (i == 0) $ do
        CB.isolate i
        CB.drop 2
        chunkedConduit sendHeaders

getLen :: Monad m => Pipe S.ByteString S.ByteString o u m Int
getLen =
    start 0
  where
    start i = await >>= maybe (return i) (go i)

    go i bs =
        case S.uncons bs of
            Nothing -> start i
            Just (w, bs') ->
                case toI w of
                    Just i' -> go (i * 16 + i') bs'
                    Nothing -> do
                        stripNewLine bs
                        return i

    stripNewLine bs =
        case S.uncons $ S.dropWhile (/= 10) bs of
            Just (10, bs') -> leftover bs'
            Just _ -> assert False $ await >>= maybe (return ()) stripNewLine
            Nothing -> await >>= maybe (return ()) stripNewLine

    toI w
        | 48 <= w && w <= 57  = Just $ fromIntegral w - 48
        | 65 <= w && w <= 70  = Just $ fromIntegral w - 55
        | 97 <= w && w <= 102 = Just $ fromIntegral w - 87
        | otherwise = Nothing

chunkIt :: Monad m => Pipe l Blaze.Builder Blaze.Builder r m r
chunkIt =
    awaitE >>= either
        (\u -> yield chunkedTransferTerminator >> return u)
        (\x -> yield (chunkedTransferEncoding x) >> chunkIt)
