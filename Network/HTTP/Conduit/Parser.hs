{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.HTTP.Conduit.Parser
    ( sinkHeaders
    ) where

import Prelude hiding (take, takeWhile)
import Control.Applicative

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

import Data.Conduit (Sink, MonadThrow (monadThrow), (=$))
import Control.Monad (when, unless)
import Network.HTTP.Conduit.Types (HttpException (..))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL


type Header = (S.ByteString, S.ByteString)
type Status = (S.ByteString, Int, S.ByteString)

-- | New version of @sinkHeaders@ that doesn't use attoparsec. Should create
-- more meaningful exceptions.
--
-- Since 1.8.7
sinkHeaders :: (MonadThrow m) => Sink S.ByteString m (Status, [Header])
sinkHeaders = do
    status <- getStatusLine
    headers <- parseHeaders id
    return (status, headers)
  where
    getStatusLine = do
        -- Ensure that there is some data coming in. If not, we want to signal
        -- this as a connection problem and not a protocol problem.
        mx <- CL.peek
        case mx of
            Nothing -> monadThrow NoResponseDataReceived
            Just _ -> return ()

        status@(_, code, _) <- sinkLine >>= parseStatus
        if code == 100
            then newline ExpectedBlankAfter100Continue >> getStatusLine
            else return status

    newline exc = do
        line <- sinkLine
        unless (S.null line) $ monadThrow exc

    sinkLine = do
        bs <- fmap (killCR . S.concat) $ CB.takeWhile (/= charLF) =$ CL.consume
        CB.drop 1
        return bs
    charLF = 10
    charCR = 13
    charSpace = 32
    charColon = 58
    killCR bs
        | S.null bs = bs
        | S.last bs == charCR = S.init bs
        | otherwise = bs

    parseStatus :: MonadThrow m => S.ByteString -> m Status
    parseStatus bs = do
        let (ver, bs2) = S.breakByte charSpace bs
            (code, bs3) = S.breakByte charSpace $ S.dropWhile (== charSpace) bs2
            msg = S.dropWhile (== charSpace) bs3
        case (,) <$> parseVersion ver <*> parseCode code of
            Just (ver', code') -> return (ver', code', msg)
            _ -> monadThrow $ InvalidStatusLine bs

    stripPrefixBS x y
        | x `S.isPrefixOf` y = Just $ S.drop (S.length x) y
        | otherwise = Nothing
    parseVersion = stripPrefixBS "HTTP/"
    parseCode bs =
        case S8.readInt bs of
            Just (i, "") -> Just i
            _ -> Nothing

    parseHeaders front = do
        line <- sinkLine
        if S.null line
            then return $ front []
            else do
                header <- parseHeader line
                parseHeaders $ front . (header:)

    parseHeader :: MonadThrow m => S.ByteString -> m Header
    parseHeader bs = do
        let (key, bs2) = S.breakByte charColon bs
        when (S.null bs2) $ monadThrow $ InvalidHeader bs
        return (strip key, strip $ S.drop 1 bs2)

    strip = S.dropWhile (== charSpace) . fst . S.spanEnd (== charSpace)
