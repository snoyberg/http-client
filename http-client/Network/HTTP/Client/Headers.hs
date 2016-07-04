{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module Network.HTTP.Client.Headers
    ( parseStatusHeaders
    ) where

import           Control.Applicative            as A ((<$>), (<*>))
import           Control.Monad
import qualified Data.ByteString                as S
import qualified Data.ByteString.Char8          as S8
import qualified Data.CaseInsensitive           as CI
import           Network.HTTP.Client.Connection
import           Network.HTTP.Client.Types
import           Network.HTTP.Client.Util       (timeout)
import           Network.HTTP.Types
import Data.Word (Word8)

charSpace, charColon, charPeriod :: Word8
charSpace = 32
charColon = 58
charPeriod = 46


parseStatusHeaders :: Connection -> Maybe Int -> Maybe (IO ()) -> IO StatusHeaders
parseStatusHeaders conn timeout' cont
    | Just k <- cont = getStatusExpectContinue k
    | otherwise      = getStatus
  where
    withTimeout = case timeout' of
        Nothing -> id
        Just  t -> timeout t >=> maybe (throwHttp ResponseTimeout) return

    getStatus = withTimeout next
      where
        next = nextStatusHeaders >>= maybe next return

    getStatusExpectContinue sendBody = do
        status <- withTimeout nextStatusHeaders
        case status of
            Just  s -> return s
            Nothing -> sendBody >> getStatus

    nextStatusHeaders = do
        (s, v) <- nextStatusLine
        if statusCode s == 100
            then connectionDropTillBlankLine conn >> return Nothing
            else Just . StatusHeaders s v A.<$> parseHeaders (0 :: Int) id

    nextStatusLine :: IO (Status, HttpVersion)
    nextStatusLine = do
        -- Ensure that there is some data coming in. If not, we want to signal
        -- this as a connection problem and not a protocol problem.
        bs <- connectionRead conn
        when (S.null bs) $ throwHttp NoResponseDataReceived
        connectionReadLineWith conn bs >>= parseStatus 3

    parseStatus :: Int -> S.ByteString -> IO (Status, HttpVersion)
    parseStatus i bs | S.null bs && i > 0 = connectionReadLine conn >>= parseStatus (i - 1)
    parseStatus _ bs = do
        let (ver, bs2) = S.break (== charSpace) bs
            (code, bs3) = S.break (== charSpace) $ S.dropWhile (== charSpace) bs2
            msg = S.dropWhile (== charSpace) bs3
        case (,) <$> parseVersion ver A.<*> readInt code of
            Just (ver', code') -> return (Status code' msg, ver')
            Nothing -> throwHttp $ InvalidStatusLine bs

    stripPrefixBS x y
        | x `S.isPrefixOf` y = Just $ S.drop (S.length x) y
        | otherwise = Nothing
    parseVersion bs0 = do
        bs1 <- stripPrefixBS "HTTP/" bs0
        let (num1, S.drop 1 -> num2) = S.break (== charPeriod) bs1
        HttpVersion <$> readInt num1 <*> readInt num2

    readInt bs =
        case S8.readInt bs of
            Just (i, "") -> Just i
            _ -> Nothing

    parseHeaders 100 _ = throwHttp OverlongHeaders
    parseHeaders count front = do
        line <- connectionReadLine conn
        if S.null line
            then return $ front []
            else do
                header <- parseHeader line
                parseHeaders (count + 1) $ front . (header:)

    parseHeader :: S.ByteString -> IO Header
    parseHeader bs = do
        let (key, bs2) = S.break (== charColon) bs
        when (S.null bs2) $ throwHttp $ InvalidHeader bs
        return (CI.mk $! strip key, strip $! S.drop 1 bs2)

    strip = S.dropWhile (== charSpace) . fst . S.spanEnd (== charSpace)
