{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Network.HTTP.Client.Headers
    ( parseStatusHeaders
    , validateHeaders
    , HeadersValidationResult (..)
    ) where

import           Control.Applicative            as A ((<$>), (<*>))
import           Control.Monad
import qualified Data.ByteString                as S
import qualified Data.ByteString.Char8          as S8
import qualified Data.CaseInsensitive           as CI
import           Data.Maybe (mapMaybe)
import           Data.Monoid
import           Data.Word (Word8)
import           Network.HTTP.Client.Connection
import           Network.HTTP.Client.Types
import           Network.HTTP.Types
import           System.Timeout                 (timeout)

charSpace, charColon, charPeriod :: Word8
charSpace = 32
charColon = 58
charPeriod = 46


parseStatusHeaders :: Maybe MaxHeaderLength -> Maybe MaxNumberHeaders -> Connection -> Maybe Int -> ([Header] -> IO ()) -> Maybe (IO ()) -> IO StatusHeaders
parseStatusHeaders mhl mnh conn timeout' onEarlyHintHeaders cont
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

    nextStatusHeaders :: IO (Maybe StatusHeaders)
    nextStatusHeaders = do
        (s, v) <- nextStatusLine mhl
        if | statusCode s == 100 -> connectionDropTillBlankLine mhl conn >> return Nothing
           | statusCode s == 103 -> do
                 earlyHeaders <- parseEarlyHintHeadersUntilFailure 0 id
                 onEarlyHintHeaders earlyHeaders
                 nextStatusHeaders >>= \case
                     Nothing -> return Nothing
                     Just (StatusHeaders s' v' earlyHeaders' reqHeaders) ->
                         return $ Just $ StatusHeaders s' v' (earlyHeaders <> earlyHeaders') reqHeaders
           | otherwise -> (Just <$>) $ StatusHeaders s v mempty A.<$> parseHeaders 0 id

    nextStatusLine :: Maybe MaxHeaderLength -> IO (Status, HttpVersion)
    nextStatusLine mhl = do
        -- Ensure that there is some data coming in. If not, we want to signal
        -- this as a connection problem and not a protocol problem.
        bs <- connectionRead conn
        when (S.null bs) $ throwHttp NoResponseDataReceived
        connectionReadLineWith mhl conn bs >>= parseStatus mhl 3

    parseStatus :: Maybe MaxHeaderLength -> Int -> S.ByteString -> IO (Status, HttpVersion)
    parseStatus mhl i bs | S.null bs && i > 0 = connectionReadLine mhl conn >>= parseStatus mhl (i - 1)
    parseStatus _ _ bs = do
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

    guardMaxNumberHeaders :: Int -> IO ()
    guardMaxNumberHeaders count = case fmap unMaxNumberHeaders mnh of
        -- We reached the maximum number of headers, let's throw an error
        Just n | count >= n -> throwHttp OverlongHeaders
        -- We didn't reach the maximum number of headers yet
        Just _ -> pure ()
        -- We do not have any limit on the number of headers
        Nothing -> pure ()

    parseHeaders :: Int -> ([Header] -> [Header]) -> IO [Header]
    parseHeaders count front = do
        guardMaxNumberHeaders count
        line <- connectionReadLine mhl conn
        if S.null line
            then return $ front []
            else
                parseHeader line >>= \case
                    Just header ->
                        parseHeaders (count + 1) $ front . (header:)
                    Nothing ->
                        -- Unparseable header line; rather than throwing
                        -- an exception, ignore it for robustness.
                        parseHeaders count front

    parseEarlyHintHeadersUntilFailure :: Int -> ([Header] -> [Header]) -> IO [Header]
    parseEarlyHintHeadersUntilFailure count front = do
        guardMaxNumberHeaders count
        line <- connectionReadLine mhl conn
        if S.null line
            then return $ front []
            else
                parseHeader line >>= \case
                    Just header ->
                      parseEarlyHintHeadersUntilFailure (count + 1) $ front . (header:)
                    Nothing -> do
                      connectionUnreadLine conn line
                      return $ front []

    parseHeader :: S.ByteString -> IO (Maybe Header)
    parseHeader bs = do
        let (key, bs2) = S.break (== charColon) bs
        if S.null bs2
            then return Nothing
            else return (Just (CI.mk $! strip key, strip $! S.drop 1 bs2))

    strip = S.dropWhile (== charSpace) . fst . S.spanEnd (== charSpace)

data HeadersValidationResult
    = GoodHeaders
    | BadHeaders S.ByteString -- contains a message with the reason

validateHeaders :: RequestHeaders -> HeadersValidationResult
validateHeaders headers =
    case mapMaybe validateHeader headers of
        [] -> GoodHeaders
        reasons -> BadHeaders (S8.unlines reasons)
    where
    validateHeader (k, v)
        | S8.elem '\n' v = Just ("Header " <> CI.original k <> " has newlines")
        | True = Nothing
