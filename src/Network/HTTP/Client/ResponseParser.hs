{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module Network.HTTP.Client.ResponseParser where

import           Control.Applicative            ((<$>), (<*>))
import           Control.Exception              (throwIO)
import           Control.Monad
import qualified Data.ByteString                as S
import qualified Data.ByteString.Char8          as S8
import qualified Data.CaseInsensitive           as CI
import           Network.HTTP.Client.Connection
import           Network.HTTP.Client.Types
import           Network.HTTP.Types

parseStatusHeaders :: Connection -> IO StatusHeaders
parseStatusHeaders Connection {..} = do
    (status, version) <- getStatusLine
    headers <- parseHeaders 0 id
    return $! StatusHeaders status version headers
  where
    getStatusLine = do
        -- Ensure that there is some data coming in. If not, we want to signal
        -- this as a connection problem and not a protocol problem.
        bs <- connectionRead
        when (S.null bs) $ throwIO NoResponseDataReceived

        status@(code, _) <- sinkLineWith bs >>= parseStatus
        if code == status100
            then newline ExpectedBlankAfter100Continue >> getStatusLine
            else return status

    newline exc = do
        line <- sinkLine
        unless (S.null line) $ throwIO exc

    sinkLine = do
        bs <- connectionRead
        when (S.null bs) $ throwIO IncompleteHeaders
        sinkLineWith bs

    sinkLineWith bs0 =
        go bs0 id 0
      where
        go bs front total =
            case S.breakByte charLF bs of
                (_, "") -> do
                    let total' = total + S.length bs
                    when (total' > 1024) $ throwIO OverlongHeaders
                    bs' <- connectionRead
                    when (S.null bs') $ throwIO IncompleteHeaders
                    go bs' (front . (bs:)) total'
                (x, S.drop 1 -> y) -> do
                    unless (S.null y) $! connectionUnread y
                    return $! killCR $! S.concat $! front [x]

    charLF = 10
    charCR = 13
    charSpace = 32
    charColon = 58
    charPeriod = 46
    killCR bs
        | S.null bs = bs
        | S.last bs == charCR = S.init bs
        | otherwise = bs

    parseStatus :: S.ByteString -> IO (Status, HttpVersion)
    parseStatus bs = do
        let (ver, bs2) = S.breakByte charSpace bs
            (code, bs3) = S.breakByte charSpace $ S.dropWhile (== charSpace) bs2
            msg = S.dropWhile (== charSpace) bs3
        case (,) <$> parseVersion ver <*> readInt code of
            Just (ver', code') -> return (Status code' msg, ver')
            Nothing -> throwIO $ InvalidStatusLine bs

    stripPrefixBS x y
        | x `S.isPrefixOf` y = Just $ S.drop (S.length x) y
        | otherwise = Nothing
    parseVersion bs0 = do
        bs1 <- stripPrefixBS "HTTP/" bs0
        let (num1, S.drop 1 -> num2) = S.breakByte charPeriod bs1
        HttpVersion <$> readInt num1 <*> readInt num2

    readInt bs =
        case S8.readInt bs of
            Just (i, "") -> Just i
            _ -> Nothing

    parseHeaders 100 _ = throwIO OverlongHeaders
    parseHeaders count front = do
        line <- sinkLine
        if S.null line
            then return $ front []
            else do
                header <- parseHeader line
                parseHeaders (count + 1) $ front . (header:)

    parseHeader :: S.ByteString -> IO Header
    parseHeader bs = do
        let (key, bs2) = S.breakByte charColon bs
        when (S.null bs2) $ throwIO $ InvalidHeader bs
        return (CI.mk $! strip key, strip $! S.drop 1 bs2)

    strip = S.dropWhile (== charSpace) . fst . S.spanEnd (== charSpace)
