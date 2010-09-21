{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.HTTP.Enumerator
    ( Request (..)
    , http
    ) where

import qualified OpenSSL.Session as SSL
import Network.Socket
import qualified Network.Socket.ByteString as B
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Enumerator hiding (head, map)
import qualified Data.Enumerator as E
import Http
import Safe
import Control.Exception (throwIO)
import Control.Arrow (first)
import Data.Char (toLower)
import Control.Monad (forM_)

getSocket :: String -> Int -> IO Socket
getSocket host' port' = do
    addrs <- getAddrInfo Nothing (Just host') (Just $ show port')
    let addr = head addrs
    sock <- socket (addrFamily addr) Stream defaultProtocol
    connect sock (addrAddress addr)
    return sock

withSocketConn :: String -> Int -> (HttpConn -> IO a) -> IO a
withSocketConn host' port' f = do
    sock <- getSocket host' port'
    a <- f HttpConn
        { hcRead = B.recv sock
        , hcWrite = B.sendAll sock
        }
    sClose sock
    return a

withOpenSslConn :: String -> Int -> (HttpConn -> IO a) -> IO a
withOpenSslConn host' port' f = do
    ctx <- SSL.context
    sock <- getSocket host' port'
    ssl <- SSL.connection ctx sock
    SSL.connect ssl
    a <- f HttpConn
        { hcRead = SSL.read ssl
        , hcWrite = SSL.write ssl
        }
    SSL.shutdown ssl SSL.Unidirectional
    return a

data HttpConn = HttpConn
    { hcRead :: Int -> IO S.ByteString
    , hcWrite :: S.ByteString -> IO ()
    }

connToEnum :: HttpConn -> Enumerator S.ByteString IO a
connToEnum (HttpConn r _) =
    Iteratee . loop
  where
    loop (Continue k) = do
        bs <- r 2 -- FIXME better size
        if S.null bs
            then return $ Continue k
            else do
                runIteratee (k $ Chunks [bs]) >>= loop
    loop step = return step

data Request = Request
    { host :: S.ByteString
    , port :: Int
    , secure :: Bool
    , headers :: [(S.ByteString, S.ByteString)]
    }

http :: Request -> IO ([Header], S.ByteString)
http (Request {..}) = do
    let h' = S8.unpack host
    res <- (if secure then withOpenSslConn else withSocketConn) h' port go
    case res of
        Left e -> throwIO e
        Right x -> return x
  where
    hh
        | port == 80 && not secure = host
        | port == 443 && secure = host
        | otherwise = host `S.append` S8.pack (':' : show port)
    go hc = do
        hcWrite hc "GET / HTTP/1.1\r\n"
        let headers' = ("Host", hh) : headers
        forM_ headers' $ \(k, v) -> hcWrite hc $ S.concat
            [ k
            , ": "
            , v
            , "\r\n"
            ]
        hcWrite hc "\r\n"
        run $ connToEnum hc $$ do
            (_FIXMEstatus, hs) <- iterHeaders
            let hs' = map (first $ S8.map toLower) hs -- FIXME use wai CIByteString?
            let mcl = lookup "content-length" hs'
            body <-
                if ("transfer-encoding", "chunked") `elem` hs'
                    then iterChunks
                    else case mcl >>= readMay . S8.unpack of
                        Just len -> takeLBS len
                        Nothing -> return [] -- FIXME read in body anyways?
            return (hs, S.concat body)

takeLBS :: Monad m => Int -> Iteratee S.ByteString m [S.ByteString]
takeLBS 0 = return []
takeLBS len = do
    mbs <- E.head
    case mbs of
        Nothing -> return []
        Just bs -> do
            let len' = len - S.length bs
            rest <- takeLBS len'
            return $ bs : rest
