{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.HTTP.Enumerator
    ( Request (..)
    , Response (..)
    , http
    ) where

import qualified OpenSSL.Session as SSL
import Network.Socket
import qualified Network.Socket.ByteString as B
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import Data.Enumerator hiding (head, map)
import qualified Data.Enumerator as E
import Http
import Safe
import Control.Exception (throwIO)
import Control.Arrow (first)
import Data.Char (toLower)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)

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
    , requestHeaders :: [(S.ByteString, S.ByteString)]
    , path :: S.ByteString
    , queryString :: [(S.ByteString, S.ByteString)]
    , requestBody :: L.ByteString
    , method :: S.ByteString
    }

data Response a = Response
    { statusCode :: Int
    , statusMessage :: S.ByteString
    , responseHeaders :: [(S.ByteString, S.ByteString)]
    , responseBody :: a
    }

http :: Request -> Iteratee S.ByteString IO a -> IO (Response a)
http (Request {..}) bodyIter = do
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
        hcWrite hc $ S.concat
            $ method
            : " "
            : path
            : renderQS queryString [" HTTP/1.1\r\n"]
        let headers' = ("Host", hh)
                     : ("Content-Length", S8.pack $ show
                                                  $ L.length requestBody)
                     : requestHeaders
        forM_ headers' $ \(k, v) -> hcWrite hc $ S.concat
            [ k
            , ": "
            , v
            , "\r\n"
            ]
        hcWrite hc "\r\n"
        mapM_ (hcWrite hc) $ L.toChunks requestBody
        run $ connToEnum hc $$ do
            ((_, sc, sm), hs) <- iterHeaders
            let hs' = map (first $ S8.map toLower) hs
            let mcl = lookup "content-length" hs'
            body' <-
                if ("transfer-encoding", "chunked") `elem` hs'
                    then iterChunks
                    else case mcl >>= readMay . S8.unpack of
                        Just len -> takeLBS len
                        Nothing -> return [] -- FIXME read in body anyways?
            ebody'' <- liftIO $ run $ enumList 1 body' $$ bodyIter
            case ebody'' of
                Left err -> liftIO $ throwIO err
                Right body'' ->
                    return $ Response
                        { statusCode = sc
                        , statusMessage = sm
                        , responseHeaders = hs
                        , responseBody = body''
                        }

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

renderQS :: [(S.ByteString, S.ByteString)]
         -> [S.ByteString]
         -> [S.ByteString]
renderQS [] x = x
renderQS (p:ps) x =
    go "?" p ++ concatMap (go "&") ps ++ x
  where
    go sep (k, v) = [sep, escape k, "=", escape v]
    escape = S8.concatMap (S8.pack . encodeUrlChar)

encodeUrlChar :: Char -> String
encodeUrlChar c
    -- List of unreserved characters per RFC 3986
    -- Gleaned from http://en.wikipedia.org/wiki/Percent-encoding
    | 'A' <= c && c <= 'Z' = [c]
    | 'a' <= c && c <= 'z' = [c]
    | '0' <= c && c <= '9' = [c]
encodeUrlChar c@'-' = [c]
encodeUrlChar c@'_' = [c]
encodeUrlChar c@'.' = [c]
encodeUrlChar c@'~' = [c]
encodeUrlChar ' ' = "+"
encodeUrlChar y =
    let (a, c) = fromEnum y `divMod` 16
        b = a `mod` 16
        showHex' x -- FIXME just use Numeric version?
            | x < 10 = toEnum $ x + (fromEnum '0')
            | x < 16 = toEnum $ x - 10 + (fromEnum 'A')
            | otherwise = error $ "Invalid argument to showHex: " ++ show x
     in ['%', showHex' b, showHex' c]
