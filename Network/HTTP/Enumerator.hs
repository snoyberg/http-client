{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.HTTP.Enumerator
    ( Request (..)
    , Response (..)
    , http
    , parseUrl
    , httpLbs
    , simpleHttp
    , withHttpEnumerator
    ) where

import qualified OpenSSL.Session as SSL
import Network.Socket
import qualified Network.Socket.ByteString as B
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import Data.Enumerator hiding (head, map, break)
import qualified Data.Enumerator as E
import Network.HTTP.Enumerator.HttpParser
import Control.Exception (throwIO, Exception)
import Control.Arrow (first)
import Data.Char (toLower)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Failure
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Data.Bits
import Data.Maybe (fromMaybe)
import OpenSSL

-- | The OpenSSL library requires some initialization of variables to be used,
-- and therefore you must call 'withOpenSSL' before using any of its functions.
-- As this library uses OpenSSL, you must use 'withOpenSSL' as well. (As a side
-- note, you'll also want to use the withSocketsDo function for network
-- activity.)
--
-- To future-proof this package against switching to different SSL libraries,
-- we re-export 'withOpenSSL' under this name. You can call this function as
-- early as you like; in fact, simply wrapping the do block of your main
-- function is probably best.
withHttpEnumerator :: IO a -> IO a
withHttpEnumerator = withOpenSSL

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
    deriving Show

data Response a = Response
    { statusCode :: Int
    , responseHeaders :: [(S.ByteString, S.ByteString)]
    , responseBody :: a
    }

http :: Request -> Iteratee S.ByteString IO a -> IO (Response a)
http req@(Request {..}) bodyIter = do
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
            ((_, sc, _), hs) <- iterHeaders
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

data InvalidUrlException = InvalidUrlException String String
    deriving (Show, Typeable)
instance Exception InvalidUrlException

parseUrl :: Failure InvalidUrlException m => String -> m Request
parseUrl s@('h':'t':'t':'p':':':'/':'/':rest) = parseUrl1 s False rest
parseUrl s@('h':'t':'t':'p':'s':':':'/':'/':rest) = parseUrl1 s True rest
parseUrl x = failure $ InvalidUrlException x "Invalid scheme"

parseUrl1 :: Failure InvalidUrlException m
          => String -> Bool -> String -> m Request
parseUrl1 full sec s = do
    port' <- mport
    return Request
        { host = S8.pack hostname -- FIXME check chars
        , port = port'
        , secure = sec
        , requestHeaders = []
        , path = S8.pack $ if null path' then "/" else path' -- FIXME check chars
        , queryString = parseQueryString $ S8.pack qstring -- FIXME check chars
        , requestBody = L.empty
        , method = "GET"
        }
  where
    (beforeSlash, afterSlash) = break (== '/') s
    (hostname, portStr) = break (== ':') beforeSlash
    (path', qstring') = break (== '?') afterSlash
    qstring'' = case qstring' of
                '?':x -> x
                _ -> qstring'
    qstring = takeWhile (/= '#') qstring''
    mport =
        case (portStr, sec) of
            ("", False) -> return 80
            ("", True) -> return 443
            (':':rest, _) ->
                case readMay rest of
                    Just i -> return i
                    Nothing -> failure $ InvalidUrlException full "Invalid port"
            x -> error $ "parseUrl1: this should never happen: " ++ show x

parseQueryString :: S.ByteString -> [(S.ByteString, S.ByteString)]
parseQueryString = parseQueryString' . dropQuestion
  where
    dropQuestion q | S.null q || S.head q /= 63 = q
    dropQuestion q | otherwise = S.tail q
    parseQueryString' q | S.null q = []
    parseQueryString' q =
        let (x, xs) = breakDiscard 38 q -- ampersand
         in parsePair x : parseQueryString' xs
      where
        parsePair x =
            let (k, v) = breakDiscard 61 x -- equal sign
             in (qsDecode k, qsDecode v)


qsDecode :: S.ByteString -> S.ByteString
qsDecode z = fst $ S.unfoldrN (S.length z) go z
  where
    go bs =
        case uncons bs of
            Nothing -> Nothing
            Just (43, ws) -> Just (32, ws) -- plus to space
            Just (37, ws) -> Just $ fromMaybe (37, ws) $ do -- percent
                (x, xs) <- uncons ws
                x' <- hexVal x
                (y, ys) <- uncons xs
                y' <- hexVal y
                Just $ (combine x' y', ys)
            Just (w, ws) -> Just (w, ws)
    hexVal w
        | 48 <= w && w <= 57  = Just $ w - 48 -- 0 - 9
        | 65 <= w && w <= 70  = Just $ w - 55 -- A - F
        | 97 <= w && w <= 102 = Just $ w - 87 -- a - f
        | otherwise = Nothing
    combine :: Word8 -> Word8 -> Word8
    combine a b = shiftL a 4 .|. b

uncons :: S.ByteString -> Maybe (Word8, S.ByteString)
uncons s
    | S.null s = Nothing
    | otherwise = Just (S.head s, S.tail s)

breakDiscard :: Word8 -> S.ByteString -> (S.ByteString, S.ByteString)
breakDiscard w s =
    let (x, y) = S.break (== w) s
     in (x, S.drop 1 y)

httpLbs :: Request -> IO (Response L.ByteString)
httpLbs = flip http (L.fromChunks `fmap` consume)

simpleHttp :: String -> IO (Response L.ByteString)
simpleHttp url = parseUrl url >>= httpLbs

readMay :: Read a => String -> Maybe a
readMay s = case reads s of
                [] -> Nothing
                (x, _):_ -> Just x
