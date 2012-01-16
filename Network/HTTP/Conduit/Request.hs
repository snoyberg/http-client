{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Conduit.Request
    ( Request (..)
    , RequestBody (..)
    , ContentType
    , Proxy (..)
    , parseUrl
    , browserDecompress
    , HttpException (..)
    , defaultCheckCerts
    , alwaysDecompress
    , addProxy
    , applyBasicAuth
    , urlEncodedBody
    , needsGunzip
    , requestBuilder
    ) where

import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty, mappend)
import Data.Typeable (Typeable)

import Data.Default (Default (def))

import Blaze.ByteString.Builder (Builder, fromByteString, fromLazyByteString)
import Blaze.ByteString.Builder.Char8 (fromChar)
import qualified Blaze.ByteString.Builder as Blaze

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L

import qualified Network.HTTP.Types as W
import Data.Certificate.X509 (X509)

import Network.TLS (TLSCertificateUsage (CertificateUsageAccept))
import Network.TLS.Extra (certificateVerifyChain, certificateVerifyDomain)

import Control.Exception (Exception, SomeException, toException)
import Control.Failure (Failure (failure))
import Codec.Binary.UTF8.String (encodeString)
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Base64 as B64

import Network.HTTP.Conduit.Chunk (chunkIt)
import Network.HTTP.Conduit.Util (readDec, (<>))

type ContentType = S.ByteString

-- | All information on how to connect to a host and what should be sent in the
-- HTTP request.
--
-- If you simply wish to download from a URL, see 'parseUrl'.
--
-- The constructor for this data type is not exposed. Instead, you should use
-- either the 'def' method to retrieve a default instance, or 'parseUrl' to
-- construct from a URL, and then use the records below to make modifications.
-- This approach allows http-conduit to add configuration options without
-- breaking backwards compatibility.
data Request m = Request
    { method :: W.Method
    -- ^ HTTP request method, eg GET, POST.
    , secure :: Bool
    -- ^ Whether to use HTTPS (ie, SSL).
    , checkCerts :: W.Ascii -> [X509] -> IO TLSCertificateUsage
    -- ^ Check if the server certificate is valid. Only relevant for HTTPS.
    , host :: W.Ascii
    , port :: Int
    , path :: W.Ascii
    -- ^ Everything from the host to the query string.
    , queryString :: W.Ascii
    , requestHeaders :: W.RequestHeaders
    , requestBody :: RequestBody m
    , proxy :: Maybe Proxy
    -- ^ Optional HTTP proxy.
    , rawBody :: Bool
    -- ^ If @True@, a chunked and\/or gzipped body will not be
    -- decoded. Use with caution.
    , decompress :: ContentType -> Bool
    -- ^ Predicate to specify whether gzipped data should be
    -- decompressed on the fly (see 'alwaysDecompress' and
    -- 'browserDecompress'). Default: browserDecompress.
    , redirectCount :: Int
    -- ^ How many redirects to follow when getting a resource. 0 means follow
    -- no redirects. Default value: 10.
    , checkStatus :: W.Status -> W.ResponseHeaders -> Maybe SomeException
    -- ^ Check the status code. Note that this will run after all redirects are
    -- performed. Default: return a @StatusCodeException@ on non-2XX responses.
    }

-- | When using one of the
-- 'RequestBodySource' \/ 'RequestBodySourceChunked' constructors,
-- you must ensure
-- that the 'Source' can be called multiple times.  Usually this
-- is not a problem.
--
-- The 'RequestBodySourceChunked' will send a chunked request
-- body, note that not all servers support this. Only use
-- 'RequestBodySourceChunked' if you know the server you're
-- sending to supports chunked request bodies.
data RequestBody m
    = RequestBodyLBS L.ByteString
    | RequestBodyBS S.ByteString
    | RequestBodyBuilder Int64 Blaze.Builder
    | RequestBodySource Int64 (C.Source m Blaze.Builder)
    | RequestBodySourceChunked (C.Source m Blaze.Builder)

-- | Define a HTTP proxy, consisting of a hostname and port number.

data Proxy = Proxy
    { proxyHost :: W.Ascii -- ^ The host name of the HTTP proxy.
    , proxyPort :: Int -- ^ The port number of the HTTP proxy.
    }

encodeUrlCharPI :: Char -> String
encodeUrlCharPI '/' = "/"
encodeUrlCharPI c = encodeUrlChar c

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
encodeUrlChar y =
    let (a, c) = fromEnum y `divMod` 16
        b = a `mod` 16
        showHex' x
            | x < 10 = toEnum $ x + (fromEnum '0')
            | x < 16 = toEnum $ x - 10 + (fromEnum 'A')
            | otherwise = error $ "Invalid argument to showHex: " ++ show x
     in ['%', showHex' b, showHex' c]

-- | Convert a URL into a 'Request'.
--
-- This defaults some of the values in 'Request', such as setting 'method' to
-- GET and 'requestHeaders' to @[]@.
--
-- Since this function uses 'Failure', the return monad can be anything that is
-- an instance of 'Failure', such as 'IO' or 'Maybe'.
parseUrl :: Failure HttpException m => String -> m (Request m')
parseUrl s@('h':'t':'t':'p':':':'/':'/':rest) = parseUrl1 s False rest
parseUrl s@('h':'t':'t':'p':'s':':':'/':'/':rest) = parseUrl1 s True rest
parseUrl x = failure $ InvalidUrlException x "Invalid scheme"

parseUrl1 :: Failure HttpException m
          => String -> Bool -> String -> m (Request m')
parseUrl1 full sec s =
    parseUrl2 full sec s'
  where
    s' = encodeString s

defaultCheckCerts :: W.Ascii -> [X509] -> IO TLSCertificateUsage
defaultCheckCerts host' certs =
    case certificateVerifyDomain (S8.unpack host') certs of
        CertificateUsageAccept -> certificateVerifyChain certs
        _                          -> return CertificateUsageAccept

instance Default (Request m) where
    def = Request
        { host = "localhost"
        , port = 80
        , secure = False
        , checkCerts = defaultCheckCerts
        , requestHeaders = []
        , path = "/"
        , queryString = S8.empty
        , requestBody = RequestBodyLBS L.empty
        , method = "GET"
        , proxy = Nothing
        , rawBody = False
        , decompress = browserDecompress
        , redirectCount = 10
        , checkStatus = \s@(W.Status sci _) hs ->
            if 200 <= sci && sci < 300
                then Nothing
                else Just $ toException $ StatusCodeException s hs
        }

parseUrl2 :: Failure HttpException m
          => String -> Bool -> String -> m (Request m')
parseUrl2 full sec s = do
    port' <- mport
    return def
        { host = S8.pack hostname
        , port = port'
        , secure = sec
        , path = S8.pack
                    $ if null path''
                            then "/"
                            else concatMap encodeUrlCharPI path''
        , queryString = S8.pack qstring
        }
  where
    (beforeSlash, afterSlash) = break (== '/') s
    (hostname, portStr) = break (== ':') beforeSlash
    (path', qstring') = break (== '?') afterSlash
    path'' = path'
    qstring'' = case qstring' of
                '?':x -> x
                _ -> qstring'
    qstring = takeWhile (/= '#') qstring''
    mport =
        case (portStr, sec) of
            ("", False) -> return 80
            ("", True) -> return 443
            (':':rest, _) -> maybe
                (failure $ InvalidUrlException full "Invalid port")
                return
                (readDec rest)
            x -> error $ "parseUrl1: this should never happen: " ++ show x

data HttpException = StatusCodeException W.Status W.ResponseHeaders
                   | InvalidUrlException String String
                   | TooManyRedirects
                   | HttpParserException String
    deriving (Show, Typeable)
instance Exception HttpException

-- | Always decompress a compressed stream.
alwaysDecompress :: ContentType -> Bool
alwaysDecompress = const True

-- | Decompress a compressed stream unless the content-type is 'application/x-tar'.
browserDecompress :: ContentType -> Bool
browserDecompress = (/= "application/x-tar")

-- | Add a Basic Auth header (with the specified user name and password) to the
-- given Request. Ignore error handling:
--
--    applyBasicAuth "user" "pass" $ fromJust $ parseUrl url

applyBasicAuth :: S.ByteString -> S.ByteString -> Request m -> Request m
applyBasicAuth user passwd req =
    req { requestHeaders = authHeader : requestHeaders req }
  where
    authHeader = (CI.mk "Authorization", basic)
    basic = S8.append "Basic " (B64.encode $ S8.concat [ user, ":", passwd ])


-- | Add a proxy to the the Request so that the Request when executed will use
-- the provided proxy.
addProxy :: S.ByteString -> Int -> Request m -> Request m
addProxy hst prt req =
    req { proxy = Just $ Proxy hst prt }

-- FIXME add a helper for generating POST bodies

-- | Add url-encoded paramters to the 'Request'.
--
-- This sets a new 'requestBody', adds a content-type request header and
-- changes the 'method' to POST.
urlEncodedBody :: Monad m => [(S.ByteString, S.ByteString)] -> Request m' -> Request m
urlEncodedBody headers req = req
    { requestBody = RequestBodyLBS body
    , method = "POST"
    , requestHeaders =
        (ct, "application/x-www-form-urlencoded")
      : filter (\(x, _) -> x /= ct) (requestHeaders req)
    }
  where
    ct = "Content-Type"
    body = L.fromChunks . return $ W.renderSimpleQuery False headers

needsGunzip :: Request m
            -> [W.Header] -- ^ response headers
            -> Bool
needsGunzip req hs' =
        not (rawBody req)
     && ("content-encoding", "gzip") `elem` hs'
     && decompress req (fromMaybe "" $ lookup "content-type" hs')

requestBuilder
    :: C.Resource m
    => Request m
    -> C.Source m Builder
requestBuilder req =
    CL.sourceList [builder] `mappend` bodySource
  where
    sourceSingle = CL.sourceList . return

    (contentLength, bodySource) =
        case requestBody req of
            RequestBodyLBS lbs -> (Just $ L.length lbs, sourceSingle $ fromLazyByteString lbs)
            RequestBodyBS bs -> (Just $ fromIntegral $ S.length bs, sourceSingle $ fromByteString bs)
            RequestBodyBuilder i b -> (Just $ i, sourceSingle b)
            RequestBodySource i source -> (Just i, source)
            RequestBodySourceChunked source -> (Nothing, source C.$= chunkIt)

    hh
        | port req == 80 && not (secure req) = host req
        | port req == 443 && secure req = host req
        | otherwise = host req <> S8.pack (':' : show (port req))

    contentLengthHeader (Just contentLength') =
            if method req `elem` ["GET", "HEAD"] && contentLength' == 0
                then id
                else (:) ("Content-Length", S8.pack $ show contentLength')
    contentLengthHeader Nothing = (:) ("Transfer-Encoding", "chunked")

    headerPairs :: W.RequestHeaders
    headerPairs
        = ("Host", hh)
        : ("Accept-Encoding", "gzip")
        : (contentLengthHeader contentLength)
          (requestHeaders req)

    builder :: Builder
    builder =
            fromByteString (method req)
            <> fromByteString " "
            <> (case proxy req of
                    Just{} ->
                        fromByteString (if secure req then "https://" else "http://")
                        <> fromByteString hh
                    Nothing -> mempty)
            <> (case S8.uncons $ path req of
                    Just ('/', _) -> fromByteString $ path req
                    _ -> fromByteString "/" <> fromByteString (path req))
            <> (if S8.null (queryString req)
                        then mempty
                        else fromChar '?' <> fromByteString (queryString req))
            <> fromByteString " HTTP/1.1\r\n"
            <> foldr
                (\a b -> headerPairToBuilder a <> b)
                (fromByteString "\r\n")
                headerPairs

    headerPairToBuilder (k, v) =
           fromByteString (CI.original k)
        <> fromByteString ": "
        <> fromByteString v
        <> fromByteString "\r\n"
