{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Conduit.Request
    ( Request (..)
    , RequestBody (..)
    , ContentType
    , Proxy (..)
    , parseUrl
    , setUriRelative
    , browserDecompress
    , HttpException (..)
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
import Network.Socks5 (SocksConf)
import Network.URI (URI (..), URIAuth (..), parseURI, relativeTo, escapeURIString, isAllowedInURI)

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
--
-- For example, to construct a POST request, you could do something like:
--
-- > initReq <- parseUrl "http://www.example.com/path"
-- > let req = initReq
-- >             { method = "POST"
-- >             }
--
-- For more information, please see
-- <http://www.yesodweb.com/book/settings-types>.
data Request m = Request
    { method :: W.Method
    -- ^ HTTP request method, eg GET, POST.
    , secure :: Bool
    -- ^ Whether to use HTTPS (ie, SSL).
    , host :: S.ByteString
    , port :: Int
    , path :: S.ByteString
    -- ^ Everything from the host to the query string.
    , queryString :: S.ByteString
    , requestHeaders :: W.RequestHeaders
    -- ^ Custom HTTP request headers
    --
    -- As already stated in the introduction, the Content-Length and Host
    -- headers are set automatically by this module, and shall not be added to
    -- requestHeaders.
    --
    -- Moreover, the Accept-Encoding header is set implicitly to gzip for
    -- convenience by default. This behaviour can be overridden if needed, by
    -- setting the header explicitly to a different value. In order to omit the
    -- Accept-Header altogether, set it to the empty string \"\". If you need an
    -- empty Accept-Header (i.e. requesting the identity encoding), set it to a
    -- non-empty white-space string, e.g. \" \". See RFC 2616 section 14.3 for
    -- details about the semantics of the Accept-Header field. If you request a
    -- content-encoding not supported by this module, you will have to decode
    -- it yourself (see also the 'decompress' field).
    --
    -- Note: Multiple header fields with the same field-name will result in
    -- multiple header fields being sent and therefore it\'s the responsibility
    -- of the client code to ensure that the rules from RFC 2616 section 4.2
    -- are honoured.
    , requestBody :: RequestBody m
    , proxy :: Maybe Proxy
    -- ^ Optional HTTP proxy.
    , socksProxy :: Maybe SocksConf
    -- ^ Optional SOCKS proxy.
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
    { proxyHost :: S.ByteString -- ^ The host name of the HTTP proxy.
    , proxyPort :: Int -- ^ The port number of the HTTP proxy.
    }

-- | Convert a URL into a 'Request'.
--
-- This defaults some of the values in 'Request', such as setting 'method' to
-- GET and 'requestHeaders' to @[]@.
--
-- Since this function uses 'Failure', the return monad can be anything that is
-- an instance of 'Failure', such as 'IO' or 'Maybe'.
parseUrl :: Failure HttpException m => String -> m (Request m')
parseUrl s =
    case parseURI (encode s) of
        Just uri -> setUri def uri
        Nothing  -> failure $ InvalidUrlException s "Invalid URL"
  where
    encode = escapeURIString isAllowedInURI . encodeString

-- | Add a 'URI' to the request. If it is absolute (includes a host name), add
-- it as per 'setUri'; if it is relative, merge it with the existing request.
setUriRelative :: Failure HttpException m => Request m' -> URI -> m (Request m')
setUriRelative req uri =
    case uri `relativeTo` getUri req of
        Just uri' -> setUri req uri'
        Nothing   -> failure $ InvalidUrlException (show uri) "Invalid URL"

-- | Extract a 'URI' from the request.
getUri :: Request m' -> URI
getUri req = URI
    { uriScheme = if secure req
                    then "https:"
                    else "http:"
    , uriAuthority = Just URIAuth
        { uriUserInfo = ""
        , uriRegName = S8.unpack $ host req
        , uriPort = ':' : show (port req)
        }
    , uriPath = S8.unpack $ path req
    , uriQuery = S8.unpack $ queryString req
    , uriFragment = ""
    }

-- | Validate a 'URI', then add it to the request.
setUri :: Failure HttpException m => Request m' -> URI -> m (Request m')
setUri req uri = do
    sec <- parseScheme uri
    auth <- maybe (failUri "URL must be absolute") return $ uriAuthority uri
    if not . null $ uriUserInfo auth
        then failUri "URL auth not supported; use applyBasicAuth instead"
        else return ()
    port' <- parsePort sec auth
    return req
        { host = S8.pack $ uriRegName auth
        , port = port'
        , secure = sec
        , path = S8.pack $
                    if null $ uriPath uri
                        then "/"
                        else uriPath uri
        , queryString = S8.pack $ uriQuery uri
        }
  where
    failUri = failure . InvalidUrlException (show uri)

    parseScheme URI{uriScheme = scheme} =
        case scheme of
            "http:"  -> return False
            "https:" -> return True
            _        -> failUri "Invalid scheme"

    parsePort sec URIAuth{uriPort = portStr} =
        case portStr of
            -- If the user specifies a port, then use it
            ':':rest -> maybe
                (failUri "Invalid port")
                return
                (readDec rest)
            -- Otherwise, use the default port
            _ -> case sec of
                    False {- HTTP -} -> return 80
                    True {- HTTPS -} -> return 443

instance Default (Request m) where
    def = Request
        { host = "localhost"
        , port = 80
        , secure = False
        , requestHeaders = []
        , path = "/"
        , queryString = S8.empty
        , requestBody = RequestBodyLBS L.empty
        , method = "GET"
        , proxy = Nothing
        , socksProxy = Nothing
        , rawBody = False
        , decompress = browserDecompress
        , redirectCount = 10
        , checkStatus = \s@(W.Status sci _) hs ->
            if 200 <= sci && sci < 300
                then Nothing
                else Just $ toException $ StatusCodeException s hs
        }

data HttpException = StatusCodeException W.Status W.ResponseHeaders
                   | InvalidUrlException String String
                   | TooManyRedirects
                   | UnparseableRedirect
                   | TooManyRetries
                   | HttpParserException String
                   | HandshakeFailed
                   | OverlongHeaders
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
    :: Monad m
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

    acceptEncodingHeader =
        case lookup "Accept-Encoding" $ requestHeaders req of
            Nothing -> (("Accept-Encoding", "gzip"):)
            Just "" -> filter (\(k, _) -> k /= "Accept-Encoding")
            Just _ -> id

    hostHeader = (("Host", hh):)

    headerPairs :: W.RequestHeaders
    headerPairs = hostHeader
                $ acceptEncodingHeader
                $ contentLengthHeader contentLength
                $ requestHeaders req

    builder :: Builder
    builder =
            fromByteString (method req)
            <> fromByteString " "
            <> (case S8.uncons $ path req of
                    Just ('/', _) -> fromByteString $ path req
                    _ -> fromChar '/' <> fromByteString (path req))
            <> (case S8.uncons $ queryString req of
                    Nothing -> mempty
                    Just ('?', _) -> fromByteString $ queryString req
                    _ -> fromChar '?' <> fromByteString (queryString req))
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
