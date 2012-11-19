{-# LANGUAGE DeriveDataTypeable #-}
module Network.HTTP.Conduit.Types
    ( Request (..)
    , RequestBody (..)
    , ContentType
    , Proxy (..)
    , HttpException (..)
    , Response (..)
    ) where

import Data.Int (Int64)
import Data.Typeable (Typeable)

import qualified Blaze.ByteString.Builder as Blaze

import qualified Data.Conduit as C

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import qualified Network.HTTP.Types as W
import Network.Socks5 (SocksConf)

import Control.Exception (Exception, SomeException)

import Data.Certificate.X509 (X509)
import Network.TLS (PrivateKey)

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
    , clientCertificates :: [(X509, Maybe PrivateKey)]
    -- ^ SSL client certificates
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
    , responseTimeout :: Maybe Int
    -- ^ Number of microseconds to wait for a response. If @Nothing@, will wait
    -- indefinitely. Default: 5 seconds.
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
    deriving (Show, Read, Eq, Typeable)

data HttpException = StatusCodeException W.Status W.ResponseHeaders
                   | InvalidUrlException String String
                   | TooManyRedirects [Response L.ByteString]  -- ^ List of encountered responses containing redirects in reverse chronological order; including last redirect, which triggered the exception and was not followed.
                   | UnparseableRedirect (Response L.ByteString) -- ^ Response containing unparseable redirect.
                   | TooManyRetries
                   | HttpParserException String
                   | HandshakeFailed
                   | OverlongHeaders
                   | ResponseTimeout
    deriving (Show, Typeable)
instance Exception HttpException

-- | A simple representation of the HTTP response created by 'lbsConsumer'.
data Response body = Response
    { responseStatus :: W.Status
    , responseVersion :: W.HttpVersion
    , responseHeaders :: W.ResponseHeaders
    , responseBody :: body
    }
    deriving (Show, Eq, Typeable)

-- | Since 1.1.2.
instance Functor Response where
    fmap f (Response status v headers body) = Response status v headers (f body)
