{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.HTTP.Conduit.Types
    ( Request (..)
    , RequestBody (..)
    , ContentType
    , Proxy (..)
    , HttpException (..)
    , Response (..)
    , ConnRelease
    , ConnReuse (..)
    , ManagedConn (..)
    , Cookie (..)
    , CookieJar (..)
    ) where

import Data.Int (Int64)
import Data.Typeable (Typeable)

import Blaze.ByteString.Builder

import qualified Data.Conduit as C

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Data.Time.Clock
import Data.Default
import qualified Data.List as DL

import qualified Network.HTTP.Types as W
import qualified Network.Socket as NS
import Network.Socks5 (SocksConf)

import Control.Exception (Exception, SomeException, IOException)

import Data.Certificate.X509 (X509)
import Network.TLS (PrivateKey)
import Network.HTTP.Conduit.ConnInfo (ConnInfo)
import Network.HTTP.Conduit.Util

import Data.Monoid (Monoid(..))

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
    , hostAddress :: Maybe NS.HostAddress
    -- ^ Optional resolved host address.
    --
    -- Since 1.8.9
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
    , checkStatus :: W.Status -> W.ResponseHeaders -> CookieJar -> Maybe SomeException
    -- ^ Check the status code. Note that this will run after all redirects are
    -- performed. Default: return a @StatusCodeException@ on non-2XX responses.
    , responseTimeout :: Maybe Int
    -- ^ Number of microseconds to wait for a response. If @Nothing@, will wait
    -- indefinitely. Default: 5 seconds.
    , getConnectionWrapper :: forall n. (C.MonadResource n, C.MonadBaseControl IO n)
                           => Maybe Int
                           -> HttpException
                           -> n (ConnRelease n, ConnInfo, ManagedConn)
                           -> n (Maybe Int, (ConnRelease n, ConnInfo, ManagedConn))
    -- ^ Wraps the calls for getting new connections. This can be useful for
    -- instituting some kind of timeouts. The first argument is the value of
    -- @responseTimeout@. Second argument is the exception to be thrown on
    -- failure.
    --
    -- Default: If @responseTimeout@ is @Nothing@, does nothing. Otherwise,
    -- institutes timeout, and returns remaining time for @responseTimeout@.
    --
    -- Since 1.8.8
    , cookieJar :: Maybe CookieJar
    -- ^ A user-defined cookie jar.
    -- If 'Nothing', no cookie handling will take place, \"Cookie\" headers
    -- in 'requestHeaders' will be sent raw, and 'responseCookieJar' will be
    -- empty.
    --
    -- Since 1.9.0
    }

data ConnReuse = Reuse | DontReuse

type ConnRelease m = ConnReuse -> m ()

data ManagedConn = Fresh | Reused

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
    | RequestBodyBuilder Int64 Builder
    | RequestBodySource Int64 (C.Source m Builder)
    | RequestBodySourceChunked (C.Source m Builder)

-- | Define a HTTP proxy, consisting of a hostname and port number.

data Proxy = Proxy
    { proxyHost :: S.ByteString -- ^ The host name of the HTTP proxy.
    , proxyPort :: Int -- ^ The port number of the HTTP proxy.
    }
    deriving (Show, Read, Eq, Ord, Typeable)

data HttpException = StatusCodeException W.Status W.ResponseHeaders CookieJar
                   | InvalidUrlException String String
                   | TooManyRedirects [Response L.ByteString]  -- ^ List of encountered responses containing redirects in reverse chronological order; including last redirect, which triggered the exception and was not followed.
                   | UnparseableRedirect (Response L.ByteString) -- ^ Response containing unparseable redirect.
                   | TooManyRetries
                   | HttpParserException String
                   | HandshakeFailed
                   | OverlongHeaders
                   | ResponseTimeout
                   | FailedConnectionException String Int -- ^ host/port
                   | ExpectedBlankAfter100Continue
                   | InvalidStatusLine S.ByteString
                   | InvalidHeader S.ByteString
                   | InternalIOException IOException
                   | ProxyConnectException S.ByteString Int (Either S.ByteString HttpException) -- ^ host/port
                   | NoResponseDataReceived
                   | TlsException SomeException
    deriving (Show, Typeable)
instance Exception HttpException

-- | A simple representation of the HTTP response created by 'lbsConsumer'.
data Response body = Response
    { responseStatus :: W.Status
    -- ^ Status code of the response.
    , responseVersion :: W.HttpVersion
    -- ^ HTTP version used by the server.
    , responseHeaders :: W.ResponseHeaders
    -- ^ Response headers sent by the server.
    , responseBody :: body
    -- ^ Response body sent by the server.
    , responseCookieJar :: CookieJar
    -- ^ Cookies set on the client after interacting with the server. If
    -- cookies have been disabled by setting 'cookieJar' to @Nothing@, then
    -- this will always be empty.
    }
    deriving (Show, Eq, Typeable)

-- This corresponds to the description of a cookie detailed in Section 5.3 \"Storage Model\"
data Cookie = Cookie
  { cookie_name :: S.ByteString
  , cookie_value :: S.ByteString
  , cookie_expiry_time :: UTCTime
  , cookie_domain :: S.ByteString
  , cookie_path :: S.ByteString
  , cookie_creation_time :: UTCTime
  , cookie_last_access_time :: UTCTime
  , cookie_persistent :: Bool
  , cookie_host_only :: Bool
  , cookie_secure_only :: Bool
  , cookie_http_only :: Bool
  }
  deriving (Show)

newtype CookieJar = CJ { expose :: [Cookie] }
  deriving (Show)

-- This corresponds to step 11 of the algorithm described in Section 5.3 \"Storage Model\"
instance Eq Cookie where
  (==) a b = name_matches && domain_matches && path_matches
    where name_matches = cookie_name a == cookie_name b
          domain_matches = cookie_domain a == cookie_domain b
          path_matches = cookie_path a == cookie_path b

instance Ord Cookie where
  compare c1 c2
    | S.length (cookie_path c1) > S.length (cookie_path c2) = LT
    | S.length (cookie_path c1) < S.length (cookie_path c2) = GT
    | cookie_creation_time c1 > cookie_creation_time c2 = GT
    | otherwise = LT

instance Default CookieJar where
  def = CJ []

instance Eq CookieJar where
  (==) cj1 cj2 = (DL.sort $ expose cj1) == (DL.sort $ expose cj2)

-- | Since 1.9
instance Monoid CookieJar where
  mempty = def
  (CJ a) `mappend` (CJ b) = CJ (DL.nub $ DL.sortBy compare' $ a `mappend` b)
    where compare' c1 c2 =
            -- inverse so that recent cookies are kept by nub over older
            if cookie_creation_time c1 > cookie_creation_time c2
                then LT
                else GT

-- | Since 1.1.2.
instance Functor Response where
    fmap f response = response {responseBody = f (responseBody response)}

-- | Since 1.8.7
instance Show (RequestBody m) where
    showsPrec d (RequestBodyBS a) =
        showParen (d>=11) $ showString "RequestBodyBS " . showsPrec 11 a
    showsPrec d (RequestBodyLBS a) =
        showParen (d>=11) $ showString "RequestBodyLBS " . showsPrec 11 a
    showsPrec d (RequestBodyBuilder l _) =
        showParen (d>=11) $ showString "RequestBodyBuilder " . showsPrec 11 l .
            showString " " . showString "<Builder>"
    showsPrec d (RequestBodySource l _) =
        showParen (d>=11) $ showString "RequestBodySource " . showsPrec 11 l .
            showString " <Source m Builder>"
    showsPrec d (RequestBodySourceChunked _) =
        showParen (d>=11) $ showString "RequestBodySource <Source m Builder>"

-- | Since 1.8.7
instance Monad m => Monoid (RequestBody m) where
    mempty = RequestBodyLBS mempty

    mappend a b =
        case (simplify a, simplify b) of
            (SBuilder l1 b1, SBuilder l2 b2) -> RequestBodyBuilder (l1 + l2) (b1 <> b2)
            (SBuilder l1 b1, SSource l2 s2) -> RequestBodySource (l1 + l2) (C.yield b1 <> s2)
            (SSource l1 s1, SBuilder l2 b2) -> RequestBodySource (l1 + l2) (s1 <> C.yield b2)
            (SSource l1 s1, SSource l2 s2) -> RequestBodySource (l1 + l2) (s1 <> s2)
            (a', b') -> RequestBodySourceChunked (toChunked a' <> toChunked b')

data Simplified m = SBuilder Int64 Builder
                  | SSource Int64 (C.Source m Builder)
                  | SChunked (C.Source m Builder)

simplify :: Monad m => RequestBody m -> Simplified m
simplify (RequestBodyBS a) = SBuilder (fromIntegral $ S.length a) (fromByteString a)
simplify (RequestBodyLBS a) = SBuilder (fromIntegral $ L.length a) (fromLazyByteString a)
simplify (RequestBodyBuilder l a) = SBuilder l a
simplify (RequestBodySource l a) = SSource l a
simplify (RequestBodySourceChunked a) = SChunked a

toChunked :: Monad m => Simplified m -> C.Source m Builder
toChunked (SBuilder _ b) = C.yield b
toChunked (SSource _ s) = s
toChunked (SChunked s) = s
