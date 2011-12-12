module Network.HTTP.Conduit.Request
    ( Request (..)
    , RequestBody (..)
    , ContentType
    , Proxy (..)
    ) where

import qualified Data.Conduit as C
import qualified Blaze.ByteString.Builder as Blaze
import Data.Int (Int64)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Types as W
import Data.Certificate.X509 (X509)
import Network.TLS (TLSCertificateUsage)

type ContentType = S.ByteString

-- | All information on how to connect to a host and what should be sent in the
-- HTTP request.
--
-- If you simply wish to download from a URL, see 'parseUrl'.
--
-- The constructor for this data type is not exposed. Instead, you should use
-- either the 'def' method to retrieve a default instance, or 'parseUrl' to
-- construct from a URL, and then use the records below to make modifications.
-- This approach allows http-enumerator to add configuration options without
-- breaking backwards compatibility.
data Request m = Request
    { method :: W.Method -- ^ HTTP request method, eg GET, POST.
    , secure :: Bool -- ^ Whether to use HTTPS (ie, SSL).
    , checkCerts :: W.Ascii -> [X509] -> IO TLSCertificateUsage -- ^ Check if the server certificate is valid. Only relevant for HTTPS.
    , host :: W.Ascii
    , port :: Int
    , path :: W.Ascii -- ^ Everything from the host to the query string.
    , queryString :: W.Query -- ^ Automatically escaped for your convenience.
    , requestHeaders :: W.RequestHeaders
    , requestBody :: RequestBody m
    , proxy :: Maybe Proxy -- ^ Optional HTTP proxy.
    , rawBody :: Bool -- ^ If True, a chunked and/or gzipped body will not be decoded. Use with caution.
    , decompress :: ContentType -> Bool -- ^ Predicate to specify whether gzipped data should be decompressed on the fly.
    }

-- | When using the 'RequestBodyEnum' constructor and any function which calls
-- 'redirectIter', you must ensure that the 'Enumerator' can be called multiple
-- times.
--
-- The 'RequestBodyEnumChunked' will send a chunked request body, note
-- that not all servers support this. Only use 'RequestBodyEnumChunked'
-- if you know the server you're sending to supports chunked request
-- bodies.
data RequestBody m
    = RequestBodyLBS L.ByteString
    | RequestBodyBS S.ByteString
    | RequestBodyBuilder Int64 Blaze.Builder
    | RequestBodySource Int64 (C.SourceM m Blaze.Builder)
    | RequestBodySourceChunked (C.SourceM m Blaze.Builder)

-- | Define a HTTP proxy, consisting of a hostname and port number.

data Proxy = Proxy
    { proxyHost :: W.Ascii -- ^ The host name of the HTTP proxy.
    , proxyPort :: Int -- ^ The port numner of the HTTP proxy.
    }
