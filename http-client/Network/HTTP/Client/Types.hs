{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
module Network.HTTP.Client.Types
    ( BodyReader
    , Connection (..)
    , StatusHeaders (..)
    , ConnectionClosed (..)
    , HttpException (..)
    , Cookie (..)
    , CookieJar (..)
    , Proxy (..)
    , RequestBody (..)
    , Popper
    , NeedsPopper
    , GivesPopper
    , Request (..)
    , ConnReuse (..)
    , ConnRelease
    , ManagedConn (..)
    , Response (..)
    , ResponseClose (..)
    , Manager (..)
    , HasHttpManager (..)
    , ConnsMap (..)
    , ManagerSettings (..)
    , NonEmptyList (..)
    , ConnHost (..)
    , ConnKey (..)
    , ProxyOverride (..)
    , StreamFileStatus (..)
    ) where

import qualified Data.Typeable as T (Typeable)
import Network.HTTP.Types
import Control.Exception (Exception, IOException, SomeException)
import Data.Word (Word64)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Blaze.ByteString.Builder (Builder, fromLazyByteString, fromByteString, toLazyByteString)
import Data.Int (Int64)
import Data.Default.Class
import Data.Foldable (Foldable)
import Data.Monoid
import Data.String (IsString, fromString)
import Data.Time (UTCTime)
import Data.Traversable (Traversable)
import qualified Data.List as DL
import Network.Socket (HostAddress)
import Data.IORef
import qualified Network.Socket as NS
import qualified Data.IORef as I
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Streaming.Zlib (ZlibException)
import Control.Concurrent.MVar (MVar)
import Data.CaseInsensitive as CI

-- | An @IO@ action that represents an incoming response body coming from the
-- server. Data provided by this action has already been gunzipped and
-- de-chunked, and respects any content-length headers present.
--
-- The action gets a single chunk of data from the response body, or an empty
-- bytestring if no more data is available.
--
-- Since 0.4.0
type BodyReader = IO S.ByteString

data Connection = Connection
    { connectionRead :: IO S.ByteString
      -- ^ If no more data, return empty.
    , connectionUnread :: S.ByteString -> IO ()
      -- ^ Return data to be read next time.
    , connectionWrite :: S.ByteString -> IO ()
      -- ^ Send data to server
    , connectionClose :: IO ()
      -- ^ Close connection. Any successive operation on the connection
      -- (exept closing) should fail with `ConnectionClosed` exception.
      -- It is allowed to close connection multiple times.
    }
    deriving T.Typeable

data StatusHeaders = StatusHeaders Status HttpVersion RequestHeaders
    deriving (Show, Eq, Ord, T.Typeable)

data ConnectionClosed = ConnectionClosed
  deriving (Eq, Show, T.Typeable)

instance Exception ConnectionClosed

data HttpException = StatusCodeException Status ResponseHeaders CookieJar
                   | InvalidUrlException String String
                   | TooManyRedirects [Response L.ByteString]  -- ^ List of encountered responses containing redirects in reverse chronological order; including last redirect, which triggered the exception and was not followed.
                   | UnparseableRedirect (Response L.ByteString) -- ^ Response containing unparseable redirect.
                   | TooManyRetries
                   | HttpParserException String
                   | HandshakeFailed
                   | OverlongHeaders
                   | ResponseTimeout
                   | FailedConnectionException String Int
                   -- ^ host/port
                   --
                   -- Note that in old versions of http-client and
                   -- http-conduit, this exception would indicate a failed
                   -- attempt to create a connection. However, since (at least)
                   -- http-client 0.4, it indicates a timeout occurred while
                   -- trying to establish the connection. For more information
                   -- on this, see:
                   --
                   -- <https://github.com/snoyberg/http-client/commit/b86b1cdd91e56ee33150433dedb32954d2082621#commitcomment-10718689>
                   | FailedConnectionException2 String Int Bool SomeException -- ^ host\/port\/secure
                   | ExpectedBlankAfter100Continue
                   | InvalidStatusLine S.ByteString
                   | InvalidHeader S.ByteString
                   | InternalIOException IOException
                   | ProxyConnectException S.ByteString Int (Either S.ByteString HttpException) -- ^ host/port
                   | NoResponseDataReceived
                   | TlsException SomeException
                   | TlsNotSupported
                   | ResponseBodyTooShort Word64 Word64
                   -- ^ Expected size/actual size.
                   --
                   -- Since 1.9.4
                   | InvalidChunkHeaders
                   -- ^
                   --
                   -- Since 1.9.4
                   | IncompleteHeaders
                   | InvalidDestinationHost S.ByteString
                   | HttpZlibException ZlibException
                   -- ^
                   --
                   -- Since 0.3
                   | InvalidProxyEnvironmentVariable Text Text
                   -- ^ Environment name and value
                   --
                   -- Since 0.4.7
                   | ResponseLengthAndChunkingBothUsed
                   -- ^ Detect a case where both the @content-length@ header
                   -- and @transfer-encoding: chunked@ are used. Since 0.4.8.
                   --
                   -- Since 0.4.11 this exception isn't thrown anymore.
                   | TlsExceptionHostPort SomeException S.ByteString Int
                   -- ^ TLS exception, together with the host and port
                   --
                   -- @since 0.4.24
    deriving (Show, T.Typeable)
instance Exception HttpException


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
  deriving (Read, Show, T.Typeable)

newtype CookieJar = CJ { expose :: [Cookie] }
  deriving (Read, Show, T.Typeable)

-- This corresponds to step 11 of the algorithm described in Section 5.3 \"Storage Model\"
instance Eq Cookie where
  (==) a b = name_matches && domain_matches && path_matches
    where name_matches = cookie_name a == cookie_name b
          domain_matches = CI.foldCase (cookie_domain a) == CI.foldCase (cookie_domain b)
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

-- | Define a HTTP proxy, consisting of a hostname and port number.

data Proxy = Proxy
    { proxyHost :: S.ByteString -- ^ The host name of the HTTP proxy.
    , proxyPort :: Int -- ^ The port number of the HTTP proxy.
    }
    deriving (Show, Read, Eq, Ord, T.Typeable)

-- | When using one of the 'RequestBodyStream' \/ 'RequestBodyStreamChunked'
-- constructors, you must ensure that the 'GivesPopper' can be called multiple
-- times.  Usually this is not a problem.
--
-- The 'RequestBodyStreamChunked' will send a chunked request body. Note that
-- not all servers support this. Only use 'RequestBodyStreamChunked' if you
-- know the server you're sending to supports chunked request bodies.
--
-- Since 0.1.0
data RequestBody
    = RequestBodyLBS L.ByteString
    | RequestBodyBS S.ByteString
    | RequestBodyBuilder Int64 Builder
    | RequestBodyStream Int64 (GivesPopper ())
    | RequestBodyStreamChunked (GivesPopper ())
    | RequestBodyIO (IO RequestBody)
    -- ^ Allows creation of a @RequestBody@ inside the @IO@ monad, which is
    -- useful for making easier APIs (like @setRequestBodyFile@).
    --
    -- @since 0.4.28
    deriving T.Typeable
-- |
--
-- Since 0.4.12
instance IsString RequestBody where
    fromString str = RequestBodyBS (fromString str)
instance Monoid RequestBody where
    mempty = RequestBodyBS S.empty
    mappend x0 y0 =
        case (simplify x0, simplify y0) of
            (Left (i, x), Left (j, y)) -> RequestBodyBuilder (i + j) (x `mappend` y)
            (Left x, Right y) -> combine (builderToStream x) y
            (Right x, Left y) -> combine x (builderToStream y)
            (Right x, Right y) -> combine x y
      where
        combine (Just i, x) (Just j, y) = RequestBodyStream (i + j) (combine' x y)
        combine (_, x) (_, y) = RequestBodyStreamChunked (combine' x y)

        combine' :: GivesPopper () -> GivesPopper () -> GivesPopper ()
        combine' x y f = x $ \x' -> y $ \y' -> combine'' x' y' f

        combine'' :: Popper -> Popper -> NeedsPopper () -> IO ()
        combine'' x y f = do
            istate <- newIORef $ Left (x, y)
            f $ go istate

        go istate = do
            state <- readIORef istate
            case state of
                Left (x, y) -> do
                    bs <- x
                    if S.null bs
                        then do
                            writeIORef istate $ Right y
                            y
                        else return bs
                Right y -> y

simplify :: RequestBody -> Either (Int64, Builder) (Maybe Int64, GivesPopper ())
simplify (RequestBodyLBS lbs) = Left (L.length lbs, fromLazyByteString lbs)
simplify (RequestBodyBS bs) = Left (fromIntegral $ S.length bs, fromByteString bs)
simplify (RequestBodyBuilder len b) = Left (len, b)
simplify (RequestBodyStream i gp) = Right (Just i, gp)
simplify (RequestBodyStreamChunked gp) = Right (Nothing, gp)

builderToStream :: (Int64, Builder) -> (Maybe Int64, GivesPopper ())
builderToStream (len, builder) =
    (Just len, gp)
  where
    gp np = do
        ibss <- newIORef $ L.toChunks $ toLazyByteString builder
        np $ do
            bss <- readIORef ibss
            case bss of
                [] -> return S.empty
                bs:bss' -> do
                    writeIORef ibss bss'
                    return bs

-- | A function which generates successive chunks of a request body, provider a
-- single empty bytestring when no more data is available.
--
-- Since 0.1.0
type Popper = IO S.ByteString

-- | A function which must be provided with a 'Popper'.
--
-- Since 0.1.0
type NeedsPopper a = Popper -> IO a

-- | A function which will provide a 'Popper' to a 'NeedsPopper'. This
-- seemingly convoluted structure allows for creation of request bodies which
-- allocate scarce resources in an exception safe manner.
--
-- Since 0.1.0
type GivesPopper a = NeedsPopper a -> IO a

-- | All information on how to connect to a host and what should be sent in the
-- HTTP request.
--
-- If you simply wish to download from a URL, see 'parseRequest'.
--
-- The constructor for this data type is not exposed. Instead, you should use
-- either the 'defaultRequest' value, or 'parseRequest' to
-- construct from a URL, and then use the records below to make modifications.
-- This approach allows http-client to add configuration options without
-- breaking backwards compatibility.
--
-- For example, to construct a POST request, you could do something like:
--
-- > initReq <- parseRequest "http://www.example.com/path"
-- > let req = initReq
-- >             { method = "POST"
-- >             }
--
-- For more information, please see
-- <http://www.yesodweb.com/book/settings-types>.
--
-- Since 0.1.0
data Request = Request
    { method :: Method
    -- ^ HTTP request method, eg GET, POST.
    --
    -- Since 0.1.0
    , secure :: Bool
    -- ^ Whether to use HTTPS (ie, SSL).
    --
    -- Since 0.1.0
    , host :: S.ByteString
    -- ^ Requested host name, used for both the IP address to connect to and
    -- the @host@ request header.
    --
    -- Since 0.1.0
    , port :: Int
    -- ^ The port to connect to. Also used for generating the @host@ request header.
    --
    -- Since 0.1.0
    , path :: S.ByteString
    -- ^ Everything from the host to the query string.
    --
    -- Since 0.1.0
    , queryString :: S.ByteString
    -- ^ Query string appended to the path.
    --
    -- Since 0.1.0
    , requestHeaders :: RequestHeaders
    -- ^ Custom HTTP request headers
    --
    -- The Content-Length and Transfer-Encoding headers are set automatically
    -- by this module, and shall not be added to @requestHeaders@.
    --
    -- If not provided by the user, @Host@ will automatically be set based on
    -- the @host@ and @port@ fields.
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
    --
    -- Since 0.1.0
    , requestBody :: RequestBody
    -- ^ Request body to be sent to the server.
    --
    -- Since 0.1.0
    , proxy :: Maybe Proxy
    -- ^ Optional HTTP proxy.
    --
    -- Since 0.1.0
    , hostAddress :: Maybe HostAddress
    -- ^ Optional resolved host address. May not be used by all backends.
    --
    -- Since 0.1.0
    , rawBody :: Bool
    -- ^ If @True@, a chunked and\/or gzipped body will not be
    -- decoded. Use with caution.
    --
    -- Since 0.1.0
    , decompress :: S.ByteString -> Bool
    -- ^ Predicate to specify whether gzipped data should be
    -- decompressed on the fly (see 'alwaysDecompress' and
    -- 'browserDecompress'). Argument is the mime type.
    -- Default: browserDecompress.
    --
    -- Since 0.1.0
    , redirectCount :: Int
    -- ^ How many redirects to follow when getting a resource. 0 means follow
    -- no redirects. Default value: 10.
    --
    -- Since 0.1.0
    , checkStatus :: Status -> ResponseHeaders -> CookieJar -> Maybe SomeException
    -- ^ Check the status code. Note that this will run after all redirects are
    -- performed. Default: return a @StatusCodeException@ on non-2XX responses.
    --
    -- Since 0.1.0
    , responseTimeout :: Maybe Int
    -- ^ Number of microseconds to wait for a response. If
    -- @Nothing@, will wait indefinitely. Default: use
    -- 'managerResponseTimeout' (which by default is 30 seconds).
    --
    -- Since 0.1.0
    , getConnectionWrapper :: Maybe Int
                           -> HttpException
                           -> IO (ConnRelease, Connection, ManagedConn)
                           -> IO (Maybe Int, (ConnRelease, Connection, ManagedConn))
    -- ^ Wraps the calls for getting new connections. This can be useful for
    -- instituting some kind of timeouts. The first argument is the value of
    -- @responseTimeout@. Second argument is the exception to be thrown on
    -- failure.
    --
    -- Default: If @responseTimeout@ is @Nothing@, does nothing. Otherwise,
    -- institutes timeout, and returns remaining time for @responseTimeout@.
    --
    -- Since 0.1.0
    , cookieJar :: Maybe CookieJar
    -- ^ A user-defined cookie jar.
    -- If 'Nothing', no cookie handling will take place, \"Cookie\" headers
    -- in 'requestHeaders' will be sent raw, and 'responseCookieJar' will be
    -- empty.
    --
    -- Since 0.1.0

    , requestVersion :: HttpVersion
    -- ^ HTTP version to send to server.
    --
    -- Default: HTTP 1.1
    --
    -- Since 0.4.3

    , onRequestBodyException :: SomeException -> IO ()
    -- ^ How to deal with exceptions thrown while sending the request.
    --
    -- Default: ignore @IOException@s, rethrow all other exceptions.
    --
    -- Since: 0.4.6

    , requestManagerOverride :: Maybe Manager
    -- ^ A 'Manager' value that should override whatever @Manager@ value was
    -- passed in to the HTTP request function manually. This is useful when
    -- dealing with implicit global managers, such as in @Network.HTTP.Simple@
    --
    -- @since 0.4.28
    }
    deriving T.Typeable

data ConnReuse = Reuse | DontReuse
    deriving T.Typeable

type ConnRelease = ConnReuse -> IO ()

data ManagedConn = Fresh | Reused

-- | A simple representation of the HTTP response.
--
-- Since 0.1.0
data Response body = Response
    { responseStatus :: Status
    -- ^ Status code of the response.
    --
    -- Since 0.1.0
    , responseVersion :: HttpVersion
    -- ^ HTTP version used by the server.
    --
    -- Since 0.1.0
    , responseHeaders :: ResponseHeaders
    -- ^ Response headers sent by the server.
    --
    -- Since 0.1.0
    , responseBody :: body
    -- ^ Response body sent by the server.
    --
    -- Since 0.1.0
    , responseCookieJar :: CookieJar
    -- ^ Cookies set on the client after interacting with the server. If
    -- cookies have been disabled by setting 'cookieJar' to @Nothing@, then
    -- this will always be empty.
    --
    -- Since 0.1.0
    , responseClose' :: ResponseClose
    -- ^ Releases any resource held by this response. If the response body
    -- has not been fully read yet, doing so after this call will likely
    -- be impossible.
    --
    -- Since 0.1.0
    }
    deriving (Show, Eq, T.Typeable, Functor, Foldable, Traversable)

newtype ResponseClose = ResponseClose { runResponseClose :: IO () }
    deriving T.Typeable
instance Show ResponseClose where
    show _ = "ResponseClose"
instance Eq ResponseClose where
    _ == _ = True

-- | Settings for a @Manager@. Please use the 'defaultManagerSettings' function and then modify
-- individual settings. For more information, see <http://www.yesodweb.com/book/settings-types>.
--
-- Since 0.1.0
data ManagerSettings = ManagerSettings
    { managerConnCount :: Int
      -- ^ Number of connections to a single host to keep alive. Default: 10.
      --
      -- Since 0.1.0
    , managerRawConnection :: IO (Maybe NS.HostAddress -> String -> Int -> IO Connection)
      -- ^ Create an insecure connection.
      --
      -- Since 0.1.0
    -- FIXME in the future, combine managerTlsConnection and managerTlsProxyConnection
    , managerTlsConnection :: IO (Maybe NS.HostAddress -> String -> Int -> IO Connection)
      -- ^ Create a TLS connection. Default behavior: throw an exception that TLS is not supported.
      --
      -- Since 0.1.0
    , managerTlsProxyConnection :: IO (S.ByteString -> (Connection -> IO ()) -> String -> Maybe NS.HostAddress -> String -> Int -> IO Connection)
      -- ^ Create a TLS proxy connection. Default behavior: throw an exception that TLS is not supported.
      --
      -- Since 0.2.2
    , managerResponseTimeout :: Maybe Int
      -- ^ Default timeout (in microseconds) to be applied to requests which do
      -- not provide a timeout value.
      --
      -- Default is 30 seconds
      --
      -- Since 0.1.0
    , managerRetryableException :: SomeException -> Bool
    -- ^ Exceptions for which we should retry our request if we were reusing an
    -- already open connection. In the case of IOExceptions, for example, we
    -- assume that the connection was closed on the server and therefore open a
    -- new one.
    --
    -- Since 0.1.0
    , managerWrapIOException :: forall a. IO a -> IO a
    -- ^ Action wrapped around all attempted @Request@s, usually used to wrap
    -- up exceptions in library-specific types.
    --
    -- Default: wrap all @IOException@s in the @InternalIOException@ constructor.
    --
    -- Since 0.1.0
    , managerIdleConnectionCount :: Int
    -- ^ Total number of idle connection to keep open at a given time.
    --
    -- This limit helps deal with the case where you are making a large number
    -- of connections to different hosts. Without this limit, you could run out
    -- of file descriptors.
    --
    -- Default: 512
    --
    -- Since 0.3.7
    , managerModifyRequest :: Request -> IO Request
    -- ^ Perform the given modification to a @Request@ before performing it.
    --
    -- Default: no modification
    --
    -- Since 0.4.4
    , managerProxyInsecure :: ProxyOverride
    -- ^ How HTTP proxy server settings should be discovered.
    --
    -- Default: respect the @proxy@ value on the @Request@ itself.
    --
    -- Since 0.4.7
    , managerProxySecure :: ProxyOverride
    -- ^ How HTTPS proxy server settings should be discovered.
    --
    -- Default: respect the @proxy@ value on the @Request@ itself.
    --
    -- Since 0.4.7
    }
    deriving T.Typeable

-- | How the HTTP proxy server settings should be discovered.
--
-- Since 0.4.7
newtype ProxyOverride = ProxyOverride
    { runProxyOverride :: Bool -> IO (Request -> Request)
    }
    deriving T.Typeable

-- | Keeps track of open connections for keep-alive.
--
-- If possible, you should share a single 'Manager' between multiple threads and requests.
--
-- Since 0.1.0
data Manager = Manager
    { mConns :: I.IORef ConnsMap
    -- ^ @Nothing@ indicates that the manager is closed.
    , mConnsBaton :: MVar ()
    -- ^ Used to indicate to the reaper thread that it has some work to do.
    -- This must be filled every time a connection is returned to the manager.
    -- While redundant with the @IORef@ above, this allows us to have the
    -- reaper thread fully blocked instead of running every 5 seconds when
    -- there are no connections to manage.
    , mMaxConns :: Int
    -- ^ This is a per-@ConnKey@ value.
    , mResponseTimeout :: Maybe Int
    -- ^ Copied from 'managerResponseTimeout'
    , mRawConnection :: Maybe NS.HostAddress -> String -> Int -> IO Connection
    , mTlsConnection :: Maybe NS.HostAddress -> String -> Int -> IO Connection
    , mTlsProxyConnection :: S.ByteString -> (Connection -> IO ()) -> String -> Maybe NS.HostAddress -> String -> Int -> IO Connection
    , mRetryableException :: SomeException -> Bool
    , mWrapIOException :: forall a. IO a -> IO a
    , mIdleConnectionCount :: Int
    , mModifyRequest :: Request -> IO Request
    , mSetProxy :: Request -> Request
    -- ^ See 'managerProxy'
    }
    deriving T.Typeable

class HasHttpManager a where
    getHttpManager :: a -> Manager
instance HasHttpManager Manager where
    getHttpManager = id

data ConnsMap
    = ManagerClosed
    | ManagerOpen {-# UNPACK #-} !Int !(Map.Map ConnKey (NonEmptyList Connection))

data NonEmptyList a =
    One a UTCTime |
    Cons a Int UTCTime (NonEmptyList a)
    deriving T.Typeable

-- | Hostname or resolved host address.
data ConnHost =
    HostName Text |
    HostAddress NS.HostAddress
    deriving (Eq, Show, Ord, T.Typeable)

-- | @ConnKey@ consists of a hostname, a port and a @Bool@
-- specifying whether to use SSL.
data ConnKey = ConnKey ConnHost Int S.ByteString Int Bool
    deriving (Eq, Show, Ord, T.Typeable)

-- | Status of streaming a request body from a file.
--
-- Since 0.4.9
data StreamFileStatus = StreamFileStatus
    { fileSize :: Int64
    , readSoFar :: Int64
    , thisChunkSize :: Int
    }
    deriving (Eq, Show, Ord, T.Typeable)
