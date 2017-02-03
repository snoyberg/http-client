-- | Optics for @http-client@ types
module Network.HTTP.Client.Lens
  ( -- * 'Request' lenses
    method
  , secure
  , host
  , port
  , path
  , queryString
  , requestBody
  , requestHeaders
  , proxy
  , hostAddress
  , rawBody
  , decompress
  , redirectCount
  , checkStatus
  , responseTimeout
  , cookieJar
  , getConnectionWrapper
    -- * 'HttpException' prisms
  , AsHttpException(..)
  , _StatusCodeException
  , _InvalidUrlException
  , _TooManyRedirects
  , _UnparseableRedirect
  , _TooManyRetries
  , _HttpParserException
  , _HandshakeFailed
  , _OverlongHeaders
  , _ResponseTimeout
  , _FailedConnectionException
  , _ExpectedBlankAfter100Continue
  , _InvalidStatusLine
  , _InvalidHeader
  , _InternalIOException
  , _ProxyConnectException
  , _NoResponseDataReceived
  , _TlsException
  , _TlsNotSupported
  , _ResponseBodyTooShort
  , _InvalidChunkHeaders
  , _IncompleteHeaders
  ) where

import           Control.Exception            (IOException, SomeException)
import           Control.Exception.Lens       (exception)
import           Control.Lens
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Lazy         as Lazy
import           Data.Word                    (Word64)
import qualified Network.HTTP.Client.Internal as H
import qualified Network.HTTP.Types           as H
import           Network.Socket               (HostAddress)

-- | 'H.method' lens
method :: Lens' H.Request H.Method
method f req = f (H.method req) <&> \m' -> req { H.method = m' }
{-# INLINE method #-}

-- | 'H.secure' lens
secure :: Lens' H.Request Bool
secure f req = f (H.secure req) <&> \s' -> req { H.secure = s' }
{-# INLINE secure #-}

-- | 'H.host' lens
host :: Lens' H.Request ByteString
host f req = f (H.host req) <&> \h' -> req { H.host = h' }
{-# INLINE host #-}

-- | 'H.port' lens
port :: Lens' H.Request Int
port f req = f (H.port req) <&> \p' -> req { H.port = p' }
{-# INLINE port #-}

-- | 'H.path' lens
path :: Lens' H.Request ByteString
path f req = f (H.path req) <&> \p' -> req { H.path = p' }
{-# INLINE path #-}

-- | 'H.queryString' lens
queryString :: Lens' H.Request ByteString
queryString f req = f (H.queryString req) <&> \qs' -> req { H.queryString = qs' }
{-# INLINE queryString #-}

-- | 'H.requestBody' lens
requestBody :: Lens' H.Request H.RequestBody
requestBody f req = f (H.requestBody req) <&> \rb' -> req { H.requestBody = rb' }
{-# INLINE requestBody #-}

-- | 'H.requestHeaders' lens
requestHeaders :: Lens' H.Request H.RequestHeaders
requestHeaders f req = f (H.requestHeaders req) <&> \rh' -> req { H.requestHeaders = rh' }
{-# INLINE requestHeaders #-}

-- | 'H.proxy'
proxy :: Lens' H.Request (Maybe H.Proxy)
proxy f req = f (H.proxy req) <&> \mp' -> req { H.proxy = mp' }
{-# INLINE proxy #-}

-- | 'H.hostAddress'
hostAddress :: Lens' H.Request (Maybe HostAddress)
hostAddress f req = f (H.hostAddress req) <&> \ha' -> req { H.hostAddress = ha' }
{-# INLINE hostAddress #-}

-- | 'H.rawBody'
rawBody :: Lens' H.Request Bool
rawBody f req = f (H.rawBody req) <&> \b' -> req { H.rawBody = b' }
{-# INLINE rawBody #-}

-- | 'H.decompress'
decompress :: Lens' H.Request (ByteString -> Bool)
decompress f req = f (H.decompress req) <&> \btb' -> req { H.decompress = btb' }
{-# INLINE decompress #-}

-- | 'H.redirectCount' lens
redirectCount :: Lens' H.Request Int
redirectCount f req = f (H.redirectCount req) <&> \rc' -> req { H.redirectCount = rc' }
{-# INLINE redirectCount #-}

-- | 'H.checkStatus' lens
checkStatus :: Lens' H.Request (H.Status -> H.ResponseHeaders -> H.CookieJar -> Maybe SomeException)
checkStatus f req = f (H.checkStatus req) <&> \cs' -> req { H.checkStatus = cs' }
{-# INLINE checkStatus #-}

-- | 'H.responseTimeout' lens
responseTimeout :: Lens' H.Request (Maybe Int)
responseTimeout f req = f (H.responseTimeout req) <&> \rt' -> req { H.responseTimeout = rt' }
{-# INLINE responseTimeout #-}

-- | 'H.cookieJar'
cookieJar :: Lens' H.Request (Maybe H.CookieJar)
cookieJar f req = f (H.cookieJar req) <&> \mcj' -> req { H.cookieJar = mcj' }
{-# INLINE cookieJar #-}

-- | 'H.getConnectionWrapper'
getConnectionWrapper
  :: Lens' H.Request
      ( Maybe Int
      -> H.HttpException
      -> IO (H.ConnRelease, H.Connection, H.ManagedConn)
      -> IO (Maybe Int, (H.ConnRelease, H.Connection, H.ManagedConn))
      )
getConnectionWrapper f req =
  f (H.getConnectionWrapper req) <&> \wat' -> req { H.getConnectionWrapper = wat' }
{-# INLINE getConnectionWrapper #-}


-- | @http-conduit@ exceptions
class AsHttpException t where
  -- | @http-conduit@ exceptions overloading
  _HttpException :: Prism' t H.HttpException

instance AsHttpException H.HttpException where
  _HttpException = id
  {-# INLINE _HttpException #-}

instance AsHttpException SomeException where
  _HttpException = exception
  {-# INLINE _HttpException #-}

-- | 'H.StatusCodeException' exception
_StatusCodeException :: AsHttpException t => Prism' t (H.Status, H.ResponseHeaders, H.CookieJar)
_StatusCodeException = _HttpException . prism' (uncurry3 H.StatusCodeException) go where
  go (H.StatusCodeException s rh cj) = Just (s, rh, cj)
  go _ = Nothing
{-# INLINE _StatusCodeException #-}

-- | 'H.InvalidUrlException' exception
_InvalidUrlException :: AsHttpException t => Prism' t (String, String)
_InvalidUrlException = _HttpException . prism' (uncurry H.InvalidUrlException) go where
  go (H.InvalidUrlException s s') = Just (s, s')
  go _ = Nothing
{-# INLINE _InvalidUrlException #-}

-- | 'H.TooManyRedirects' exception
_TooManyRedirects :: AsHttpException t => Prism' t [H.Response Lazy.ByteString]
_TooManyRedirects = _HttpException . prism' H.TooManyRedirects go where
  go (H.TooManyRedirects rs) = Just rs
  go _ = Nothing
{-# INLINE _TooManyRedirects #-}

-- | 'H.UnparseableRedirect' exception
_UnparseableRedirect :: AsHttpException t => Prism' t (H.Response Lazy.ByteString)
_UnparseableRedirect = _HttpException . prism' H.UnparseableRedirect go where
  go (H.UnparseableRedirect r) = Just r
  go _ = Nothing
{-# INLINE _UnparseableRedirect #-}

-- | 'H.TooManyRetries' exception
_TooManyRetries :: AsHttpException t => Prism' t ()
_TooManyRetries = _HttpException . prism' (const H.TooManyRetries) go where
  go H.TooManyRetries = Just ()
  go _ = Nothing
{-# INLINE _TooManyRetries #-}

-- | 'H.HttpParserException' exception
_HttpParserException :: AsHttpException t => Prism' t String
_HttpParserException = _HttpException . prism' H.HttpParserException go where
  go (H.HttpParserException s) = Just s
  go _ = Nothing
{-# INLINE _HttpParserException #-}

-- | 'H.HandshakeFailed' exception
_HandshakeFailed :: AsHttpException t => Prism' t ()
_HandshakeFailed = _HttpException . prism' (const H.HandshakeFailed) go where
  go H.HandshakeFailed = Just ()
  go _ = Nothing
{-# INLINE _HandshakeFailed #-}

-- | 'H.OverlongHeaders' exception
_OverlongHeaders :: AsHttpException t => Prism' t ()
_OverlongHeaders = _HttpException . prism' (const H.OverlongHeaders) go where
  go H.OverlongHeaders = Just ()
  go _ = Nothing
{-# INLINE _OverlongHeaders #-}

-- | 'H.ResponseTimeout' exception
_ResponseTimeout :: AsHttpException t => Prism' t ()
_ResponseTimeout = _HttpException . prism' (const H.ResponseTimeout) go where
  go H.ResponseTimeout = Just ()
  go _ = Nothing
{-# INLINE _ResponseTimeout #-}

-- | 'H.FailedConnectionException' exception
_FailedConnectionException :: AsHttpException t => Prism' t (String, Int)
_FailedConnectionException = _HttpException . prism' (uncurry H.FailedConnectionException) go where
  go (H.FailedConnectionException s i) = Just (s, i)
  go _ = Nothing
{-# INLINE _FailedConnectionException #-}

-- | 'H.ExpectedBlankAfter100Continue' exception
_ExpectedBlankAfter100Continue :: AsHttpException t => Prism' t ()
_ExpectedBlankAfter100Continue =
  _HttpException . prism' (const H.ExpectedBlankAfter100Continue) go where
    go H.ExpectedBlankAfter100Continue = Just ()
    go _ = Nothing
{-# INLINE _ExpectedBlankAfter100Continue #-}

-- | 'H.InvalidStatusLine' exception
_InvalidStatusLine :: AsHttpException t => Prism' t ByteString
_InvalidStatusLine = _HttpException . prism' H.InvalidStatusLine go where
  go (H.InvalidStatusLine b) = Just b
  go _ = Nothing
{-# INLINE _InvalidStatusLine #-}

-- | 'H.InvalidHeader' exception
_InvalidHeader :: AsHttpException t => Prism' t ByteString
_InvalidHeader = _HttpException . prism' H.InvalidHeader go where
  go (H.InvalidHeader b) = Just b
  go _ = Nothing
{-# INLINE _InvalidHeader #-}

-- | 'H.InternalIOException' exception
_InternalIOException :: AsHttpException t => Prism' t IOException
_InternalIOException = _HttpException . prism' H.InternalIOException go where
  go (H.InternalIOException ioe) = Just ioe
  go _ = Nothing
{-# INLINE _InternalIOException #-}

-- | 'H.ProxyConnectException' exception
_ProxyConnectException :: AsHttpException t => Prism' t (ByteString, Int, Either ByteString H.HttpException)
_ProxyConnectException = _HttpException . prism' (uncurry3 H.ProxyConnectException) go where
  go (H.ProxyConnectException b i ebhe) = Just (b, i, ebhe)
  go _ = Nothing
{-# INLINE _ProxyConnectException #-}

-- | 'H.NoResponseDataReceived' exception
_NoResponseDataReceived :: AsHttpException t => Prism' t ()
_NoResponseDataReceived = _HttpException . prism' (const H.NoResponseDataReceived) go where
  go H.NoResponseDataReceived = Just ()
  go _ = Nothing
{-# INLINE _NoResponseDataReceived #-}

-- | 'H.TlsException' exception
_TlsException :: AsHttpException t => Prism' t SomeException
_TlsException = _HttpException . prism' H.TlsException go where
  go (H.TlsException se) = Just se
  go _ = Nothing
{-# INLINE _TlsException #-}

-- | 'H.TlsNotSupported' exception
_TlsNotSupported :: AsHttpException t => Prism' t ()
_TlsNotSupported = _HttpException . prism' (const H.TlsNotSupported) go where
  go H.TlsNotSupported = Just ()
  go _ = Nothing
{-# INLINE _TlsNotSupported #-}

-- | 'H.ResponseBodyTooShort' exception
_ResponseBodyTooShort :: AsHttpException t => Prism' t (Word64, Word64)
_ResponseBodyTooShort = _HttpException . prism' (uncurry H.ResponseBodyTooShort) go where
  go (H.ResponseBodyTooShort w w') = Just (w, w')
  go _ = Nothing
{-# INLINE _ResponseBodyTooShort #-}

-- | 'H.InvalidChunkHeaders' exception
_InvalidChunkHeaders :: AsHttpException t => Prism' t ()
_InvalidChunkHeaders = _HttpException . prism' (const H.InvalidChunkHeaders) go where
  go H.InvalidChunkHeaders = Just ()
  go _ = Nothing
{-# INLINE _InvalidChunkHeaders #-}

-- | 'H.IncompleteHeaders' exception
_IncompleteHeaders :: AsHttpException t => Prism' t ()
_IncompleteHeaders = _HttpException . prism' (const H.IncompleteHeaders) go where
  go H.IncompleteHeaders = Just ()
  go _ = Nothing
{-# INLINE _IncompleteHeaders #-}

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
{-# INLINE uncurry3 #-}
