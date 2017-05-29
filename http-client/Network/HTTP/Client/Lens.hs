{-# LANGUAGE RankNTypes #-}

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
  , responseTimeout
  , cookieJar
  , responseCookieJar
  , responseStatus
  , responseVersion
  , responseHeaders
  , responseBody
  ) where

import           Data.ByteString              (ByteString)
import qualified Network.HTTP.Client.Internal as H
import qualified Network.HTTP.Types           as H
import           Network.Socket               (HostAddress)

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

method :: Lens' H.Request H.Method
method f req = f (H.method req) <&> \m' -> req { H.method = m' }
{-# INLINE method #-}

secure :: Lens' H.Request Bool
secure f req = f (H.secure req) <&> \s' -> req { H.secure = s' }
{-# INLINE secure #-}

host :: Lens' H.Request ByteString
host f req = f (H.host req) <&> \h' -> req { H.host = h' }
{-# INLINE host #-}

port :: Lens' H.Request Int
port f req = f (H.port req) <&> \p' -> req { H.port = p' }
{-# INLINE port #-}

path :: Lens' H.Request ByteString
path f req = f (H.path req) <&> \p' -> req { H.path = p' }
{-# INLINE path #-}

-- | 'H.queryString' lens
queryString :: Lens' H.Request ByteString
queryString f req = f (H.queryString req) <&> \qs' -> req { H.queryString = qs' }
{-# INLINE queryString #-}

requestBody :: Lens' H.Request H.RequestBody
requestBody f req = f (H.requestBody req) <&> \rb' -> req { H.requestBody = rb' }
{-# INLINE requestBody #-}

requestHeaders :: Lens' H.Request H.RequestHeaders
requestHeaders f req = f (H.requestHeaders req) <&> \rh' -> req { H.requestHeaders = rh' }
{-# INLINE requestHeaders #-}

proxy :: Lens' H.Request (Maybe H.Proxy)
proxy f req = f (H.proxy req) <&> \mp' -> req { H.proxy = mp' }
{-# INLINE proxy #-}

hostAddress :: Lens' H.Request (Maybe HostAddress)
hostAddress f req = f (H.hostAddress req) <&> \ha' -> req { H.hostAddress = ha' }
{-# INLINE hostAddress #-}

rawBody :: Lens' H.Request Bool
rawBody f req = f (H.rawBody req) <&> \b' -> req { H.rawBody = b' }
{-# INLINE rawBody #-}

decompress :: Lens' H.Request (ByteString -> Bool)
decompress f req = f (H.decompress req) <&> \btb' -> req { H.decompress = btb' }
{-# INLINE decompress #-}

redirectCount :: Lens' H.Request Int
redirectCount f req = f (H.redirectCount req) <&> \rc' -> req { H.redirectCount = rc' }
{-# INLINE redirectCount #-}

cookieJar :: Lens' H.Request (Maybe H.CookieJar)
cookieJar f req = f (H.cookieJar req) <&> \mcj' -> req { H.cookieJar = mcj' }
{-# INLINE cookieJar #-}

responseTimeout :: Lens' H.Request H.ResponseTimeout
responseTimeout f req = f (H.responseTimeout req) <&> \cs' -> req { H.responseTimeout = cs' }
{-# INLINE responseTimeout #-}

responseStatus :: Lens' (H.Response a) H.Status
responseStatus f resp = f (H.responseStatus resp) <&> \cs' -> resp { H.responseStatus = cs' }
{-# INLINE responseStatus #-}

responseVersion :: Lens' (H.Response a) H.HttpVersion
responseVersion f resp = f (H.responseVersion resp) <&> \cs' -> resp { H.responseVersion = cs' }
{-# INLINE responseVersion #-}

responseHeaders :: Lens' (H.Response a) H.ResponseHeaders
responseHeaders f resp = f (H.responseHeaders resp) <&> \cs' -> resp { H.responseHeaders = cs' }
{-# INLINE responseHeaders #-}

responseBody :: Lens' (H.Response a) a
responseBody f resp = f (H.responseBody resp) <&> \cs' -> resp { H.responseBody = cs' }
{-# INLINE responseBody #-}

responseCookieJar :: Lens' (H.Response a) H.CookieJar
responseCookieJar f resp = f (H.responseCookieJar resp) <&> \mcj' -> resp { H.responseCookieJar = mcj' }
{-# INLINE responseCookieJar #-}
