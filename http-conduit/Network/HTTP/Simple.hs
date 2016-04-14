{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Simplified interface for common HTTP client interactions. Tutorial
-- available at
-- <https://github.com/commercialhaskell/jump/blob/master/doc/http-client.md>.
--
-- Important note: 'Request' is an instance of 'IsString', and therefore
-- recommended usage is to turn on @OverloadedStrings@, e.g.
--
-- @@@
-- {-# LANGUAGE OverloadedStrings #-}
-- import Network.HTTP.Simple
-- import qualified Data.ByteString.Lazy.Char8 as L8
--
-- main :: IO ()
-- main = httpLBS "http://example.com" >>= L8.putStrLn
-- @@@
module Network.HTTP.Simple
    ( -- * Perform requests
      httpLBS
    , httpJSON
    , httpJSONEither
    , httpSink
      -- * Types
    , H.Request
    , H.Response
    , JSONException (..)
    , H.HttpException (..)
      -- * Request constructions
    , defaultRequest
    , parseRequest
      -- * Request lenses
      -- ** Basics
    , requestMethod
    , requestSecure
    , requestHost
    , requestPort
    , requestPath
    , requestHeader
    , requestHeaders
    , requestQueryString
      -- ** Request body
    , requestBody
    , requestBodyJSON
    , requestBodyLBS
    , requestBodySource
    -- FIXME , requestBodyFile
    , requestBodyURLEncoded
      -- ** Special fields
    , requestIgnoreStatus
    , requestBasicAuth
    , requestManager
      -- * Response lenses
      -- * Alternate spellings
    , httpLbs
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.Internal as HI
import qualified Network.HTTP.Client.TLS as H
import Network.HTTP.Client.Conduit (bodyReaderSource)
import qualified Network.HTTP.Client.Conduit as HC
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON (..), Value)
import Data.Aeson.Parser (json')
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Encode as A
import qualified Data.Traversable as T
import Control.Exception (throwIO, Exception)
import Data.Typeable (Typeable)
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as C
import qualified Control.Monad.Catch as Catch
import Data.Default.Class (def)
import qualified Network.HTTP.Types as H
import Data.Int (Int64)

-- | Perform an HTTP request and return the body as a lazy @ByteString@. Note
-- that the entire value will be read into memory at once (no lazy I\/O will be
-- performed).
--
-- @since 0.2.4
httpLBS :: MonadIO m => H.Request -> m (H.Response L.ByteString)
httpLBS req = liftIO $ do
    man <- H.getGlobalManager
    H.httpLbs req man

-- | Perform an HTTP request and parse the body as JSON. In the event of an
-- JSON parse errors, a 'JSONException' runtime exception will be thrown.
--
-- @since 0.2.4
httpJSON :: (MonadIO m, FromJSON a) => H.Request -> m (H.Response a)
httpJSON req = liftIO $ httpJSONEither req >>= T.mapM (either throwIO return)

-- | Perform an HTTP request and parse the body as JSON. In the event of an
-- JSON parse errors, a @Left@ value will be returned.
--
-- @since 0.2.4
httpJSONEither :: (MonadIO m, FromJSON a)
               => H.Request
               -> m (H.Response (Either JSONException a))
httpJSONEither req =
    liftIO $ httpSink req sink
  where
    sink orig = do
        eres1 <- C.sinkParserEither json'
        case eres1 of
            Left e -> return $ Left $ JSONParseException req orig e
            Right value ->
                case A.fromJSON value of
                    A.Error e -> return $ Left $ JSONConversionException
                        req (fmap (const value) orig) e
                    A.Success x -> return $ Right x

-- | An exception that can occur when parsing JSON
--
-- @since 0.2.4
data JSONException
    = JSONParseException H.Request (H.Response ()) C.ParseError
    | JSONConversionException H.Request (H.Response Value) String
  deriving (Show, Typeable)
instance Exception JSONException

-- | The default request value. You'll almost certainly want to set the
-- 'requestHost', and likely the 'requestPath' as well.
--
-- See also 'parseRequest'
--
-- @since 0.2.4
defaultRequest :: H.Request
defaultRequest = def

-- | Parse a 'H.Request' from a 'String'. This is given as a URL, with an
-- optional leading request method, e.g.:
--
-- * @http://example.com@
-- * @https://example.com:1234/foo/bar?baz=bin@
-- * @PUT http://example.com/some-resource@
--
-- If parsing fails, 'Catch.throwM' will be called. The behavior of this
-- function is also used for the @IsString@ instance for use with
-- @OverloadedStrings@.
--
-- @since 0.2.4
parseRequest :: Catch.MonadThrow m => String -> m H.Request
parseRequest = H.parseUrl

-- | Perform an HTTP request and consume the body with the given 'C.Sink'
--
-- @since 0.2.4
httpSink :: (MonadIO m, Catch.MonadMask m)
         => H.Request
         -> (H.Response () -> C.Sink S.ByteString m a)
         -> m (H.Response a)
httpSink req sink = do
    man <- liftIO H.getGlobalManager
    Catch.bracket
        (liftIO $ H.responseOpen req man)
        (liftIO . H.responseClose)
        (\res -> T.mapM ((C.$$ sink (plain res)) . bodyReaderSource) res)
  where
    plain = fmap (const ())

-- | Alternate spelling of 'httpLBS'
--
-- @since 0.2.4
httpLbs :: MonadIO m => H.Request -> m (H.Response L.ByteString)
httpLbs = httpLBS

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set f s = fmap (set s) (f (get s))

-- | Lens for the request method
--
-- @since 0.2.4
requestMethod :: Lens' H.Request S.ByteString
requestMethod = lens H.method (\r x -> r { H.method = x })

-- | Lens for whether this is a secure/HTTPS (@True@) or insecure/HTTP
-- (@False@) request
--
-- @since 0.2.4
requestSecure :: Lens' H.Request Bool
requestSecure = lens H.secure (\r x -> r { H.secure = x })

-- | Lens for the destination host of the request
--
-- @since 0.2.4
requestHost :: Lens' H.Request S.ByteString
requestHost = lens H.host (\r x -> r { H.host = x })

-- | Lens for the destination port of the request
--
-- @since 0.2.4
requestPort :: Lens' H.Request Int
requestPort = lens H.port (\r x -> r { H.port = x })

-- | Lens for the requested path info of the request
--
-- @since 0.2.4
requestPath :: Lens' H.Request S.ByteString
requestPath = lens H.path (\r x -> r { H.path = x })

-- | Lens into all request header values with the given name
--
-- @since 0.2.4
requestHeader :: H.HeaderName -> Lens' H.Request [S.ByteString]
requestHeader name = lens
    (map snd . filter (\(x, _) -> x == name) . H.requestHeaders)
    (\req vals -> req
        { H.requestHeaders =
            filter (\(x, _) -> x /= name) (H.requestHeaders req)
         ++ (map (name, ) vals)
        })

-- | Lens into all request headers
--
-- @since 0.2.4
requestHeaders :: Lens' H.Request [(H.HeaderName, S.ByteString)]
requestHeaders = lens H.requestHeaders (\req x -> req { H.requestHeaders = x })

-- | Lens into the query string
--
-- @since 0.2.4
requestQueryString :: Lens' H.Request [(S.ByteString, Maybe S.ByteString)]
requestQueryString = lens
    (H.parseQuery . H.queryString)
    (flip H.setQueryString)

-- | Set the request body to the given 'H.RequestBody'. You may want to
-- consider using one of the convenience functions in the modules, e.g.
-- 'requestBodyJSON'.
--
-- /Note/: This will not modify the request method. For that, please use
-- 'requestMethod'. You likely don't want the default of @GET@.
--
-- This lens does not allow inspecting the request body
--
-- @since 0.2.4
requestBody :: Lens H.Request H.Request () H.RequestBody
requestBody = lens
    (const ())
    (\req x -> req { H.requestBody = x })

-- | Set the request body as a JSON value
--
-- /Note/: This will not modify the request method. For that, please use
-- 'requestMethod'. You likely don't want the default of @GET@.
--
-- This also sets the @content-type@ to @application/json; chatset=utf8@
--
-- This lens does not allow inspecting the request body
--
-- @since 0.2.4
requestBodyJSON :: A.ToJSON a => Lens H.Request H.Request () a
requestBodyJSON = lens
    (const ())
    (\req x -> req
        { H.requestHeaders
            = (H.hContentType, "application/json; charset=utf-8")
            : filter (\(x, _) -> x /= H.hContentType) (H.requestHeaders req)
        , H.requestBody = H.RequestBodyLBS $ A.encode $ A.toJSON x
        })

-- | Set the request body as a lazy @ByteString@
--
-- /Note/: This will not modify the request method. For that, please use
-- 'requestMethod'. You likely don't want the default of @GET@.
--
-- This lens does not allow inspecting the request body
--
-- @since 0.2.4
requestBodyLBS :: Lens H.Request H.Request () L.ByteString
requestBodyLBS = lens
    (const ())
    (\req x -> req { H.requestBody = H.RequestBodyLBS x })

-- | Set the request body as a 'C.Source'
--
-- /Note/: This will not modify the request method. For that, please use
-- 'requestMethod'. You likely don't want the default of @GET@.
--
-- This lens does not allow inspecting the request body
--
-- @since 0.2.4
requestBodySource :: Lens H.Request H.Request () (Int64, C.Source IO S.ByteString)
requestBodySource = lens
    (const ())
    (\req (len, src) -> req { H.requestBody = HC.requestBodySource len src })

{-
-- | Set the request body as a file
--
-- /Note/: This will not modify the request method. For that, please use
-- 'requestMethod'. You likely don't want the default of @GET@.
--
-- This lens does not allow inspecting the request body
--
-- @since 0.2.4
requestBodyFile :: Lens H.Request H.Request () FilePath
requestBodyFile = _
-}

-- | Set the request body as URL encoded data
--
-- /Note/: This will not modify the request method. For that, please use
-- 'requestMethod'. You likely don't want the default of @GET@.
--
-- This also sets the @content-type@ to @application/x-www-form-urlencoded@
--
-- This lens does not allow inspecting the request body
--
-- @since 0.2.4
requestBodyURLEncoded :: Lens H.Request H.Request () [(S.ByteString, S.ByteString)]
requestBodyURLEncoded = lens
    (const ())
    (\req x -> H.urlEncodedBody x req)

-- | Modify the request so that non-2XX status codes do not generate a runtime
-- exception. If @True@, ignore the status code. If @False@, do the default 2XX
-- check.
--
-- Note that you cannot inspect the current ignore status.
--
-- @since 0.2.4
requestIgnoreStatus :: Lens H.Request H.Request () Bool
requestIgnoreStatus = lens (const ()) (\req b ->
    req { H.checkStatus =
            if b
                then \_ _ _ -> Nothing
                else H.checkStatus def
        })

-- | Set basic auth with the given username and password
--
-- Note that you cannot inspect the username and password after setting
--
-- @since 0.2.4
requestBasicAuth :: Lens H.Request H.Request () (S.ByteString, S.ByteString)
requestBasicAuth = lens
    (const ())
    (\req (user, pass) -> H.applyBasicAuth user pass req)

-- | Instead of using the default global 'H.Manager', use the supplied
-- @Manager@.
--
-- @since 0.2.4
requestManager :: Lens' H.Request (Maybe H.Manager)
requestManager = lens
    HI.requestManagerOverride
    (\req x -> req { HI.requestManagerOverride = x })

-- TODO accessors and setters for various request and response fields. Will
-- this be lens based? Still need to decide on that.
--
-- Minimal functionality:
--
-- * Get response status
-- * Get response status code (just the Int)
-- * Get response headers
-- * Get a response header
-- * Get response body
