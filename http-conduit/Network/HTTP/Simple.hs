{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Simplified interface for common HTTP client interactions. Tutorial
-- available at
-- <https://haskell-lang.org/library/http-client>
--
-- Important note: 'H.Request' is an instance of 'Data.String.IsString', and
-- therefore recommended usage is to turn on @OverloadedStrings@, e.g.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Network.HTTP.Simple
-- > import qualified Data.ByteString.Char8 as B8
-- >
-- > main :: IO ()
-- > main = httpBS "http://example.com" >>= B8.putStrLn . getResponseBody
--
-- The `Data.String.IsString` instance uses `H.parseRequest` behind the scenes and inherits its behavior.
module Network.HTTP.Simple
    ( -- * Perform requests
      httpBS
    , httpLBS
    , httpNoBody
#ifdef VERSION_aeson
    , httpJSON
    , httpJSONEither
#endif
    , httpSink
    , httpSource
    , withResponse
      -- * Types
    , H.Header
    , H.Query
    , H.QueryItem
    , H.Request
    , H.RequestHeaders
    , H.Response
    , H.ResponseHeaders
#ifdef VERSION_aeson
    , JSONException (..)
#endif
    , H.HttpException (..)
    , H.Proxy (..)
      -- * Request constructions
    , H.defaultRequest
    , H.parseRequest
    , H.parseRequest_
    , parseRequestThrow
    , parseRequestThrow_
      -- * Request lenses
      -- ** Basics
    , setRequestMethod
    , setRequestSecure
    , setRequestHost
    , setRequestPort
    , setRequestPath
    , addRequestHeader
    , getRequestHeader
    , setRequestHeader
    , setRequestHeaders
    , setRequestQueryString
    , getRequestQueryString
    , addToRequestQueryString
      -- ** Request body
    , setRequestBody
#ifdef VERSION_aeson
    , setRequestBodyJSON
#endif
    , setRequestBodyLBS
    , setRequestBodySource
    , setRequestBodyFile
    , setRequestBodyURLEncoded
      -- ** Special fields
    , H.setRequestIgnoreStatus
    , H.setRequestCheckStatus
    , setRequestBasicAuth
#if MIN_VERSION_http_client(0,7,6)
    , setRequestBearerAuth
#endif
    , setRequestManager
    , setRequestProxy
    , setRequestResponseTimeout
      -- * Response lenses
    , getResponseStatus
    , getResponseStatusCode
    , getResponseHeader
    , getResponseHeaders
    , getResponseBody
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
import Control.Monad.IO.Unlift (MonadIO, liftIO, MonadUnliftIO, withRunInIO)

#ifdef VERSION_aeson
import Data.Aeson (FromJSON (..), Value)
import Data.Aeson.Parser (json')
import qualified Data.Aeson.Types as A
import qualified Data.Aeson as A
#endif

import qualified Data.Traversable as T
import Control.Exception (throw, throwIO, Exception)
import Data.Typeable (Typeable)
import qualified Data.Conduit as C
import Data.Conduit (runConduit, (.|), ConduitM)
import qualified Data.Conduit.Attoparsec as C
import qualified Network.HTTP.Types as H
import Data.Int (Int64)
import Control.Monad.Trans.Resource (MonadResource, MonadThrow)
import qualified Control.Exception as E (bracket)
import Data.Void (Void)
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.Attoparsec.ByteString.Char8 as Atto8

-- | Perform an HTTP request and return the body as a @ByteString@.
--
-- @since 2.2.4
httpBS :: MonadIO m => H.Request -> m (H.Response S.ByteString)
httpBS req = liftIO $ do
    man <- H.getGlobalManager
    fmap L.toStrict `fmap` H.httpLbs req man

-- | Perform an HTTP request and return the body as a lazy
-- @ByteString@. Note that the entire value will be read into memory
-- at once (no lazy I\/O will be performed). The advantage of a lazy
-- @ByteString@ here (versus using 'httpBS') is--if needed--a better
-- in-memory representation.
--
-- @since 2.1.10
httpLBS :: MonadIO m => H.Request -> m (H.Response L.ByteString)
httpLBS req = liftIO $ do
    man <- H.getGlobalManager
    H.httpLbs req man

-- | Perform an HTTP request and ignore the response body.
--
-- @since 2.2.2
httpNoBody :: MonadIO m => H.Request -> m (H.Response ())
httpNoBody req = liftIO $ do
    man <- H.getGlobalManager
    H.httpNoBody req man

#ifdef VERSION_aeson
-- | Perform an HTTP request and parse the body as JSON. In the event of an
-- JSON parse errors, a 'JSONException' runtime exception will be thrown.
--
-- NOTE: Depends on the @aeson@ cabal flag being enabled
--
-- @since 2.1.10
httpJSON :: (MonadIO m, FromJSON a) => H.Request -> m (H.Response a)
httpJSON req = liftIO $ httpJSONEither req >>= T.mapM (either throwIO return)

-- | Perform an HTTP request and parse the body as JSON. In the event of an
-- JSON parse errors, a @Left@ value will be returned.
--
-- NOTE: Depends on the @aeson@ cabal flag being enabled
--
-- @since 2.1.10
httpJSONEither :: (MonadIO m, FromJSON a)
               => H.Request
               -> m (H.Response (Either JSONException a))
httpJSONEither req = liftIO $ httpSink req' sink
  where
    req' = addRequestHeader H.hAccept "application/json" req
    sink orig = fmap (\x -> fmap (const x) orig) $ do
        eres1 <- C.sinkParserEither (json' <* (Atto8.skipSpace *> Atto.endOfInput))

        case eres1 of
            Left e -> return $ Left $ JSONParseException req' orig e
            Right value ->
                case A.fromJSON value of
                    A.Error e -> return $ Left $ JSONConversionException
                        req' (fmap (const value) orig) e
                    A.Success x -> return $ Right x

-- | An exception that can occur when parsing JSON
--
-- NOTE: Depends on the @aeson@ cabal flag being enabled
--
-- @since 2.1.10
data JSONException
    = JSONParseException H.Request (H.Response ()) C.ParseError
    | JSONConversionException H.Request (H.Response Value) String
  deriving (Show, Typeable)
instance Exception JSONException
#endif

-- | Perform an HTTP request and consume the body with the given 'C.Sink'
--
-- @since 2.1.10
httpSink :: MonadUnliftIO m
         => H.Request
         -> (H.Response () -> ConduitM S.ByteString Void m a)
         -> m a
httpSink req sink = withRunInIO $ \run -> do
    man <- H.getGlobalManager
    E.bracket
        (H.responseOpen req man)
        H.responseClose
        $ \res -> run
            $ runConduit
            $ bodyReaderSource (getResponseBody res)
           .| sink (fmap (const ()) res)

-- | Perform an HTTP request, and get the response body as a Source.
--
-- The second argument to this function tells us how to make the
-- Source from the Response itself. This allows you to perform actions
-- with the status or headers, for example, in addition to the raw
-- bytes themselves. If you just care about the response body, you can
-- use 'getResponseBody' as the second argument here.
--
-- @
-- \{\-# LANGUAGE OverloadedStrings \#\-}
-- import           Control.Monad.IO.Class       (liftIO)
-- import           Control.Monad.Trans.Resource (runResourceT)
-- import           Data.Conduit                 (($$))
-- import qualified Data.Conduit.Binary          as CB
-- import qualified Data.Conduit.List            as CL
-- import           Network.HTTP.Simple
-- import           System.IO                    (stdout)
--
-- main :: IO ()
-- main =
--     runResourceT
--         $ httpSource "http://httpbin.org/robots.txt" getSrc
--        $$ CB.sinkHandle stdout
--   where
--     getSrc res = do
--         liftIO $ print (getResponseStatus res, getResponseHeaders res)
--         getResponseBody res
-- @
--
-- @since 2.2.1
httpSource :: (MonadResource m, MonadIO n)
           => H.Request
           -> (H.Response (C.ConduitM i S.ByteString n ())
                -> C.ConduitM i o m r)
           -> C.ConduitM i o m r
httpSource req withRes = do
    man <- liftIO H.getGlobalManager
    C.bracketP (H.responseOpen req man) H.responseClose
        (withRes . fmap bodyReaderSource)

-- | Perform an action with the given request. This employes the
-- bracket pattern.
--
-- This is similar to 'httpSource', but does not require
-- 'MonadResource' and allows the result to not contain a 'C.ConduitM'
-- value.
--
-- @since 2.2.3
withResponse :: (MonadUnliftIO m, MonadIO n)
             => H.Request
             -> (H.Response (C.ConduitM i S.ByteString n ()) -> m a)
             -> m a
withResponse req withRes = withRunInIO $ \run -> do
    man <- H.getGlobalManager
    E.bracket
        (H.responseOpen req man)
        H.responseClose
        (run . withRes . fmap bodyReaderSource)

-- | Same as 'parseRequest', except will throw an 'HttpException' in the
-- event of a non-2XX response. This uses 'throwErrorStatusCodes' to
-- implement 'checkResponse'.
--
-- Exactly the same as 'parseUrlThrow', but has a name that is more
-- consistent with the other parseRequest functions.
--
-- @since 2.3.2
parseRequestThrow :: MonadThrow m => String -> m HC.Request
parseRequestThrow = HC.parseUrlThrow

-- | Same as 'parseRequestThrow', but parse errors cause an impure
-- exception. Mostly useful for static strings which are known to be
-- correctly formatted.
--
-- @since 2.3.2
parseRequestThrow_ :: String -> HC.Request
parseRequestThrow_ = either throw id . HC.parseUrlThrow

-- | Alternate spelling of 'httpLBS'
--
-- @since 2.1.10
httpLbs :: MonadIO m => H.Request -> m (H.Response L.ByteString)
httpLbs = httpLBS

-- | Set the request method
--
-- @since 2.1.10
setRequestMethod :: S.ByteString -> H.Request -> H.Request
setRequestMethod x req = req { H.method = x }

-- | Set whether this is a secure/HTTPS (@True@) or insecure/HTTP
-- (@False@) request
--
-- @since 2.1.10
setRequestSecure :: Bool -> H.Request -> H.Request
setRequestSecure x req = req { H.secure = x }

-- | Set the destination host of the request
--
-- @since 2.1.10
setRequestHost :: S.ByteString -> H.Request -> H.Request
setRequestHost x r = r { H.host = x }

-- | Set the destination port of the request
--
-- @since 2.1.10
setRequestPort :: Int -> H.Request -> H.Request
setRequestPort x r = r { H.port = x }

-- | Lens for the requested path info of the request
--
-- @since 2.1.10
setRequestPath :: S.ByteString -> H.Request -> H.Request
setRequestPath x r = r { H.path = x }

-- | Add a request header name/value combination
--
-- @since 2.1.10
addRequestHeader :: H.HeaderName -> S.ByteString -> H.Request -> H.Request
addRequestHeader name val req =
    req { H.requestHeaders = (name, val) : H.requestHeaders req }

-- | Get all request header values for the given name
--
-- @since 2.1.10
getRequestHeader :: H.HeaderName -> H.Request -> [S.ByteString]
getRequestHeader name =
    map snd . filter (\(x, _) -> x == name) . H.requestHeaders

-- | Set the given request header to the given list of values. Removes any
-- previously set header values with the same name.
--
-- @since 2.1.10
setRequestHeader :: H.HeaderName -> [S.ByteString] -> H.Request -> H.Request
setRequestHeader name vals req =
    req { H.requestHeaders =
            filter (\(x, _) -> x /= name) (H.requestHeaders req)
         ++ (map (name, ) vals)
        }

-- | Set the request headers, wiping out __all__ previously set headers. This
-- means if you use 'setRequestHeaders' to set some headers and also use one of
-- the other setters that modifies the @content-type@ header (such as
-- 'setRequestBodyJSON'), be sure that 'setRequestHeaders' is evaluated
-- __first__.
--
-- @since 2.1.10
setRequestHeaders :: H.RequestHeaders -> H.Request -> H.Request
setRequestHeaders x req = req { H.requestHeaders = x }

-- | Get the query string parameters
--
-- @since 2.1.10
getRequestQueryString :: H.Request -> H.Query
getRequestQueryString = H.parseQuery . H.queryString

-- | Set the query string parameters
--
-- @since 2.1.10
setRequestQueryString :: H.Query -> H.Request -> H.Request
setRequestQueryString = H.setQueryString

-- | Add to the existing query string parameters.
--
-- @since 2.3.5
addToRequestQueryString :: H.Query -> H.Request -> H.Request
addToRequestQueryString additions req = setRequestQueryString q req
    where q = additions <> getRequestQueryString req

-- | Set the request body to the given 'H.RequestBody'. You may want to
-- consider using one of the convenience functions in the modules, e.g.
-- 'requestBodyJSON'.
--
-- /Note/: This will not modify the request method. For that, please use
-- 'requestMethod'. You likely don't want the default of @GET@.
--
-- @since 2.1.10
setRequestBody :: H.RequestBody -> H.Request -> H.Request
setRequestBody x req = req { H.requestBody = x }

#ifdef VERSION_aeson
-- | Set the request body as a JSON value
--
-- /Note/: This will not modify the request method. For that, please use
-- 'requestMethod'. You likely don't want the default of @GET@.
--
-- This also sets the @Content-Type@ to @application/json; charset=utf-8@
--
-- NOTE: Depends on the @aeson@ cabal flag being enabled
--
-- @since 2.1.10
setRequestBodyJSON :: A.ToJSON a => a -> H.Request -> H.Request
setRequestBodyJSON x req =
    req { H.requestHeaders
            = (H.hContentType, "application/json; charset=utf-8")
            : filter (\(y, _) -> y /= H.hContentType) (H.requestHeaders req)
        , H.requestBody = H.RequestBodyLBS $ A.encode x
        }
#endif

-- | Set the request body as a lazy @ByteString@
--
-- /Note/: This will not modify the request method. For that, please use
-- 'requestMethod'. You likely don't want the default of @GET@.
--
-- @since 2.1.10
setRequestBodyLBS :: L.ByteString -> H.Request -> H.Request
setRequestBodyLBS = setRequestBody . H.RequestBodyLBS

-- | Set the request body as a 'C.Source'
--
-- /Note/: This will not modify the request method. For that, please use
-- 'requestMethod'. You likely don't want the default of @GET@.
--
-- @since 2.1.10
setRequestBodySource :: Int64 -- ^ length of source
                     -> ConduitM () S.ByteString IO ()
                     -> H.Request
                     -> H.Request
setRequestBodySource len src req = req { H.requestBody = HC.requestBodySource len src }

-- | Set the request body as a file
--
-- /Note/: This will not modify the request method. For that, please use
-- 'requestMethod'. You likely don't want the default of @GET@.
--
-- @since 2.1.10
setRequestBodyFile :: FilePath -> H.Request -> H.Request
setRequestBodyFile = setRequestBody . HI.RequestBodyIO . H.streamFile

-- | Set the request body as URL encoded data
--
-- /Note/: This will change the request method to @POST@ and set the @content-type@
-- to @application/x-www-form-urlencoded@
--
-- @since 2.1.10
setRequestBodyURLEncoded :: [(S.ByteString, S.ByteString)] -> H.Request -> H.Request
setRequestBodyURLEncoded = H.urlEncodedBody

-- | Set basic auth with the given username and password
--
-- @since 2.1.10
setRequestBasicAuth :: S.ByteString -- ^ username
                    -> S.ByteString -- ^ password
                    -> H.Request
                    -> H.Request
setRequestBasicAuth = H.applyBasicAuth

#if MIN_VERSION_http_client(0,7,6)
-- | Set bearer auth with the given token
--
-- @since 2.3.8
setRequestBearerAuth :: S.ByteString -- ^ token
                    -> H.Request
                    -> H.Request
setRequestBearerAuth = H.applyBearerAuth
#endif

-- | Instead of using the default global 'H.Manager', use the supplied
-- @Manager@.
--
-- @since 2.1.10
setRequestManager :: H.Manager -> H.Request -> H.Request
setRequestManager x req = req { HI.requestManagerOverride = Just x }

-- | Override the default proxy server settings
--
-- @since 2.1.10
setRequestProxy :: Maybe H.Proxy -> H.Request -> H.Request
setRequestProxy x req = req { H.proxy = x }

-- | Set the maximum time to wait for a response
--
-- @since 2.3.8
setRequestResponseTimeout :: H.ResponseTimeout -> H.Request -> H.Request
setRequestResponseTimeout x req = req { H.responseTimeout = x }

-- | Get the status of the response
--
-- @since 2.1.10
getResponseStatus :: H.Response a -> H.Status
getResponseStatus = H.responseStatus

-- | Get the integral status code of the response
--
-- @since 2.1.10
getResponseStatusCode :: H.Response a -> Int
getResponseStatusCode = H.statusCode . getResponseStatus

-- | Get all response header values with the given name
--
-- @since 2.1.10
getResponseHeader :: H.HeaderName -> H.Response a -> [S.ByteString]
getResponseHeader name = map snd . filter (\(x, _) -> x == name) . H.responseHeaders

-- | Get all response headers
--
-- @since 2.1.10
getResponseHeaders :: H.Response a -> [(H.HeaderName, S.ByteString)]
getResponseHeaders = H.responseHeaders

-- | Get the response body
--
-- @since 2.1.10
getResponseBody :: H.Response a -> a
getResponseBody = H.responseBody
