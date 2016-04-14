{-# LANGUAGE DeriveDataTypeable #-}
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
      -- * Alternate spellings
    , httpLbs
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as H
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON (..), Value)
import Data.Aeson.Parser (json')
import qualified Data.Aeson.Types as A
import qualified Data.Traversable as T
import Control.Exception (throwIO, Exception)
import Data.Typeable (Typeable)
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as C
import Control.Monad.Catch (MonadMask, bracket)

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

-- | Perform an HTTP request and consume the body with the given 'C.Sink'
--
-- @since 0.2.4
httpSink :: (MonadIO m, MonadMask m)
         => H.Request
         -> (H.Response () -> C.Sink S.ByteString m a)
         -> m (H.Response a)
httpSink req sink = do
    man <- liftIO H.getGlobalManager
    bracket
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

-- TODO accessors and setters for various request and response fields. Will
-- this be lens based? Still need to decide on that.
--
-- Minimal functionality:
--
-- * Set request method
-- * Set JSON request body
-- * Set LBS request body
-- * Set Source request body
-- * Set file request body
-- * Add request headers
-- * Set URL encoded body
-- * Disable exceptions on non-2XX
-- * applyBasicAuth
-- * Add to query string (or just set the whole thing?)
-- * Set request: secure, host, port, path
--
-- * Get response status
-- * Get response status code (just the Int)
-- * Get response headers
-- * Get a response header
-- * Get response body
