{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module contains everything you need to initiate HTTP connections.  If
-- you want a simple interface based on URLs, you can use 'simpleHttp'. If you
-- want raw power, 'http' is the underlying workhorse of this package. Some
-- examples:
--
-- > -- Just download an HTML document and print it.
-- > import Network.HTTP.Conduit
-- > import qualified Data.ByteString.Lazy as L
-- >
-- > main = simpleHttp "http://www.haskell.org/" >>= L.putStr
--
-- This example uses interleaved IO to write the response body to a file in
-- constant memory space.
--
-- > import Data.Conduit.Binary (sinkFile)
-- > import Network.HTTP.Conduit
-- > import qualified Data.Conduit as C
-- >
-- > main :: IO ()
-- > main = do
-- >      request <- parseUrl "http://google.com/"
-- >      withManager $ \manager -> do
-- >          response <- http request manager
-- >          responseBody response C.$$+- sinkFile "google.html"
--
-- The following headers are automatically set by this module, and should not
-- be added to 'requestHeaders':
--
-- * Cookie
--
-- * Content-Length
--
-- * Transfer-Encoding
--
-- Note: In previous versions, the Host header would be set by this module in
-- all cases. Starting from 1.6.1, if a Host header is present in
-- @requestHeaders@, it will be used in place of the header this module would
-- have generated. This can be useful for calling a server which utilizes
-- virtual hosting.
--
-- Use `cookieJar` If you want to supply cookies with your request:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Network.HTTP.Conduit
-- > import Network
-- > import Data.Time.Clock
-- > import Data.Time.Calendar
-- > import qualified Control.Exception as E
-- >
-- > past :: UTCTime
-- > past = UTCTime (ModifiedJulianDay 56200) (secondsToDiffTime 0)
-- >
-- > future :: UTCTime
-- > future = UTCTime (ModifiedJulianDay 562000) (secondsToDiffTime 0)
-- >
-- > cookie :: Cookie
-- > cookie = Cookie { cookie_name = "password_hash"
-- >                 , cookie_value = "abf472c35f8297fbcabf2911230001234fd2"
-- >                 , cookie_expiry_time = future
-- >                 , cookie_domain = "example.com"
-- >                 , cookie_path = "/"
-- >                 , cookie_creation_time = past
-- >                 , cookie_last_access_time = past
-- >                 , cookie_persistent = False
-- >                 , cookie_host_only = False
-- >                 , cookie_secure_only = False
-- >                 , cookie_http_only = False
-- >                 }
-- >
-- > main = withSocketsDo $ do
-- >      request' <- parseUrl "http://example.com/secret-page"
-- >      let request = request' { cookieJar = Just $ createCookieJar [cookie] }
-- >      E.catch (withManager $ httpLbs request)
-- >              (\(StatusCodeException s _ _) ->
-- >                if statusCode==403 then putStrLn "login failed" else return ())
--
-- Any network code on Windows requires some initialization, and the network
-- library provides withSocketsDo to perform it. Therefore, proper usage of
-- this library will always involve calling that function at some point.  The
-- best approach is to simply call them at the beginning of your main function,
-- such as:
--
-- > import Network.HTTP.Conduit
-- > import qualified Data.ByteString.Lazy as L
-- > import Network (withSocketsDo)
-- >
-- > main = withSocketsDo
-- >      $ simpleHttp "http://www.haskell.org/" >>= L.putStr
-- >
-- > Cookies are implemented according to RFC 6265.
--
-- Note that by default, the functions in this package will throw exceptions
-- for non-2xx status codes. If you would like to avoid this, you should use
-- 'checkStatus', e.g.:
--
-- > import Data.Conduit.Binary (sinkFile)
-- > import Network.HTTP.Conduit
-- > import qualified Data.Conduit as C
-- > import Network
-- >
-- > main :: IO ()
-- > main = withSocketsDo $ do
-- >      request' <- parseUrl "http://www.yesodweb.com/does-not-exist"
-- >      let request = request' { checkStatus = \_ _ -> Nothing }
-- >      res <- withManager $ httpLbs request
-- >      print res
module Network.HTTP.Conduit
    ( -- * Perform a request
      simpleHttp
    , httpLbs
    , http
      -- * Datatypes
    , Proxy (..)
    , RequestBody (..)
      -- ** Request
    , Request
    , def
    , method
    , secure
    , host
    , port
    , path
    , queryString
    , requestHeaders
    , requestBody
    , proxy
    , hostAddress
    , rawBody
    , decompress
    , redirectCount
    , checkStatus
    , responseTimeout
    , cookieJar
    , getConnectionWrapper
      -- * Response
    , Response
    , responseStatus
    , responseVersion
    , responseHeaders
    , responseBody
    , responseCookieJar
      -- * Manager
    , Manager
    , newManager
    , closeManager
    , withManager
    , withManagerSettings
      -- ** Settings
    , ManagerSettings
    , conduitManagerSettings
    , managerConnCount
    , managerResponseTimeout
    , managerTlsConnection
    , getTlsConnection
      -- * Cookies
    , Cookie(..)
    , CookieJar
    , createCookieJar
    , destroyCookieJar
      -- * Utility functions
    , parseUrl
    , applyBasicAuth
    , addProxy
    , lbsResponse
    , getRedirectedRequest
      -- * Decompression predicates
    , alwaysDecompress
    , browserDecompress
      -- * Request bodies
      -- | "Network.HTTP.Conduit.MultipartFormData" provides an API for building
      -- form-data request bodies.
    , urlEncodedBody
      -- * Exceptions
    , HttpException (..)
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import qualified Network.HTTP.Types as W
import Data.Default (def)

import Control.Exception.Lifted (throwIO, try, IOException, handle, fromException, toException, bracket)
import qualified Network.TLS as TLS
import Control.Applicative
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource

import qualified Data.Conduit as C
import qualified Data.Conduit.Internal as CI
import Data.Conduit.Blaze (builderToByteString)

import Data.Time.Clock
import Network.Socket (HostAddress)

import Network.HTTP.Client.Request
import Network.HTTP.Client.Connection (makeConnection)
import Network.HTTP.Client.Body
import Network.HTTP.Client.Response
import Network.HTTP.Client.Manager
import Network.HTTP.Client.Cookies
import Network.HTTP.Client.Types
import qualified Network.HTTP.Client as Client
import qualified Network.Connection as NC

-- | Download the specified 'Request', returning the results as a 'Response'.
--
-- This is a simplified version of 'http' for the common case where you simply
-- want the response data as a simple datatype. If you want more power, such as
-- interleaved actions on the response body during download, you'll need to use
-- 'http' directly. This function is defined as:
--
-- @httpLbs = 'lbsResponse' <=< 'http'@
--
-- Even though the 'Response' contains a lazy bytestring, this
-- function does /not/ utilize lazy I/O, and therefore the entire
-- response body will live in memory. If you want constant memory
-- usage, you'll need to use @conduit@ packages's
-- 'C.Source' returned by 'http'.
--
-- Note: Unlike previous versions, this function will perform redirects, as
-- specified by the 'redirectCount' setting.
httpLbs :: MonadIO m => Request -> Manager -> m (Response L.ByteString)
httpLbs r m = liftIO $ Client.httpLbs r m

-- | Download the specified URL, following any redirects, and
-- return the response body.
--
-- This function will 'throwIO' an 'HttpException' for any
-- response with a non-2xx status code (besides 3xx redirects up
-- to a limit of 10 redirects). It uses 'parseUrl' to parse the
-- input. This function essentially wraps 'httpLbs'.
--
-- Note: Even though this function returns a lazy bytestring, it
-- does /not/ utilize lazy I/O, and therefore the entire response
-- body will live in memory. If you want constant memory usage,
-- you'll need to use the @conduit@ package and 'http' directly.
--
-- Note: This function creates a new 'Manager'. It should be avoided
-- in production code.
simpleHttp :: MonadIO m => String -> m L.ByteString
simpleHttp url = liftIO $ withManager $ \man -> do
    req <- liftIO $ parseUrl url
    responseBody <$> httpLbs (setConnectionClose req) man

getTlsConnection :: Maybe NC.TLSSettings
                 -> Maybe NC.SockSettings
                 -> IO (Maybe HostAddress -> String -> Int -> IO Connection)
getTlsConnection tls sock = do
    context <- NC.initConnectionContext
    return $ \_ha host port -> do
        conn <- NC.connectTo context NC.ConnectionParams
            { NC.connectionHostname = host
            , NC.connectionPort = fromIntegral port
            , NC.connectionUseSecure = tls
            , NC.connectionUseSocks = sock
            }
        convertConnection conn
  where
    convertConnection conn = makeConnection
        (NC.connectionGetChunk conn)
        (NC.connectionPut conn)
        (NC.connectionClose conn)

conduitManagerSettings :: ManagerSettings
conduitManagerSettings = defaultManagerSettings
    { managerTlsConnection = getTlsConnection (Just def) Nothing
    , managerRetryableException = \e ->
        case () of
            ()
                | ((fromException e)::(Maybe TLS.TLSError))==Just TLS.Error_EOF -> True
                | otherwise -> case fromException e of
                    Just (_ :: IOException) -> True
                    _ ->
                        case fromException e of
                            -- Note: Some servers will timeout connections by accepting
                            -- the incoming packets for the new request, but closing
                            -- the connection as soon as we try to read. To make sure
                            -- we open a new connection under these circumstances, we
                            -- check for the NoResponseDataReceived exception.
                            Just NoResponseDataReceived -> True
                            _ -> False
    , managerWrapIOException =
        let wrapper se =
                case fromException se of
                    Just e -> toException $ InternalIOException e
                    Nothing ->
                        case fromException se of
                            Just TLS.Terminated{} -> toException $ TlsException se
                            Nothing ->
                                case fromException se of
                                    Just TLS.HandshakeFailed{} -> toException $ TlsException se
                                    Nothing ->
                                        case fromException se of
                                            Just TLS.ConnectionNotEstablished -> toException $ TlsException se
                                            Nothing -> se
         in handle $ throwIO . wrapper
    }

withManager :: (MonadIO m, MonadBaseControl IO m)
            => (Manager -> ResourceT m a)
            -> m a
withManager = withManagerSettings conduitManagerSettings

withManagerSettings :: (MonadIO m, MonadBaseControl IO m)
                    => ManagerSettings
                    -> (Manager -> ResourceT m a)
                    -> m a
withManagerSettings set f = bracket
    (liftIO $ newManager set)
    (liftIO . closeManager)
    (runResourceT . f)

setConnectionClose :: Request -> Request
setConnectionClose req = req{requestHeaders = ("Connection", "close") : requestHeaders req}

http :: MonadResource m
     => Request
     -> Manager
     -> m (Response (C.ResumableSource m S.ByteString))
http req man = do
    (key, res) <- allocate (Client.responseOpen req man) responseClose
    let rsrc = CI.ResumableSource
            (brSource $ responseBody res)
            (release key)
    return res { responseBody = rsrc }
  where
    brSource :: BodyReader -> C.Source m S.ByteString
    brSource = undefined
