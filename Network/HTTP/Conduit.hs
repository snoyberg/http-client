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
-- >          Response _ _ _ src <- http request manager
-- >          src C.$$+- sinkFile "google.html"
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
    , clientCertificates
    , host
    , port
    , path
    , queryString
    , requestHeaders
    , requestBody
    , proxy
    , socksProxy
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
    , managerConnCount
    , managerCheckCerts
    , managerCertStore
      -- *** Defaults
    , defaultCheckCerts
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
#if DEBUG
      -- * Debug
    , printOpenSockets
#endif
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import qualified Network.HTTP.Types as W
import Data.Default (def)

import Control.Exception.Lifted (throwIO, try, IOException, handle, fromException)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource

import qualified Data.Conduit as C
import Data.Conduit.Blaze (builderToByteString)

import Data.Time.Clock

import Network.HTTP.Conduit.Request
import Network.HTTP.Conduit.Response
import Network.HTTP.Conduit.Manager
import Network.HTTP.Conduit.ConnInfo
import Network.HTTP.Conduit.Cookies
import Network.HTTP.Conduit.Internal (httpRedirect, applyCheckStatus)
import Network.HTTP.Conduit.Types

-- | The most low-level function for initiating an HTTP request.
--
-- The first argument to this function gives a full specification
-- on the request: the host to connect to, whether to use SSL,
-- headers, etc. Please see 'Request' for full details.  The
-- second argument specifies which 'Manager' should be used.
--
-- This function then returns a 'Response' with a
-- 'C.Source'.  The 'Response' contains the status code
-- and headers that were sent back to us, and the
-- 'C.Source' contains the body of the request.  Note
-- that this 'C.Source' allows you to have fully
-- interleaved IO actions during your HTTP download, making it
-- possible to download very large responses in constant memory.
-- You may also directly connect the returned 'C.Source'
-- into a 'C.Sink', perhaps a file or another socket.
--
-- An important note: the response body returned by this function represents a
-- live HTTP connection. As such, if you do not use the response body, an open
-- socket will be retained until the containing @ResourceT@ block exits. If you
-- do not need the response body, it is recommended that you explicitly shut
-- down the connection immediately, using the pattern:
--
-- > responseBody res $$+- return ()
--
-- As a more thorough example, consider the following program. Without the
-- explicit response body closing, the program will run out of file descriptors
-- around the 1000th request (depending on the operating system limits).
--
-- > import Control.Monad          (replicateM_)
-- > import Control.Monad.IO.Class (liftIO)
-- > import Data.Conduit           (($$+-))
-- > import Network                (withSocketsDo)
-- > import Network.HTTP.Conduit
-- >
-- > main = withSocketsDo $ withManager $ \manager -> do
-- >     req <- parseUrl "http://localhost/"
-- >     mapM_ (worker manager req) [1..5000]
-- >
-- > worker manager req i = do
-- >     res <- http req manager
-- >     responseBody res $$+- return () -- The important line
-- >     liftIO $ print (i, responseStatus res)
--
-- Note: Unlike previous versions, this function will perform redirects, as
-- specified by the 'redirectCount' setting.
http
    :: (MonadResource m, MonadBaseControl IO m)
    => Request m
    -> Manager
    -> m (Response (C.ResumableSource m S.ByteString))
http req0 manager = wrapIOException $ do
    res <-
        if redirectCount req0 == 0
            then httpRaw req0 manager
            else go (redirectCount req0) req0
    maybe (return res) throwIO =<< applyCheckStatus (checkStatus req0) res
  where
    go count req' = httpRedirect
      count
      (\req -> do
        res <- httpRaw req manager
        let mreq = getRedirectedRequest req (responseHeaders res) (responseCookieJar res) (W.statusCode (responseStatus res))
        return (res, mreq))
      id
      req'

-- | Get a 'Response' without any redirect following.
httpRaw
     :: (MonadBaseControl IO m, MonadResource m)
     => Request m
     -> Manager
     -> m (Response (C.ResumableSource m S.ByteString))
httpRaw req' m = do
    (req, cookie_jar') <- case cookieJar req' of
        Just cj -> do
            now <- liftIO getCurrentTime
            return $ insertCookiesIntoRequest req' (evictExpiredCookies cj now) now
        Nothing -> return (req', def)
    (timeout', (connRelease, ci, isManaged)) <- getConnectionWrapper
        req
        (responseTimeout req)
        (failedConnectionException req)
        (getConn req m)
    let src = connSource ci

    -- Originally, we would only test for exceptions when sending the request,
    -- not on calling @getResponse@. However, some servers seem to close
    -- connections after accepting the request headers, so we need to check for
    -- exceptions in both.
    ex <- try $ do
        requestBuilder req C.$$ builderToByteString C.=$ connSink ci

        getResponse connRelease timeout' req src

    case (ex, isManaged) of
        -- Connection was reused, and might have been closed. Try again
        (Left e, Reused) | isRetryableException e -> do
            connRelease DontReuse
            http req m
        -- Not reused, or a non-retry, so this is a real exception
        (Left e, _) -> liftIO $ throwIO e
        -- Everything went ok, so the connection is good. If any exceptions get
        -- thrown in the response body, just throw them as normal.
        (Right res, _) -> case cookieJar req' of
            Just _ -> do
                now' <- liftIO getCurrentTime
                let (cookie_jar, _) = updateCookieJar res req now' cookie_jar'
                return $ res {responseCookieJar = cookie_jar}
            Nothing -> return res
  where

    -- Exceptions for which we should retry our request if we were reusing an
    -- already open connection. In the case of IOExceptions, for example, we
    -- assume that the connection was closed on the server and therefore open a
    -- new one.
    isRetryableException e =
        case fromException e of
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
httpLbs :: (MonadBaseControl IO m, MonadResource m) => Request m -> Manager -> m (Response L.ByteString)
httpLbs r = wrapIOException . (lbsResponse <=< http r)

wrapIOException :: MonadBaseControl IO m => m a -> m a
wrapIOException = handle $ throwIO . InternalIOException

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
    url' <- liftIO $ parseUrl url
    fmap responseBody $ httpLbs url' man
