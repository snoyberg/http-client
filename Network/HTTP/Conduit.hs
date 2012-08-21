{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
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
-- * Content-Length
--
-- * Host
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
    , Response (..)
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
    , socksProxy
    , rawBody
    , decompress
    , redirectCount
    , checkStatus
    , responseTimeout
      -- * Manager
    , Manager
    , newManager
    , closeManager
    , withManager
      -- ** Settings
    , ManagerSettings
    , managerConnCount
    , managerCheckCerts
      -- *** Defaults
    , defaultCheckCerts
      -- * Cookies
    , Cookie(..)
    , CookieJar
    , createCookieJar
    , destroyCookieJar
    , updateCookieJar
    , receiveSetCookie
    , generateCookie
    , insertCheckedCookie
    , insertCookiesIntoRequest
    , computeCookieString
    , evictExpiredCookies
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

import Control.Exception.Lifted (throwIO)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Control (MonadBaseControl)

import qualified Data.Conduit as C
import qualified Data.Conduit.Internal as CI
import Data.Conduit.Blaze (builderToByteString)
import Data.Conduit (MonadResource)
import Control.Exception.Lifted (try, SomeException)

import Data.Time.Clock

import Network.HTTP.Conduit.Request
import Network.HTTP.Conduit.Response
import Network.HTTP.Conduit.Manager
import Network.HTTP.Conduit.ConnInfo
import Network.HTTP.Conduit.Cookies

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
-- Note: Unlike previous versions, this function will perform redirects, as
-- specified by the 'redirectCount' setting.
http
    :: (MonadResource m, MonadBaseControl IO m)
    => Request m
    -> Manager
    -> m (Response (C.ResumableSource m S.ByteString))
http req0 manager = do
    res@(Response status _version hs body) <-
        if redirectCount req0 == 0
            then httpRaw req0 manager
            else go (redirectCount req0) req0 def []
    case checkStatus req0 status hs of
        Nothing -> return res
        Just exc -> do
            let CI.ResumableSource _ final = body
            final
            liftIO $ throwIO exc
  where
    go (-1) _ _ ress = liftIO . throwIO . TooManyRedirects =<< mapM lbsResponse ress
    go count req'' cookie_jar'' ress = do
        now <- liftIO getCurrentTime
        let (req', cookie_jar') = insertCookiesIntoRequest req'' (evictExpiredCookies cookie_jar'' now) now
        res <- httpRaw req' manager
        let (cookie_jar, _) = updateCookieJar res req' now cookie_jar'
        case getRedirectedRequest req' (responseHeaders res) (W.statusCode (responseStatus res)) of
            Just req -> go (count - 1) req cookie_jar (res:ress)
            Nothing -> return res

-- | Get a 'Response' without any redirect following.
httpRaw
     :: (MonadBaseControl IO m, MonadResource m)
     => Request m
     -> Manager
     -> m (Response (C.ResumableSource m S.ByteString))
httpRaw req m = do
    (connRelease, ci, isManaged) <- getConn req m
    let src = connSource ci

    -- Originally, we would only test for exceptions when sending the request,
    -- not on calling @getResponse@. However, some servers seem to close
    -- connections after accepting the request headers, so we need to check for
    -- exceptions in both.
    ex <- try' $ do
        requestBuilder req C.$$ builderToByteString C.=$ connSink ci
        getResponse connRelease req src

    case (ex, isManaged) of
        -- Connection was reused, and might be been closed. Try again
        (Left _, Reused) -> do
            connRelease DontReuse
            http req m
        -- Not reused, so this is a real exception
        (Left e, Fresh) -> liftIO $ throwIO e
        -- Everything went ok, so the connection is good. If any exceptions get
        -- thrown in the response body, just throw them as normal.
        (Right x, _) -> return x
  where
    try' :: MonadBaseControl IO m => m a -> m (Either SomeException a)
    try' = try

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
httpLbs r = lbsResponse <=< http r

-- | Download the specified URL, following any redirects, and
-- return the response body.
--
-- This function will 'throwIO' an 'HttpException' for any
-- response with a non-2xx status code (besides 3xx redirects up
-- to a limit of 10 redirects). It uses 'parseUrl' to parse the
-- input. This function essentially wraps 'httpLbsRedirect'.
--
-- Note: Even though this function returns a lazy bytestring, it
-- does /not/ utilize lazy I/O, and therefore the entire response
-- body will live in memory. If you want constant memory usage,
-- you'll need to use the @conduit@ package and 'http' or
-- 'httpRedirect' directly.
simpleHttp :: MonadIO m => String -> m L.ByteString
simpleHttp url = liftIO $ withManager $ \man -> do
    url' <- liftIO $ parseUrl url
    fmap responseBody $ httpLbs url' man
