{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- |
--
-- = Simpler API
--
-- The API below is rather low-level. The "Network.HTTP.Simple" module provides
-- a higher-level API with built-in support for things like JSON request and
-- response bodies. For most users, this will be an easier place to start. You
-- can read the tutorial at:
--
-- <https://haskell-lang.org/library/http-client>
--
-- = Lower-level API
--
-- This module contains everything you need to initiate HTTP connections.  If
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
-- > import Data.Conduit.Binary (sinkFile) -- Exported from the package conduit-extra
-- > import Network.HTTP.Conduit
-- > import qualified Data.Conduit as C
-- > import Control.Monad.Trans.Resource (runResourceT)
-- >
-- > main :: IO ()
-- > main = do
-- >      request <- parseRequest "http://google.com/"
-- >      manager <- newManager tlsManagerSettings
-- >      runResourceT $ do
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
-- > import Network.HTTP.Types.Status (statusCode)
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
-- > main = do
-- >      request' <- parseRequest "http://example.com/secret-page"
-- >      manager <- newManager tlsManagerSettings
-- >      let request = request' { cookieJar = Just $ createCookieJar [cookie] }
-- >      fmap Just (httpLbs request manager) `E.catch`
-- >              (\ex -> case ex of
-- >                  HttpExceptionRequest _ (StatusCodeException res _) ->
-- >                      if statusCode (responseStatus res) == 403
-- >                        then (putStrLn "login failed" >> return Nothing)
-- >                        else return Nothing
-- >                  _ -> E.throw ex)
--
-- Cookies are implemented according to RFC 6265.
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
-- > main = do
-- >      request' <- parseRequest "http://www.yesodweb.com/does-not-exist"
-- >      let request = request' { checkStatus = \_ _ _ -> Nothing }
-- >      manager <- newManager tlsManagerSettings
-- >      res <- httpLbs request manager
-- >      print res
--
-- By default, when connecting to websites using HTTPS, functions in this
-- package will throw an exception if the TLS certificate doesn't validate. To
-- continue the HTTPS transaction even if the TLS cerficate validation fails,
-- you should use 'mkManagerSetttings' as follows:
--
-- > import Network.Connection (TLSSettings (..))
-- > import Network.HTTP.Conduit
-- >
-- > main :: IO ()
-- > main = do
-- >     request <- parseRequest "https://github.com/"
-- >     let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
-- >     manager <- newManager settings
-- >     res <- httpLbs request manager
-- >     print res
--
-- For more information, please be sure to read the documentation in the
-- "Network.HTTP.Client" module.

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
    , checkResponse
    , responseTimeout
    , cookieJar
    , requestVersion
    , HCC.setQueryString
      -- *** Request body
    , requestBodySource
    , requestBodySourceChunked
    , requestBodySourceIO
    , requestBodySourceChunkedIO
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
    , tlsManagerSettings
    , mkManagerSettings
    , managerConnCount
    , managerResponseTimeout
    , managerTlsConnection
      -- ** Response timeout
    , HC.ResponseTimeout
    , HC.responseTimeoutMicro
    , HC.responseTimeoutNone
    , HC.responseTimeoutDefault
      -- * Cookies
    , Cookie(..)
    , CookieJar
    , createCookieJar
    , destroyCookieJar
      -- * Utility functions
    , parseUrl
    , parseUrlThrow
    , parseRequest
    , parseRequest_
    , defaultRequest
    , applyBasicAuth
    , addProxy
    , lbsResponse
    , getRedirectedRequest
      -- * Decompression predicates
    , alwaysDecompress
    , browserDecompress
      -- * Request bodies
      -- | "Network.HTTP.Client.MultipartFormData" provides an API for building
      -- form-data request bodies.
    , urlEncodedBody
      -- * Exceptions
    , HttpException (..)
    , HCC.HttpExceptionContent (..)
    ) where

import qualified Data.ByteString              as S
import qualified Data.ByteString.Lazy         as L
import           Data.Conduit                 (ResumableSource, ($$+-), await, ($$++), ($$+), Source, addCleanup)
import qualified Data.Conduit.Internal        as CI
import qualified Data.Conduit.List            as CL
import           Data.IORef                   (readIORef, writeIORef, newIORef)
import           Data.Int                     (Int64)
import           Control.Applicative          as A ((<$>))
import           Control.Monad.IO.Unlift      (MonadIO (liftIO), MonadUnliftIO)
import           Control.Monad.Trans.Resource

import qualified Network.HTTP.Client          as Client (httpLbs, responseOpen, responseClose)
import qualified Network.HTTP.Client          as HC
import qualified Network.HTTP.Client.Conduit  as HCC
import           Network.HTTP.Client.Internal (createCookieJar,
                                               destroyCookieJar)
import           Network.HTTP.Client.Internal (Manager, ManagerSettings,
                                               closeManager, managerConnCount,
                                               managerResponseTimeout,
                                               managerTlsConnection, newManager)
import           Network.HTTP.Client          (parseUrl, parseUrlThrow, urlEncodedBody, applyBasicAuth,
                                               defaultRequest, parseRequest, parseRequest_)
import           Network.HTTP.Client.Internal (addProxy, alwaysDecompress,
                                               browserDecompress)
import           Network.HTTP.Client.Internal (getRedirectedRequest)
import           Network.HTTP.Client.TLS      (mkManagerSettings,
                                               tlsManagerSettings)
import           Network.HTTP.Client.Internal (Cookie (..), CookieJar (..),
                                               HttpException (..), Proxy (..),
                                               Request (..), RequestBody (..),
                                               Response (..))

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
-- This function will 'throwIO' an 'HttpException' for any
-- response with a non-2xx status code (besides 3xx redirects up
-- to a limit of 10 redirects). This behavior can be modified by
-- changing the 'checkStatus' field of your request.
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
-- to a limit of 10 redirects). It uses 'parseUrlThrow' to parse the
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
simpleHttp url = liftIO $ do
    man <- newManager tlsManagerSettings
    req <- liftIO $ parseUrlThrow url
    responseBody A.<$> httpLbs (setConnectionClose req) man

conduitManagerSettings :: ManagerSettings
conduitManagerSettings = tlsManagerSettings
{-# DEPRECATED conduitManagerSettings "Use tlsManagerSettings" #-}

withManager :: MonadUnliftIO m
            => (Manager -> ResourceT m a)
            -> m a
withManager = withManagerSettings tlsManagerSettings
{-# DEPRECATED withManager "Please use newManager tlsManagerSettings" #-}

withManagerSettings :: MonadUnliftIO m
                    => ManagerSettings
                    -> (Manager -> ResourceT m a)
                    -> m a
withManagerSettings set f = liftIO (newManager set) >>= runResourceT . f
{-# DEPRECATED withManagerSettings "Please use newManager" #-}

setConnectionClose :: Request -> Request
setConnectionClose req = req{requestHeaders = ("Connection", "close") : requestHeaders req}

lbsResponse :: Monad m
            => Response (ResumableSource m S.ByteString)
            -> m (Response L.ByteString)
lbsResponse res = do
    bss <- responseBody res $$+- CL.consume
    return res
        { responseBody = L.fromChunks bss
        }

http :: MonadResource m
     => Request
     -> Manager
     -> m (Response (ResumableSource m S.ByteString))
http req man = do
    (key, res) <- allocate (Client.responseOpen req man) Client.responseClose
    let rsrc = CI.ResumableSource
            (flip CI.unConduitT CI.Done $ addCleanup (const $ release key) $ HCC.bodyReaderSource $ responseBody res)
            (release key)
    return res { responseBody = rsrc }

requestBodySource :: Int64 -> Source (ResourceT IO) S.ByteString -> RequestBody
requestBodySource size = RequestBodyStream size . srcToPopper

requestBodySourceChunked :: Source (ResourceT IO) S.ByteString -> RequestBody
requestBodySourceChunked = RequestBodyStreamChunked . srcToPopper

srcToPopper :: Source (ResourceT IO) S.ByteString -> HCC.GivesPopper ()
srcToPopper src f = runResourceT $ do
    (rsrc0, ()) <- src $$+ return ()
    irsrc <- liftIO $ newIORef rsrc0
    is <- getInternalState
    let popper :: IO S.ByteString
        popper = do
            rsrc <- readIORef irsrc
            (rsrc', mres) <- runInternalState (rsrc $$++ await) is
            writeIORef irsrc rsrc'
            case mres of
                Nothing -> return S.empty
                Just bs
                    | S.null bs -> popper
                    | otherwise -> return bs
    liftIO $ f popper

requestBodySourceIO :: Int64 -> Source IO S.ByteString -> RequestBody
requestBodySourceIO = HCC.requestBodySource

requestBodySourceChunkedIO :: Source IO S.ByteString -> RequestBody
requestBodySourceChunkedIO = HCC.requestBodySourceChunked
