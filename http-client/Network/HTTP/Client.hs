{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
-- |
--
-- = Simpler API
--
-- The API below is rather low-level. The @Network.HTTP.Simple@ module (from
-- the @http-conduit@ package) provides a higher-level API with built-in
-- support for things like JSON request and response bodies. For most users,
-- this will be an easier place to start. You can read the tutorial at:
--
-- https://github.com/commercialhaskell/jump/blob/master/doc/http-client.md
--
-- = Lower-level API
--
-- This is the main entry point for using http-client. Used by itself, this
-- module provides low-level access for streaming request and response bodies,
-- and only non-secure HTTP connections. Helper packages such as http-conduit
-- provided higher level streaming approaches, while other helper packages like
-- http-client-tls provide secure connections.
--
-- There are three core components to be understood here: requests, responses,
-- and managers. A 'Manager' keeps track of open connections to various hosts,
-- and when requested, will provide either an existing open connection or
-- create a new connection on demand. A 'Manager' also automatically reaps
-- connections which have been unused for a certain period of time. A 'Manager'
-- allows for more efficient HTTP usage by allowing for keep-alive connections.
-- Secure HTTP connections can be allowed by modifying the settings used for
-- creating a manager. The simplest way to create a 'Manager' is with:
--
-- @
-- 'newManager' 'defaultManagerSettings'
-- @
--
-- While generally speaking it is a good idea to share a single 'Manager'
-- throughout your application, there are cases where it makes more sense to
-- create and destroy 'Manager's more frequently. As an example, if you have an
-- application which will make a large number of requests to different hosts,
-- and will never make more than one connection to a single host, then sharing
-- a 'Manager' will result in idle connections being kept open longer than
-- necessary. In such a situation, it makes sense to use 'withManager' around
-- each new request, to avoid running out of file descriptors. (Note that the
-- 'managerIdleConnectionCount' setting mitigates the risk of leaking too many
-- file descriptors.)
--
-- The next core component is a 'Request', which represents a single HTTP
-- request to be sent to a specific server. 'Request's allow for many settings
-- to control exact how they function, but usually the simplest approach for
-- creating a 'Request' is to use 'parseUrl'.
--
-- Finally, a 'Response' is the result of sending a single 'Request' to a
-- server, over a connection which was acquired from a 'Manager'. Note that you
-- must close the response when you're done with it to ensure that the
-- connection is recycled to the 'Manager' to either be used by another
-- request, or to be reaped. Usage of 'withResponse' will ensure that this
-- happens automatically.
--
-- Helper packages may provide replacements for various recommendations listed
-- above. For example, if using http-client-tls, instead of using
-- 'defaultManagerSettings', you would want to use 'tlsManagerSettings'. Be
-- sure to read the relevant helper library documentation for more information.
--
-- A note on exceptions: for the most part, all actions that perform I/O should
-- be assumed to throw an 'HttpException' in the event of some problem, and all
-- pure functions will be total. For example, 'withResponse', 'httpLbs', and
-- 'BodyReader' can all throw exceptions. Functions like 'responseStatus' and
-- 'applyBasicAuth' are guaranteed to be total (or there\'s a bug in the
-- library).
--
-- One thing to be cautioned about: the type of 'parseUrl' allows it to work in
-- different monads. If used in the 'IO' monad, it will throw an exception in
-- the case of an invalid URI. In addition, if you leverage the @IsString@
-- instance of the 'Request' value via @OverloadedStrings@, an invalid URI will
-- result in a partial value. Caveat emptor!
--
-- Non-2xx responses: the default behavior of all functions in http-client is
-- to automatically perform up to 10 redirects (response codes 301, 302, 303,
-- and 307), and to throw a 'StatusCodeException' on all responses whose status
-- are not in the 2xx range. These behaviors can be overridden by the
-- 'redirectCount' and 'checkStatus' settings on a request, respectively.
module Network.HTTP.Client
    ( -- $example1

      -- * Performing requests
      withResponse
    , httpLbs
    , httpNoBody
    , responseOpen
    , responseClose
      -- ** Tracking redirect history
    , withResponseHistory
    , responseOpenHistory
    , HistoriedResponse
    , hrRedirects
    , hrFinalRequest
    , hrFinalResponse
      -- * Connection manager
    , Manager
    , newManager
    , closeManager
    , withManager
    , HasHttpManager(..)
      -- ** Connection manager settings
    , ManagerSettings
    , defaultManagerSettings
    , managerConnCount
    , managerRawConnection
    , managerTlsConnection
    , managerResponseTimeout
    , managerRetryableException
    , managerWrapIOException
    , managerIdleConnectionCount
    , managerModifyRequest
      -- *** Manager proxy settings
    , managerSetProxy
    , managerSetInsecureProxy
    , managerSetSecureProxy
    , ProxyOverride
    , proxyFromRequest
    , noProxy
    , useProxy
    , proxyEnvironment
    , proxyEnvironmentNamed
    , defaultProxy
      -- *** Helpers
    , rawConnectionModifySocket
      -- * Request
    , parseUrl
    , applyBasicAuth
    , urlEncodedBody
    , getUri
    , setQueryString
      -- ** Request type and fields
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
    , applyBasicProxyAuth
    , decompress
    , redirectCount
    , checkStatus
    , responseTimeout
    , cookieJar
    , requestVersion
      -- ** Request body
    , RequestBody (..)
    , Popper
    , NeedsPopper
    , GivesPopper
    , streamFile
    , observedStreamFile
    , StreamFileStatus (..)
      -- * Response
    , Response
    , responseStatus
    , responseVersion
    , responseHeaders
    , responseBody
    , responseCookieJar
      -- ** Response body
    , BodyReader
    , brRead
    , brReadSome
    , brConsume
      -- * Misc
    , HttpException (..)
    , Cookie (..)
    , CookieJar
    , Proxy (..)
      -- * Cookies
    , module Network.HTTP.Client.Cookies
    ) where

import Network.HTTP.Client.Body
import Network.HTTP.Client.Cookies
import Network.HTTP.Client.Core
import Network.HTTP.Client.Manager
import Network.HTTP.Client.Request
import Network.HTTP.Client.Response
import Network.HTTP.Client.Types

import Data.Text (Text)
import Data.IORef (newIORef, writeIORef, readIORef, modifyIORef)
import qualified Data.ByteString.Lazy as L
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Network.HTTP.Types (statusCode)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Control.Exception (bracket)

-- | A datatype holding information on redirected requests and the final response.
--
-- Since 0.4.1
data HistoriedResponse body = HistoriedResponse
    { hrRedirects :: [(Request, Response L.ByteString)]
    -- ^ Requests which resulted in a redirect, together with their responses.
    -- The response contains the first 1024 bytes of the body.
    --
    -- Since 0.4.1
    , hrFinalRequest :: Request
    -- ^ The final request performed.
    --
    -- Since 0.4.1
    , hrFinalResponse :: Response body
    -- ^ The response from the final request.
    --
    -- Since 0.4.1
    }
    deriving (Functor, Traversable, Foldable, Show, Typeable, Generic)

-- | A variant of @responseOpen@ which keeps a history of all redirects
-- performed in the interim, together with the first 1024 bytes of their
-- response bodies.
--
-- Since 0.4.1
responseOpenHistory :: Request -> Manager -> IO (HistoriedResponse BodyReader)
responseOpenHistory req0 man = do
    reqRef <- newIORef req0
    historyRef <- newIORef id
    let go req = do
            (req', res) <- httpRaw' req man
            case getRedirectedRequest
                    req'
                    (responseHeaders res)
                    (responseCookieJar res)
                    (statusCode $ responseStatus res) of
                Nothing -> return (res, req', False)
                Just req'' -> do
                    writeIORef reqRef req''
                    body <- brReadSome (responseBody res) 1024
                    modifyIORef historyRef (. ((req, res { responseBody = body }):))
                    return (res, req'', True)
    (_, res) <- httpRedirect' (redirectCount req0) go req0
    reqFinal <- readIORef reqRef
    history <- readIORef historyRef
    return HistoriedResponse
        { hrRedirects = history []
        , hrFinalRequest = reqFinal
        , hrFinalResponse = res
        }

-- | A variant of @withResponse@ which keeps a history of all redirects
-- performed in the interim, together with the first 1024 bytes of their
-- response bodies.
--
-- Since 0.4.1
withResponseHistory :: Request
                    -> Manager
                    -> (HistoriedResponse BodyReader -> IO a)
                    -> IO a
withResponseHistory req man = bracket
    (responseOpenHistory req man)
    (responseClose . hrFinalResponse)

-- | Set the proxy override value, only for HTTP (insecure) connections.
--
-- Since 0.4.7
managerSetInsecureProxy :: ProxyOverride -> ManagerSettings -> ManagerSettings
managerSetInsecureProxy po m = m { managerProxyInsecure = po }

-- | Set the proxy override value, only for HTTPS (secure) connections.
--
-- Since 0.4.7
managerSetSecureProxy :: ProxyOverride -> ManagerSettings -> ManagerSettings
managerSetSecureProxy po m = m { managerProxySecure = po }

-- | Set the proxy override value, for both HTTP (insecure) and HTTPS
-- (insecure) connections.
--
-- Since 0.4.7
managerSetProxy :: ProxyOverride -> ManagerSettings -> ManagerSettings
managerSetProxy po = managerSetInsecureProxy po . managerSetSecureProxy po



-- $example1
-- = Example Usage
--
-- === Making a GET request
--
-- > import Network.HTTP.Client
-- > import Network.HTTP.Types.Status (statusCode)
-- >
-- > main :: IO ()
-- > main = do
-- >   manager <- newManager defaultManagerSettings
-- >
-- >   request <- parseUrl "http://httpbin.org/post"
-- >   response <- httpLbs request manager
-- >
-- >   putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
-- >   print $ responseBody response
--
--
-- === Posting JSON to a server
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Network.HTTP.Client
-- > import Network.HTTP.Types.Status (statusCode)
-- > import Data.Aeson (object, (.=), encode)
-- >
-- > main :: IO ()
-- > main = do
-- >   manager <- newManager defaultManagerSettings
-- >
-- >   -- Create the request
-- >   let requestObject = object ["name" .= "Michael", "age" .= 30]
-- >   initialRequest <- parseUrl "http://httpbin.org/post"
-- >   let request = initialRequest { method = "POST", requestBody = RequestBodyLBS $ encode requestObject }
-- >
-- >   response <- httpLbs request manager
-- >   putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
-- >   print $ responseBody response
--
