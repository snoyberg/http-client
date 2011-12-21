{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | This module contains everything you need to initiate HTTP connections.  If
-- you want a simple interface based on URLs, you can use 'simpleHttp'. If you
-- want raw power, 'http' is the underlying workhorse of this package. Some
-- examples:
--
-- > -- Just download an HTML document and print it.
-- > import Network.HTTP.Enumerator
-- > import qualified Data.ByteString.Lazy as L
-- >
-- > main = simpleHttp "http://www.haskell.org/" >>= L.putStr
--
-- This example uses interleaved IO to write the response body to a file in
-- constant memory space. By using 'httpRedirect', it will automatically
-- follow 3xx redirects.
--
-- > import Data.Enumerator
-- > import Data.Enumerator.Binary
-- > import Network.HTTP.Enumerator
-- > import System.IO
-- >
-- > main :: IO ()
-- > main = withFile "google.html" WriteMode $ \handle -> do
-- >     request <- parseUrl "http://google.com/"
-- >     withManager $ \manager -> do
-- >         run_ $ httpRedirect request (\_ _ -> iterHandle handle) manager
--
-- The following headers are automatically set by this module, and should not
-- be added to 'requestHeaders':
--
-- * Content-Length
--
-- * Host
--
-- * Accept-Encoding (not currently set, but client usage of this variable /will/ cause breakage).
--
-- Any network code on Windows requires some initialization, and the network
-- library provides withSocketsDo to perform it. Therefore, proper usage of
-- this library will always involve calling that function at some point.  The
-- best approach is to simply call them at the beginning of your main function,
-- such as:
--
-- > import Network.HTTP.Enumerator
-- > import qualified Data.ByteString.Lazy as L
-- > import Network (withSocketsDo)
-- >
-- > main = withSocketsDo
-- >      $ simpleHttp "http://www.haskell.org/" >>= L.putStr
module Network.HTTP.Conduit
    ( -- * Perform a request
      simpleHttp
    , httpLbs
    , httpLbsRedirect
    , http
    , httpRedirect
    , redirectConsumer
      -- * Datatypes
    , Proxy (..)
    , RequestBody (..)
    , Response (..)
    , ResponseConsumer
      -- ** Request
    , Request
    , def
    , method
    , secure
    , checkCerts
    , host
    , port
    , path
    , queryString
    , requestHeaders
    , requestBody
    , proxy
    , rawBody
    , decompress
      -- *** Defaults
    , defaultCheckCerts
      -- * Manager
    , Manager
    , newManager
      -- * Utility functions
    , parseUrl
    , applyBasicAuth
    , addProxy
    , semiParseUrl
    , lbsConsumer
      -- * Decompression predicates
    , alwaysDecompress
    , browserDecompress
      -- * Request bodies
    , urlEncodedBody
      -- * Exceptions
    , HttpException (..)
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8

import qualified Network.HTTP.Types as W
import Data.Default (def)

import Control.Exception.Lifted (throwIO)
import Control.Monad.Base (liftBase)

import qualified Data.Conduit as C
import Data.Conduit.Blaze (builderToByteString)
import Control.Monad.Trans.Resource (ResourceT, runResourceT, ResourceIO)

import Network.HTTP.Conduit.Request
import Network.HTTP.Conduit.Response
import Network.HTTP.Conduit.Manager
import Network.HTTP.Conduit.ConnInfo

-- | The most low-level function for initiating an HTTP request.
--
-- The first argument to this function gives a full specification on the
-- request: the host to connect to, whether to use SSL, headers, etc. Please
-- see 'Request' for full details.
--
-- The second argument specifies how the response should be handled. It's a
-- function that takes two arguments: the first is the HTTP status code of the
-- response, and the second is a list of all response headers. This module
-- exports 'lbsConsumer', which generates a 'Response' value.
--
-- Note that this allows you to have fully interleaved IO actions during your
-- HTTP download, making it possible to download very large responses in
-- constant memory.
http
     :: ResourceIO m
     => Request m
     -> ResponseConsumer m a
     -> Manager
     -> ResourceT m a
http req consumer m = withConn req m $ \ci -> do
    bsrc <- C.bufferSource $ connSource ci
    requestBuilder req C.$$ builderToByteString C.=$ connSink ci
    getResponse req consumer bsrc

-- | Download the specified 'Request', returning the results as a 'Response'.
--
-- This is a simplified version of 'http' for the common case where you simply
-- want the response data as a simple datatype. If you want more power, such as
-- interleaved actions on the response body during download, you'll need to use
-- 'http' directly. This function is defined as:
--
-- @httpLbs = http lbsConsumer@
--
-- Please see 'lbsConsumer' for more information on how the 'Response' value is
-- created.
--
-- Even though a 'Response' contains a lazy bytestring, this function does
-- /not/ utilize lazy I/O, and therefore the entire response body will live in
-- memory. If you want constant memory usage, you'll need to write your own
-- iteratee and use 'http' or 'httpRedirect' directly.
httpLbs :: ResourceIO m => Request m -> Manager -> ResourceT m Response
httpLbs req = http req lbsConsumer

-- | Download the specified URL, following any redirects, and return the
-- response body.
--
-- This function will 'throwIO' an 'HttpException' for any response with a
-- non-2xx status code. It uses 'parseUrl' to parse the input. This function
-- essentially wraps 'httpLbsRedirect'.
--
-- Note: Even though this function returns a lazy bytestring, it does /not/
-- utilize lazy I/O, and therefore the entire response body will live in
-- memory. If you want constant memory usage, you'll need to write your own
-- iteratee and use 'http' or 'httpRedirect' directly.
simpleHttp :: ResourceIO m => String -> m L.ByteString
simpleHttp url = runResourceT $ do
    url' <- liftBase $ parseUrl url
    man <- newManager
    Response sc _ b <- httpLbsRedirect url'
        { decompress = browserDecompress
        } man
    if 200 <= sc && sc < 300
        then return b
        else liftBase $ throwIO $ StatusCodeException sc b

-- | Same as 'http', but follows all 3xx redirect status codes that contain a
-- location header.
httpRedirect
    :: ResourceIO m
    => Request m
    -> (W.Status -> W.ResponseHeaders -> C.BSource m S.ByteString -> ResourceT m a)
    -> Manager
    -> ResourceT m a
httpRedirect req bodyStep manager =
    http req (redirectConsumer 10 req bodyStep manager) manager

-- | Download the specified 'Request', returning the results as a 'Response'
-- and automatically handling redirects.
--
-- This is a simplified version of 'httpRedirect' for the common case where you
-- simply want the response data as a simple datatype. If you want more power,
-- such as interleaved actions on the response body during download, you'll
-- need to use 'httpRedirect' directly. This function is defined as:
--
-- @httpLbsRedirect = httpRedirect lbsConsumer@
--
-- Please see 'lbsConsumer' for more information on how the 'Response' value is
-- created.
--
-- Even though a 'Response' contains a lazy bytestring, this function does
-- /not/ utilize lazy I/O, and therefore the entire response body will live in
-- memory. If you want constant memory usage, you'll need to write your own
-- iteratee and use 'http' or 'httpRedirect' directly.
httpLbsRedirect :: ResourceIO m => Request m -> Manager -> ResourceT m Response
httpLbsRedirect req m = httpRedirect req lbsConsumer m

-- | Make a request automatically follow 3xx redirects.
--
-- Used internally by 'httpRedirect' and family.
redirectConsumer :: ResourceIO m
             => Int -- ^ number of redirects to attempt
             -> Request m -- ^ Original request
             -> ResponseConsumer m a
             -> Manager
             -> ResponseConsumer m a
redirectConsumer redirects req bodyStep manager s@(W.Status code _) hs bsrc
    | 300 <= code && code < 400 =
        case lookup "location" hs of
            Just l'' -> do
                -- Prepend scheme, host and port if missing
                let l' =
                        case S8.uncons l'' of
                            Just ('/', _) -> concat
                                [ "http"
                                , if secure req then "s" else ""
                                , "://"
                                , S8.unpack $ host req
                                , ":"
                                , show $ port req
                                , S8.unpack l''
                                ]
                            _ -> S8.unpack l''
                l <- liftBase $ parseUrl l'
                let req' = req
                        { host = host l
                        , port = port l
                        , secure = secure l
                        , path = path l
                        , queryString = queryString l
                        , method =
                            if code == 303
                                then "GET"
                                else method l
                        }
                if redirects == 0
                    then liftBase $ throwIO TooManyRedirects
                    else (http req') (redirectConsumer (redirects - 1) req' bodyStep manager) manager
            Nothing -> bodyStep s hs bsrc
    | otherwise = bodyStep s hs bsrc
