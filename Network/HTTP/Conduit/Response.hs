{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Conduit.Response
    ( Response (..)
    , getRedirectedRequest
    , getResponse
    , lbsResponse
    ) where

import Control.Arrow (first)
import Data.Typeable (Typeable)
import Data.Monoid (mempty)

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L

import qualified Data.CaseInsensitive as CI

import Control.Monad.Trans.Resource (ResourceT, ResourceIO)
import qualified Data.Conduit as C
import qualified Data.Conduit.Zlib as CZ
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

import qualified Network.HTTP.Types as W

import Network.HTTP.Conduit.Manager
import Network.HTTP.Conduit.Request
import Network.HTTP.Conduit.Util
import Network.HTTP.Conduit.Parser
import Network.HTTP.Conduit.Chunk

-- | A simple representation of the HTTP response created by 'lbsConsumer'.
data Response body = Response
    { statusCode :: W.Status
    , responseHeaders :: W.ResponseHeaders
    , responseBody :: body
    }
    deriving (Show, Eq, Typeable)

-- | Since 1.1.2.
instance Functor Response where
    fmap f (Response status headers body) = Response status headers (f body)

-- | If a request is a redirection (status code 3xx) this function will create
-- a new request from the old request, the server headers returned with the
-- redirection, and the redirection code itself. This function returns 'Nothing'
-- if the code is not a 3xx, there is no 'location' header included, or if the
-- redirected response couldn't be parsed with 'parseUrl'.
--
-- If a user of this library wants to know the url chain that results from a
-- specific request, that user has to re-implement the redirect-following logic
-- themselves. An example of that might look like this:
--
-- > myHttp req man = E.catch (C.runResourceT $ http req' man >> return [req'])
-- >                    (\ (StatusCodeException status headers) -> do
-- >                        l <- myHttp (fromJust $ nextRequest status headers) man
-- >                        return $ req' : l)
-- >     where req' = req { redirectCount = 0 }
-- >           nextRequest status headers = getRedirectedRequest req' headers $ W.statusCode status
getRedirectedRequest :: Request m -> W.ResponseHeaders -> Int -> Maybe (Request m)
getRedirectedRequest req hs code
    | 300 <= code && code < 400 = do
        l' <- lookup "location" hs
        l <- parseUrl $ case S8.uncons l' of
                Just ('/', _) -> concat
                    [ "http"
                    , if secure req then "s" else ""
                    , "://"
                    , S8.unpack $ host req
                    , ":"
                    , show $ port req
                    , S8.unpack l'
                    ]
                _ -> S8.unpack l'
        return req
          { host = host l
          , port = port l
          , secure = secure l
          , path = path l
          , queryString = queryString l
          , method =
              -- According to the spec, this should *only* be for
              -- status code 303. However, almost all clients
              -- mistakenly implement it for 302 as well. So we
              -- have to be wrong like everyone else...
              if code == 302 || code == 303
                  then "GET"
                  else method l
          }
    | otherwise = Nothing

-- | Convert a 'Response' that has a 'C.Source' body to one with a lazy
-- 'L.ByteString' body.
lbsResponse :: C.Resource m
            => ResourceT m (Response (C.Source m S8.ByteString))
            -> ResourceT m (Response L.ByteString)
lbsResponse mres = do
    res <- mres
    bss <- responseBody res C.$$ CL.consume
    return res
        { responseBody = L.fromChunks bss
        }

getResponse :: ResourceIO m
            => ConnRelease m
            -> Request m
            -> C.BufferedSource m S8.ByteString
            -> ResourceT m (Response (C.Source m S8.ByteString))
getResponse connRelease req@(Request {..}) bsrc = do
    ((_, sc, sm), hs) <- bsrc C.$$ sinkHeaders
    let s = W.Status sc sm
    let hs' = map (first CI.mk) hs
    let mcl = lookup "content-length" hs' >>= readDec . S8.unpack

    -- should we put this connection back into the connection manager?
    let toPut = Just "close" /= lookup "connection" hs'
    let cleanup = connRelease $ if toPut then Reuse else DontReuse

    -- RFC 2616 section 4.4_1 defines responses that must not include a body
    body <-
        if hasNoBody method sc || mcl == Just 0
            then do
                cleanup
                return mempty
            else do
                let bsrc' =
                        if ("transfer-encoding", "chunked") `elem` hs'
                            then bsrc C.$= chunkedConduit rawBody
                            else
                                case mcl of
                                    Just len -> bsrc C.$= CB.isolate len
                                    Nothing  -> C.unbufferSource bsrc
                let bsrc'' =
                        if needsGunzip req hs'
                            then bsrc' C.$= CZ.ungzip
                            else bsrc'
                return $ addCleanup cleanup bsrc''

    return $ Response s hs' body

-- | Add some cleanup code to the given 'C.Source'. General purpose
-- function, could be included in conduit itself.
addCleanup :: C.ResourceIO m
           => ResourceT m ()
           -> C.Source m a
           -> C.Source m a
addCleanup cleanup src = src
    { C.sourcePull = do
        res <- C.sourcePull src
        case res of
            C.Closed -> cleanup >> return C.Closed
            C.Open src' val -> return $ C.Open
                (addCleanup cleanup src')
                val
    , C.sourceClose = do
        C.sourceClose src
        cleanup
    }
