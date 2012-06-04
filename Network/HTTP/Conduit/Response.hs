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
import Control.Monad (liftM)

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L

import qualified Data.CaseInsensitive as CI

import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Trans.Class (lift)
import qualified Data.Conduit as C
import qualified Data.Conduit.Zlib as CZ
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Internal

import qualified Network.HTTP.Types as W
import Network.URI (parseURIReference)

import Network.HTTP.Conduit.Manager
import Network.HTTP.Conduit.Request
import Network.HTTP.Conduit.Util
import Network.HTTP.Conduit.Parser
import Network.HTTP.Conduit.Chunk

import Data.Void (absurd)

-- | A simple representation of the HTTP response created by 'lbsConsumer'.
data Response body = Response
    { responseStatus :: W.Status
    , responseVersion :: W.HttpVersion
    , responseHeaders :: W.ResponseHeaders
    , responseBody :: body
    }
    deriving (Show, Eq, Typeable)

-- | Since 1.1.2.
instance Functor Response where
    fmap f (Response status v headers body) = Response status v headers (f body)

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
        req' <- setUriRelative req =<< parseURIReference (S8.unpack l')
        return req'
          { method =
              -- According to the spec, this should *only* be for
              -- status code 303. However, almost all clients
              -- mistakenly implement it for 302 as well. So we
              -- have to be wrong like everyone else...
              if code == 302 || code == 303
                  then "GET"
                  else method req'
          }
    | otherwise = Nothing

-- | Convert a 'Response' that has a 'C.Source' body to one with a lazy
-- 'L.ByteString' body.
lbsResponse :: Monad m
            => m (Response (C.Source m S8.ByteString))
            -> m (Response L.ByteString)
lbsResponse mres = do
    res <- mres
    bss <- responseBody res C.$$ CL.consume
    return res
        { responseBody = L.fromChunks bss
        }

checkHeaderLength :: MonadResource m
                  => Int
                  -> C.Sink S8.ByteString m a
                  -> C.Sink S8.ByteString m a
checkHeaderLength len C.NeedInput{}
    | len <= 0 =
        let x = liftIO $ throwIO OverlongHeaders
         in C.PipeM x (lift x)
checkHeaderLength len (C.NeedInput pushI closeI) = C.NeedInput
    (\bs -> checkHeaderLength
        (len - S8.length bs)
        (pushI bs)) closeI
checkHeaderLength len (C.PipeM msink close) = C.PipeM (liftM (checkHeaderLength len) msink) close
checkHeaderLength _ s@C.Done{} = s
checkHeaderLength _ (C.HaveOutput _ _ o) = absurd o

getResponse :: MonadResource m
            => ConnRelease m
            -> Request m
            -> C.Source m S8.ByteString
            -> m (Response (C.Source m S8.ByteString))
getResponse connRelease req@(Request {..}) src1 = do
    (src2, ((vbs, sc, sm), hs)) <- src1 C.$$+ checkHeaderLength 4096 sinkHeaders
    let version = if vbs == "1.1" then W.http11 else W.http10
    let s = W.Status sc sm
    let hs' = map (first CI.mk) hs
    let mcl = lookup "content-length" hs' >>= readDec . S8.unpack

    -- should we put this connection back into the connection manager?
    let toPut = Just "close" /= lookup "connection" hs'
    let cleanup bodyConsumed = connRelease $ if toPut && bodyConsumed then Reuse else DontReuse

    -- RFC 2616 section 4.4_1 defines responses that must not include a body
    body <-
        if hasNoBody method sc || mcl == Just 0
            then do
                cleanup True
                return mempty
            else do
                let src3 =
                        if ("transfer-encoding", "chunked") `elem` hs'
                            then src2 C.$= chunkedConduit rawBody
                            else
                                case mcl of
                                    Just len -> src2 C.$= CB.isolate len
                                    Nothing  -> src2
                let src4 =
                        if needsGunzip req hs'
                            then src3 C.$= CZ.ungzip
                            else src3
                return $ Data.Conduit.Internal.addCleanup cleanup src4

    return $ Response s version hs' body
