{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Network.HTTP.Conduit.Response
    ( Response (..)
    , getRedirectedRequest
    , getResponse
    , lbsResponse
    ) where

import Control.Arrow (first)
import Control.Monad (liftM)

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L

import qualified Data.CaseInsensitive as CI

import Data.Default (def)

import Data.Conduit
import Data.Conduit.Internal (ResumableSource (..), Pipe (..))
import qualified Data.Conduit.Zlib as CZ
import qualified Data.Conduit.List as CL

import qualified Network.HTTP.Types as W
import Network.URI (parseURIReference)

import Network.HTTP.Conduit.Types (Response (..), CookieJar)

import Network.HTTP.Conduit.Manager
import Network.HTTP.Conduit.Request
import Network.HTTP.Conduit.Util
import Network.HTTP.Conduit.Chunk
import Network.HTTP.Conduit.Parser (sinkHeaders)

import Data.Void (Void, absurd)

import System.Timeout.Lifted (timeout)
#if MIN_VERSION_conduit(1, 0, 0)
import Data.Conduit.Internal (ConduitM (..))
#endif

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
-- > myHttp req man = do
-- >    (res, redirectRequests) <- (`runStateT` []) $
-- >         'httpRedirect'
-- >             9000
-- >             (\req' -> do
-- >                res <- http req'{redirectCount=0} man
-- >                modify (\rqs -> req' : rqs)
-- >                return (res, getRedirectedRequest req' (responseHeaders res) (responseCookieJar res) (W.statusCode (responseStatus res))
-- >                )
-- >             'lift'
-- >             req
-- >    applyCheckStatus (checkStatus req) res
-- >    return redirectRequests
getRedirectedRequest :: Request m -> W.ResponseHeaders -> CookieJar -> Int -> Maybe (Request m)
getRedirectedRequest req hs cookie_jar code
    | 300 <= code && code < 400 = do
        l' <- lookup "location" hs
        req' <- setUriRelative req =<< parseURIReference (S8.unpack l')
        return $
            if code == 302 || code == 303
                -- According to the spec, this should *only* be for status code
                -- 303. However, almost all clients mistakenly implement it for
                -- 302 as well. So we have to be wrong like everyone else...
                then req'
                    { method = "GET"
                    , requestBody = RequestBodyBS ""
                    , cookieJar = cookie_jar'
                    }
                else req' {cookieJar = cookie_jar'}
    | otherwise = Nothing
  where
    cookie_jar' = fmap (const cookie_jar) $ cookieJar req

-- | Convert a 'Response' that has a 'Source' body to one with a lazy
-- 'L.ByteString' body.
lbsResponse :: Monad m
            => Response (ResumableSource m S8.ByteString)
            -> m (Response L.ByteString)
lbsResponse res = do
    bss <- responseBody res $$+- CL.consume
    return res
        { responseBody = L.fromChunks bss
        }

-- | This function can\'t be a Conduit, since it would lose leftovers.
checkHeaderLength :: MonadResource m
                  => Int
                  -> Pipe S8.ByteString S8.ByteString Void u m r
                  -> Pipe S8.ByteString S8.ByteString Void u m r
checkHeaderLength len NeedInput{}
    | len <= 0 = liftIO $ throwIO OverlongHeaders
checkHeaderLength len (NeedInput pushI closeI) = NeedInput
    (\bs -> checkHeaderLength
        (len - S8.length bs)
        (pushI bs)) closeI
checkHeaderLength len (PipeM msink) = PipeM (liftM (checkHeaderLength len) msink)
checkHeaderLength _ s@Done{} = s
checkHeaderLength _ (HaveOutput _ _ o) = absurd o
checkHeaderLength len (Leftover p i) = Leftover (checkHeaderLength (len + S.length i) p) i

getResponse :: (MonadResource m, MonadBaseControl IO m)
            => ConnRelease m
            -> Maybe Int
            -> Request m
            -> Source m S8.ByteString
            -> m (Response (ResumableSource m S8.ByteString))
getResponse connRelease timeout'' req@(Request {..}) src1 = do
    let timeout' =
            case timeout'' of
                Nothing -> id
                Just useconds -> \ma -> do
                    x <- timeout useconds ma
                    case x of
                        Nothing -> liftIO $ throwIO ResponseTimeout
                        Just y -> return y
    (src2, ((vbs, sc, sm), hs)) <- timeout' $ src1 $$+
#if MIN_VERSION_conduit(1, 0, 0)
        ConduitM (checkHeaderLength 4096 $ unConduitM sinkHeaders)
#else
        (checkHeaderLength 4096 sinkHeaders)
#endif
    let version = if vbs == "1.1" then W.http11 else W.http10
    let s = W.Status sc sm
    let hs' = map (first CI.mk) hs
    let mcl = lookup "content-length" hs' >>= readDec . S8.unpack

    -- should we put this connection back into the connection manager?
    let toPut = Just "close" /= lookup "connection" hs' && vbs /= "1.0"
    let cleanup bodyConsumed = connRelease $ if toPut && bodyConsumed then Reuse else DontReuse

    -- RFC 2616 section 4.4_1 defines responses that must not include a body
    body <-
        if hasNoBody method sc || mcl == Just 0
            then do
                cleanup True
                (rsrc, ()) <- return () $$+ return ()
                return rsrc
            else do
                let isChunked = ("transfer-encoding", "chunked") `elem` hs'
                    src3 =
                        if isChunked
                            then fmapResume ($= chunkedConduit rawBody) src2
                            else
                                case mcl of
                                    Just len -> fmapResume ($= requireLength len) src2
                                    Nothing  -> src2
                    src4 =
                        if needsGunzip req hs'
                            then fmapResume ($= (if isChunked then ungzipChunked else CZ.ungzip)) src3
                            else src3
                return $ addCleanup' cleanup src4

    return $ Response s version hs' body def
  where
    -- When a body is both chunked and gzipped, we need to flush each chunk
    -- immediately to ensure streaming behavior.
    ungzipChunked =
        CL.concatMap (\x -> [Chunk x, Flush])
        =$= CZ.decompressFlush (CZ.WindowBits 31)
        =$= awaitForever unChunk
      where
        unChunk Flush = return ()
        unChunk (Chunk x) = yield x
    fmapResume f (ResumableSource src m) = ResumableSource (f src) m
    addCleanup' f (ResumableSource src m) = ResumableSource (addCleanup f src) (m >> f False)

-- | Ensure that the stream has exactly the given length.
requireLength :: MonadIO m => Int -> Conduit S.ByteString m S.ByteString
requireLength total =
    loop total
  where
    loop 0 = return ()
    loop i =
        await >>= maybe
            (liftIO $ throwIO $ ResponseBodyTooShort
                (fromIntegral total)
                (fromIntegral $ total - i))
            go
      where
        go bs =
            case compare i l of
                EQ -> yield bs
                LT -> do
                    let (x, y) = S.splitAt i bs
                    leftover y
                    yield x
                GT -> yield bs >> loop (i - l)
          where
            l = S.length bs
