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

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Control.Monad (liftM, unless, when)

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L

import qualified Data.CaseInsensitive as CI

import Data.Conduit hiding (Conduit)
import Data.Conduit.Internal (ResumableSource (..), Pipe (..))
import qualified Data.Conduit.Zlib as CZ
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

import qualified Network.HTTP.Types as W
import Network.URI (parseURIReference)

import Network.HTTP.Conduit.Types (Response (..))

import Network.HTTP.Conduit.Manager
import Network.HTTP.Conduit.Request
import Network.HTTP.Conduit.Util
import Network.HTTP.Conduit.Chunk

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
-- > myHttp req man = E.catch (runResourceT $ http req' man >> return [req'])
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
        return $
            if code == 302 || code == 303
                -- According to the spec, this should *only* be for status code
                -- 303. However, almost all clients mistakenly implement it for
                -- 302 as well. So we have to be wrong like everyone else...
                then req'
                    { method = "GET"
                    , requestBody = RequestBodyBS ""
                    }
                else req'
    | otherwise = Nothing

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
        ConduitM (checkHeaderLength 4096 $ unConduitM sinkHeaders')
#else
        (checkHeaderLength 4096 sinkHeaders')
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
                let src3 =
                        if ("transfer-encoding", "chunked") `elem` hs'
                            then fmapResume ($= chunkedConduit rawBody) src2
                            else
                                case mcl of
                                    Just len -> fmapResume ($= CB.isolate len) src2
                                    Nothing  -> src2
                let src4 =
                        if needsGunzip req hs'
                            then fmapResume ($= CZ.ungzip) src3
                            else src3
                return $ addCleanup' cleanup src4

    return $ Response s version hs' body
  where
    fmapResume f (ResumableSource src m) = ResumableSource (f src) m
    addCleanup' f (ResumableSource src m) = ResumableSource (addCleanup f src) (m >> f False)

-- | New version of @sinkHeaders@ that doesn't use attoparsec. Should create
-- more meaningful exceptions.
--
-- Since 1.8.7
sinkHeaders' :: (MonadThrow m, MonadResource m) => Sink S.ByteString m (Status, [Header])
sinkHeaders' = do
    status <- getStatusLine
    headers <- parseHeaders id
    return (status, headers)
  where
    getStatusLine = do
        status@(_, code, _) <- sinkLine >>= parseStatus
        if code == 100
            then newline ExpectedBlankAfter100Continue >> getStatusLine
            else return status

    newline exc = do
        line <- sinkLine
        unless (S.null line) $ monadThrow exc

    sinkLine = do
        bs <- fmap (killCR . S.concat) $ CB.takeWhile (/= charLF) =$ CL.consume
        CB.drop 1
        return bs
    charLF = 10
    charCR = 13
    charSpace = 32
    charColon = 58
    killCR bs
        | S.null bs = bs
        | S.last bs == charCR = S.init bs
        | otherwise = bs

    parseStatus :: MonadThrow m => S.ByteString -> m Status
    parseStatus bs = do
        let (ver, bs2) = S.breakByte charSpace bs
            (code, bs3) = S.breakByte charSpace $ S.dropWhile (== charSpace) bs2
            msg = S.dropWhile (== charSpace) bs3
        case (,) <$> parseVersion ver <*> parseCode code of
            Just (ver', code') -> return (ver', code', msg)
            _ -> monadThrow $ InvalidStatusLine bs

    stripPrefixBS x y
        | x `S.isPrefixOf` y = Just $ S.drop (S.length x) y
        | otherwise = Nothing
    parseVersion = stripPrefixBS "HTTP/"
    parseCode bs =
        case S8.readInt bs of
            Just (i, "") -> Just i
            _ -> Nothing

    parseHeaders front = do
        line <- sinkLine
        if S.null line
            then return $ front []
            else do
                header <- parseHeader line
                parseHeaders $ front . (header:)

    parseHeader :: MonadThrow m => S.ByteString -> m Header
    parseHeader bs = do
        let (key, bs2) = S.breakByte charColon bs
        when (S.null bs2) $ monadThrow $ InvalidHeader bs
        return (strip key, strip $ S.drop 1 bs2)

    strip = S.dropWhile (== charSpace) . fst . S.spanEnd (== charSpace)

type Header = (S.ByteString, S.ByteString)
type Status = (S.ByteString, Int, S.ByteString)
