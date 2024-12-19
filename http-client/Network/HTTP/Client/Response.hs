{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.HTTP.Client.Response
    ( getRedirectedRequest
    , getResponse
    , lbsResponse
    , getOriginalRequest
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.CaseInsensitive as CI
import Control.Arrow (second)

import Data.Monoid (mempty)
import Data.List (nubBy)

import qualified Network.HTTP.Types as W
import Network.URI (parseURIReference, escapeURIString, isAllowedInURI)

import Network.HTTP.Client.Types

import Network.HTTP.Client.Request
import Network.HTTP.Client.Util
import Network.HTTP.Client.Body
import Network.HTTP.Client.Headers
import Data.KeyedPool

-- | If a request is a redirection (status code 3xx) this function will create
-- a new request from the old request, the server headers returned with the
-- redirection, and the redirection code itself. This function returns 'Nothing'
-- if the code is not a 3xx, there is no 'location' header included, or if the
-- redirected response couldn't be parsed with 'parseRequest'.
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
-- >                return (res, getRedirectedRequest req req' (responseHeaders res) (responseCookieJar res) (W.statusCode (responseStatus res))
-- >                )
-- >             'lift'
-- >             req
-- >    applyCheckStatus (checkStatus req) res
-- >    return redirectRequests
getRedirectedRequest :: Request -> Request -> W.ResponseHeaders -> CookieJar -> Int -> Maybe Request
getRedirectedRequest origReq req hs cookie_jar code
    | 300 <= code && code < 400 = do
        l' <- lookup "location" hs
        let l = escapeURIString isAllowedInURI (S8.unpack l')
        req' <- fmap stripHeaders <$> setUriRelative req =<< parseURIReference l
        return $
            if code == 302 || code == 303
                -- According to the spec, this should *only* be for status code
                -- 303. However, almost all clients mistakenly implement it for
                -- 302 as well. So we have to be wrong like everyone else...
                then req'
                    { method = "GET"
                    , requestBody = RequestBodyBS ""
                    , cookieJar = cookie_jar'
                    , requestHeaders = filter ((/= W.hContentType) . fst) $ requestHeaders req'
                    }
                else req' {cookieJar = cookie_jar'}
    | otherwise = Nothing
  where
    cookie_jar' :: Maybe CookieJar
    cookie_jar' = fmap (const cookie_jar) $ cookieJar req

    hostDiffer :: Request -> Bool
    hostDiffer req = host origReq /= host req

    shouldStripOnlyIfHostDiffer :: Bool
    shouldStripOnlyIfHostDiffer = shouldStripHeaderOnRedirectIfOnDifferentHostOnly req

    mergeHeaders :: W.RequestHeaders -> W.RequestHeaders -> W.RequestHeaders
    mergeHeaders lhs rhs = nubBy (\(a, _) (a', _) -> a == a') (lhs ++ rhs)

    stripHeaders :: Request -> Request
    stripHeaders r = do
        case (hostDiffer r, shouldStripOnlyIfHostDiffer) of
            (True, True) -> stripHeaders' r
            (True, False) -> stripHeaders' r
            (False, False) -> stripHeaders' r
            (False, True) -> do
                -- We need to check if we have omitted headers in previous
                -- request chain. Consider request chain:
                --
                --  1. example-1.com
                --  2. example-2.com (we may have removed some headers here from 1)
                --  3. example-1.com (since we are back at same host as 1, we need re-add stripped headers)
                --
                let strippedHeaders = filter (shouldStripHeaderOnRedirect r . fst) (requestHeaders origReq)
                r{requestHeaders = mergeHeaders (requestHeaders r) strippedHeaders}

    stripHeaders' :: Request -> Request
    stripHeaders' r = r{requestHeaders =
                        filter (not . shouldStripHeaderOnRedirect req . fst) $
                        requestHeaders r}

-- | Convert a 'Response' that has a 'Source' body to one with a lazy
-- 'L.ByteString' body.
lbsResponse :: Response BodyReader -> IO (Response L.ByteString)
lbsResponse res = do
    bss <- brConsume $ responseBody res
    return res
        { responseBody = L.fromChunks bss
        }

getResponse :: Maybe MaxHeaderLength
            -> Maybe MaxNumberHeaders
            -> Maybe Int
            -> Request
            -> Managed Connection
            -> Maybe (IO ()) -- ^ Action to run in case of a '100 Continue'.
            -> IO (Response BodyReader)
getResponse mhl mnh timeout' req@(Request {..}) mconn cont = do
    let conn = managedResource mconn
    StatusHeaders s version earlyHs hs <- parseStatusHeaders mhl mnh conn timeout' earlyHintHeadersReceived cont
    let mcl = lookup "content-length" hs >>= readPositiveInt . S8.unpack
        isChunked = ("transfer-encoding", CI.mk "chunked") `elem` map (second CI.mk) hs

        -- should we put this connection back into the connection manager?
        toPut = Just "close" /= lookup "connection" hs && version > W.HttpVersion 1 0
        cleanup bodyConsumed = do
            managedRelease mconn $ if toPut && bodyConsumed then Reuse else DontReuse
            -- Keep alive the `Managed Connection` until we're done with it, to prevent an early
            -- collection.
            -- Reasoning: as long as someone holds a reference to the explicit cleanup,
            -- we shouldn't perform an implicit cleanup.
            keepAlive mconn


    body <-
        -- RFC 2616 section 4.4_1 defines responses that must not include a body
        if hasNoBody method (W.statusCode s) || (mcl == Just 0 && not isChunked)
            then do
                cleanup True
                return brEmpty
            else do
                body1 <-
                    if isChunked
                        then makeChunkedReader mhl (cleanup True) rawBody conn
                        else
                            case mcl of
                                Just len -> makeLengthReader (cleanup True) len conn
                                Nothing -> makeUnlimitedReader (cleanup True) conn
                if needsGunzip req hs
                    then makeGzipReader body1
                    else return body1

    return Response
        { responseStatus = s
        , responseVersion = version
        , responseHeaders = hs
        , responseBody = body
        , responseCookieJar = Data.Monoid.mempty
        , responseClose' = ResponseClose (cleanup False)
        , responseOriginalRequest = req {requestBody = ""}
        , responseEarlyHints = earlyHs
        }

-- | Does this response have no body?
hasNoBody :: ByteString -- ^ request method
          -> Int -- ^ status code
          -> Bool
hasNoBody "HEAD" _ = True
hasNoBody _ 204 = True
hasNoBody _ 304 = True
hasNoBody _ i = 100 <= i && i < 200

-- | Retrieve the orignal 'Request' from a 'Response'
--
-- Note that the 'requestBody' is not available and always set to empty.
--
-- @since 0.7.8
getOriginalRequest :: Response a -> Request
getOriginalRequest = responseOriginalRequest
