{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.HTTP.Client.Response
    ( getRedirectedRequest
    , getResponse
    , lbsResponse
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.CaseInsensitive as CI
import Control.Arrow (second)

import Data.Monoid (mempty)

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
-- >                return (res, getRedirectedRequest req' (responseHeaders res) (responseCookieJar res) (W.statusCode (responseStatus res))
-- >                )
-- >             'lift'
-- >             req
-- >    applyCheckStatus (checkStatus req) res
-- >    return redirectRequests
getRedirectedRequest :: Request -> W.ResponseHeaders -> CookieJar -> Int -> Maybe Request
getRedirectedRequest req hs cookie_jar code
    | 300 <= code && code < 400 = do
        l' <- lookup "location" hs
        let l = escapeURIString isAllowedInURI (S8.unpack l')
            stripHeaders r =
              r{requestHeaders =
                filter (not . shouldStripHeaderOnRedirect req . fst) $
                requestHeaders r}
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
    cookie_jar' = fmap (const cookie_jar) $ cookieJar req

-- | Convert a 'Response' that has a 'Source' body to one with a lazy
-- 'L.ByteString' body.
lbsResponse :: Response BodyReader -> IO (Response L.ByteString)
lbsResponse res = do
    bss <- brConsume $ responseBody res
    return res
        { responseBody = L.fromChunks bss
        }

getResponse :: Maybe Int
            -> Request
            -> Managed Connection
            -> Maybe (IO ()) -- ^ Action to run in case of a '100 Continue'.
            -> IO (Response BodyReader)
getResponse timeout' req@(Request {..}) mconn cont = do
    let conn = managedResource mconn
    StatusHeaders s version hs <- parseStatusHeaders conn timeout' cont
    let mcl = lookup "content-length" hs >>= readPositiveInt . S8.unpack
        isChunked = ("transfer-encoding", CI.mk "chunked") `elem` map (second CI.mk) hs

        -- should we put this connection back into the connection manager?
        toPut = Just "close" /= lookup "connection" hs && version > W.HttpVersion 1 0
        cleanup bodyConsumed = managedRelease mconn $ if toPut && bodyConsumed then Reuse else DontReuse

    body <-
        -- RFC 2616 section 4.4_1 defines responses that must not include a body
        if hasNoBody method (W.statusCode s) || (mcl == Just 0 && not isChunked)
            then do
                cleanup True
                return brEmpty
            else do
                body1 <-
                    if isChunked
                        then makeChunkedReader (cleanup True) rawBody conn
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
        }

-- | Does this response have no body?
hasNoBody :: ByteString -- ^ request method
          -> Int -- ^ status code
          -> Bool
hasNoBody "HEAD" _ = True
hasNoBody _ 204 = True
hasNoBody _ 304 = True
hasNoBody _ i = 100 <= i && i < 200
