{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Network.HTTP.Client.Core
    ( withResponse
    , httpLbs
    , httpNoBody
    , httpRaw
    , responseOpen
    , responseClose
    , applyCheckStatus
    , httpRedirect
    ) where

import Network.HTTP.Types
import Network.HTTP.Client.Manager
import Network.HTTP.Client.Types
import Network.HTTP.Client.Body
import Network.HTTP.Client.Request
import Network.HTTP.Client.Response
import Network.HTTP.Client.Cookies
import Data.Time
import Control.Monad
import Control.Exception
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Data.Functor ((<$>))
import qualified Data.Traversable as T (mapM)
import Control.Monad (void)

-- | Perform a @Request@ using a connection acquired from the given @Manager@,
-- and then provide the @Response@ to the given function. This function is
-- fully exception safe, guaranteeing that the response will be closed when the
-- inner function exits. It is defined as:
--
-- > withResponse req man f = bracket (responseOpen req man) responseClose f
--
-- It is recommended that you use this function in place of explicit calls to
-- 'responseOpen' and 'responseClose'.
--
-- You will need to use functions such as 'brRead' to consume the response
-- body.
--
-- Since 0.1.0
withResponse :: Request
             -> Manager
             -> (Response BodyReader -> IO a)
             -> IO a
withResponse req man f = bracket (responseOpen req man) responseClose f

-- | A convenience wrapper around 'withResponse' which reads in the entire
-- response body and immediately closes the connection. Note that this function
-- performs fully strict I\/O, and only uses a lazy ByteString in its response
-- for memory efficiency. If you are anticipating a large response body, you
-- are encouraged to use 'withResponse' and 'brRead' instead.
--
-- Since 0.1.0
httpLbs :: Request -> Manager -> IO (Response L.ByteString)
httpLbs req man = withResponse req man $ \res -> do
    bss <- brConsume $ responseBody res
    return res { responseBody = L.fromChunks bss }

-- | A convenient wrapper around 'withResponse' which ignores the response
-- body. This is useful, for example, when performing a HEAD request.
--
-- Since 0.3.2
httpNoBody :: Request -> Manager -> IO (Response ())
httpNoBody req man = withResponse req man $ return . void

-- | Get a 'Response' without any redirect following.
httpRaw
     :: Request
     -> Manager
     -> IO (Response BodyReader)
httpRaw req' m = do
    (req, cookie_jar') <- case cookieJar req' of
        Just cj -> do
            now <- getCurrentTime
            return $ insertCookiesIntoRequest req' (evictExpiredCookies cj now) now
        Nothing -> return (req', mempty)
    (timeout', (connRelease, ci, isManaged)) <- getConnectionWrapper
        req
        (responseTimeout' req)
        (failedConnectionException req)
        (getConn req m)

    -- Originally, we would only test for exceptions when sending the request,
    -- not on calling @getResponse@. However, some servers seem to close
    -- connections after accepting the request headers, so we need to check for
    -- exceptions in both.
    ex <- try $ do
        requestBuilder req ci

        getResponse connRelease timeout' req ci

    case (ex, isManaged) of
        -- Connection was reused, and might have been closed. Try again
        (Left e, Reused) | mRetryableException m e -> do
            connRelease DontReuse
            responseOpen req m
        -- Not reused, or a non-retry, so this is a real exception
        (Left e, _) -> throwIO e
        -- Everything went ok, so the connection is good. If any exceptions get
        -- thrown in the response body, just throw them as normal.
        (Right res, _) -> case cookieJar req' of
            Just _ -> do
                now' <- getCurrentTime
                let (cookie_jar, _) = updateCookieJar res req now' cookie_jar'
                return $ res {responseCookieJar = cookie_jar}
            Nothing -> return res
  where

    responseTimeout' req
        | rt == useDefaultTimeout = mResponseTimeout m
        | otherwise = rt
      where
        rt = responseTimeout req

-- | The most low-level function for initiating an HTTP request.
--
-- The first argument to this function gives a full specification
-- on the request: the host to connect to, whether to use SSL,
-- headers, etc. Please see 'Request' for full details.  The
-- second argument specifies which 'Manager' should be used.
--
-- This function then returns a 'Response' with a
-- 'BodyReader'.  The 'Response' contains the status code
-- and headers that were sent back to us, and the
-- 'BodyReader' contains the body of the request.  Note
-- that this 'BodyReader' allows you to have fully
-- interleaved IO actions during your HTTP download, making it
-- possible to download very large responses in constant memory.
--
-- An important note: the response body returned by this function represents a
-- live HTTP connection. As such, if you do not use the response body, an open
-- socket will be retained indefinitely. You must be certain to call
-- 'responseClose' on this response to free up resources.
--
-- This function automatically performs any necessary redirects, as specified
-- by the 'redirectCount' setting.
--
-- When implementing a (reverse) proxy using this function or relating
-- functions, it's wise to remove Transfer-Encoding:, Content-Length:,
-- Content-Encoding: and Accept-Encoding: from request and response
-- headers to be relayed.
--
-- Since 0.1.0
responseOpen :: Request -> Manager -> IO (Response BodyReader)
responseOpen req0 manager = snd . snd <$> responseOpen' req0 manager

responseOpen'
    :: Request
    -> Manager
    -> IO ([(Request, Response L.ByteString)], (Request, Response BodyReader))
responseOpen' req0 manager = mWrapIOException manager $ do
    res@(_,(_,lastRes)) <-
        if redirectCount req0 == 0
            then ([],) . (req0,) <$> httpRaw req0 manager
            else go (redirectCount req0) req0
    maybe (return res) throwIO =<< applyCheckStatus req0 (checkStatus req0) lastRes
  where
    go count req' = httpRedirect count (defaultRedirectHandler manager) id req'

defaultRedirectHandler
     :: Manager
     -> Request
     -> IO (Either (Response L.ByteString, Request) (Response BodyReader))
defaultRedirectHandler manager req = do
    res <- httpRaw req manager
    let mNextReq = redirReq req res
    case mNextReq of
      Nothing -> return $ Right res
      Just nextReq -> Left . (,req) <$> defaultMapRes res

 where
   redirReq req (Response status _ headers _ jar _  ) =
       getRedirectedRequest req headers jar (statusCode status)

   -- By default allow the original connection to return to the connection pool
   -- immediately by flushing the body.  If the response body is too large,
   -- don't flush, but instead just close the connection.
   defaultMapRes :: Response BodyReader -> IO (Response L.ByteString)
   defaultMapRes res = flip finally (responseClose res) $ do
     let maxFlush = 1024
     T.mapM (flip brReadSome maxFlush) res

-- | Apply 'Request'\'s 'checkStatus' and return resulting exception if any.
applyCheckStatus
    :: Request
    -> (Status -> ResponseHeaders -> CookieJar -> Maybe SomeException)
    -> Response BodyReader
    -> IO (Maybe SomeException)
applyCheckStatus req checkStatus' res =
    case checkStatus' (responseStatus res) (responseHeaders res) (responseCookieJar res) of
        Nothing -> return Nothing
        Just exc -> do
            exc' <-
                case fromException exc of
                    Just (StatusCodeException s hdrs cookie_jar) -> do
                        lbs <- brReadSome (responseBody res) 1024
                        return $ toException $ StatusCodeException s (hdrs ++
                            [ ("X-Response-Body-Start", toStrict' lbs)
                            , ("X-Request-URL", S.concat
                                [ method req
                                , " "
                                , S8.pack $ show $ getUri req
                                ])
                            ]) cookie_jar
                    _ -> return exc
            responseClose res
            return (Just exc')
  where
#if MIN_VERSION_bytestring(0,10,0)
    toStrict' = L.toStrict
#else
    toStrict' = S.concat . L.toChunks
#endif

-- | Redirect logic.
httpRedirect
     :: Int -- ^ 'redirectCount'
     -> (Request -> IO (Either (Response b, Request) (Response c)))
     -- ^ Function which performs a request and either returns returns a
     -- response and another request if there is a redirect or returns the final
     -- response.
     -> (b -> L.ByteString)
     -- ^ Function to turn the body of intermediary responses into a bytestring
     -- for inclusion in a 'TooManyRedirects' exception.
     -> Request
     -> IO ([(Request, Response b)], (Request, Response c))
httpRedirect count0 http' bodyToBS req0 =
    go count0 req0 []
  where
    go count _ rs | count < 0 =
        throwIO $ TooManyRedirects (fmap bodyToBS . snd <$> rs)

    go count req rs = do
        e <- http' req
        case e of
            Left (res, nextReq) ->
                go (count - 1) nextReq $ (req, res):rs
            Right res ->
                return (rs, (req, res))

-- | Close any open resources associated with the given @Response@. In general,
-- this will either close an active @Connection@ or return it to the @Manager@
-- to be reused.
--
-- Since 0.1.0
responseClose :: Response a -> IO ()
responseClose = runResponseClose . responseClose'
