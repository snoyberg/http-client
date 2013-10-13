{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client where

import Network.HTTP.Types
import Network.HTTP.Client.Manager
import Network.HTTP.Client.Types
import Network.HTTP.Client.Body
import Network.HTTP.Client.Request
import Network.HTTP.Client.Response
import Network.HTTP.Client.Cookies
import Data.Time
import Control.Exception
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Monoid

withResponse :: Request
             -> Manager
             -> (Response BodyReader -> IO a)
             -> IO a
withResponse req man f = bracket (responseOpen req man) responseClose f

httpLbs :: Request -> Manager -> IO (Response L.ByteString)
httpLbs req man = withResponse req man $ \res -> do
    bss <- brConsume $ responseBody res
    return res { responseBody = L.fromChunks bss }

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
-- 'C.Source'.  The 'Response' contains the status code
-- and headers that were sent back to us, and the
-- 'C.Source' contains the body of the request.  Note
-- that this 'C.Source' allows you to have fully
-- interleaved IO actions during your HTTP download, making it
-- possible to download very large responses in constant memory.
-- You may also directly connect the returned 'C.Source'
-- into a 'C.Sink', perhaps a file or another socket.
--
-- An important note: the response body returned by this function represents a
-- live HTTP connection. As such, if you do not use the response body, an open
-- socket will be retained until the containing @ResourceT@ block exits. If you
-- do not need the response body, it is recommended that you explicitly shut
-- down the connection immediately, using the pattern:
--
-- > responseBody res $$+- return ()
--
-- As a more thorough example, consider the following program. Without the
-- explicit response body closing, the program will run out of file descriptors
-- around the 1000th request (depending on the operating system limits).
--
-- > import Control.Monad          (replicateM_)
-- > import Control.Monad.IO.Class (liftIO)
-- > import Data.Conduit           (($$+-))
-- > import Network                (withSocketsDo)
-- > import Network.HTTP.Conduit
-- >
-- > main = withSocketsDo $ withManager $ \manager -> do
-- >     req <- parseUrl "http://localhost/"
-- >     mapM_ (worker manager req) [1..5000]
-- >
-- > worker manager req i = do
-- >     res <- http req manager
-- >     responseBody res $$+- return () -- The important line
-- >     liftIO $ print (i, responseStatus res)
--
-- Note: Unlike previous versions, this function will perform redirects, as
-- specified by the 'redirectCount' setting.
responseOpen :: Request -> Manager -> IO (Response BodyReader)
responseOpen req0 manager = mWrapIOException manager $ do
    res <-
        if redirectCount req0 == 0
            then httpRaw req0 manager
            else go (redirectCount req0) req0
    maybe (return res) throwIO =<< applyCheckStatus (checkStatus req0) res
  where
    go count req' = httpRedirect
      count
      (\req -> do
        res <- httpRaw req manager
        let mreq = getRedirectedRequest req (responseHeaders res) (responseCookieJar res) (statusCode (responseStatus res))
        return (res, mreq))
      req'

-- | Apply 'Request'\'s 'checkStatus' and return resulting exception if any.
applyCheckStatus
    :: (Status -> ResponseHeaders -> CookieJar -> Maybe SomeException)
    -> Response BodyReader
    -> IO (Maybe SomeException)
applyCheckStatus checkStatus' res =
    case checkStatus' (responseStatus res) (responseHeaders res) (responseCookieJar res) of
        Nothing -> return Nothing
        Just exc -> do
            exc' <-
                case fromException exc of
                    Just (StatusCodeException s hdrs cookie_jar) -> do
                        lbs <- brReadSome (responseBody res) 1024
                        return $ toException $ StatusCodeException s (hdrs ++
                            [("X-Response-Body-Start", toStrict' lbs)]) cookie_jar
                    _ -> return exc
            responseClose res
            return (Just exc')
  where
#if MIN_VERSION_bytestring(0,10,0)
    toStrict' = L.toStrict
#else
    toStrict' = S.concat . L.toChunks
#endif

-- | Redirect loop
httpRedirect
     :: Int -- ^ 'redirectCount'
     -> (Request -> IO (Response BodyReader, Maybe Request)) -- ^ function which performs a request and returns a response, and possibly another request if there's a redirect.
     -> Request
     -> IO (Response BodyReader)
httpRedirect count0 http' req0 = go count0 req0 []
  where
    go (-1) _ ress = throwIO . TooManyRedirects =<< mapM lbsResponse ress
    go count req' ress = do
        (res, mreq) <- http' req'
        case mreq of
            Just req -> do
                {- FIXME
                -- Allow the original connection to return to the
                -- connection pool immediately by flushing the body.
                -- If the response body is too large, don't flush, but
                -- instead just close the connection.
                let maxFlush = 1024
                    readMay bs =
                        case S8.readInt bs of
                            Just (i, bs') | S.null bs' -> Just i
                            _ -> Nothing
                    sink =
                        case lookup "content-length" (responseHeaders res) >>= readMay of
                            Just i | i > maxFlush -> return ()
                            _ -> CB.isolate maxFlush C.=$ sinkNull
                lift' $ responseBody res C.$$+- sink
                -}
                responseClose res

                -- And now perform the actual redirect
                go (count - 1) req (res:ress)
            Nothing -> return res
