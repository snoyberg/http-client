{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.Core
    ( withResponse
    , httpLbs
    , httpNoBody
    , httpRaw
    , httpRaw'
    , getModifiedRequestManager
    , responseOpen
    , responseClose
    , httpRedirect
    , httpRedirect'
    , withConnection
    , handleClosedRead
    ) where

import Network.HTTP.Types
import Network.HTTP.Client.Manager
import Network.HTTP.Client.Types
import Network.HTTP.Client.Headers
import Network.HTTP.Client.Body
import Network.HTTP.Client.Request
import Network.HTTP.Client.Response
import Network.HTTP.Client.Cookies
import Data.Maybe (fromMaybe, isJust)
import Data.Time
import Control.Exception
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Control.Monad (void)
import System.Timeout (timeout)
import Data.KeyedPool
import GHC.IO.Exception (IOException(..), IOErrorType(..))

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
httpRaw = fmap (fmap snd) . httpRaw'

-- | Get a 'Response' without any redirect following.
--
-- This extended version of 'httpRaw' also returns the potentially modified Request.
httpRaw'
     :: Request
     -> Manager
     -> IO (Request, Response BodyReader)
httpRaw' req0 m = do
    let req' = mSetProxy m req0
    (req, cookie_jar') <- case cookieJar req' of
        Just cj -> do
            now <- getCurrentTime
            return $ insertCookiesIntoRequest req' (evictExpiredCookies cj now) now
        Nothing -> return (req', Data.Monoid.mempty)
    (timeout', mconn) <- getConnectionWrapper
        (responseTimeout' req)
        (getConn req m)

    -- Originally, we would only test for exceptions when sending the request,
    -- not on calling @getResponse@. However, some servers seem to close
    -- connections after accepting the request headers, so we need to check for
    -- exceptions in both.
    ex <- try $ do
        cont <- requestBuilder (dropProxyAuthSecure req) (managedResource mconn)

        getResponse (mMaxHeaderLength m) (mMaxNumberHeaders m) timeout' req mconn cont

    case ex of
        -- Connection was reused, and might have been closed. Try again
        Left e | managedReused mconn && mRetryableException m e -> do
            managedRelease mconn DontReuse
            httpRaw' req m
        -- Not reused, or a non-retry, so this is a real exception
        Left e -> do
          -- Explicitly release connection for all real exceptions:
          -- https://github.com/snoyberg/http-client/pull/454
          managedRelease mconn DontReuse
          throwIO e
        -- Everything went ok, so the connection is good. If any exceptions get
        -- thrown in the response body, just throw them as normal.
        Right res -> case cookieJar req' of
            Just _ -> do
                now' <- getCurrentTime
                let (cookie_jar, _) = updateCookieJar res req now' cookie_jar'
                return (req, res {responseCookieJar = cookie_jar})
            Nothing -> return (req, res)
  where
    getConnectionWrapper mtimeout f =
        case mtimeout of
            Nothing -> fmap ((,) Nothing) f
            Just timeout' -> do
                before <- getCurrentTime
                mres <- timeout timeout' f
                case mres of
                     Nothing -> throwHttp ConnectionTimeout
                     Just mConn -> do
                         now <- getCurrentTime
                         let timeSpentMicro = diffUTCTime now before * 1000000
                             remainingTime = round $ fromIntegral timeout' - timeSpentMicro
                         if remainingTime <= 0
                             then do
                                 managedRelease mConn DontReuse
                                 throwHttp ConnectionTimeout
                             else return (Just remainingTime, mConn)

    responseTimeout' req =
        case responseTimeout req of
            ResponseTimeoutDefault ->
                case mResponseTimeout m of
                    ResponseTimeoutDefault -> Just 30000000
                    ResponseTimeoutNone -> Nothing
                    ResponseTimeoutMicro u -> Just u
            ResponseTimeoutNone -> Nothing
            ResponseTimeoutMicro u -> Just u

-- | The used Manager can be overridden (by requestManagerOverride) and the used
-- Request can be modified (through managerModifyRequest). This function allows
-- to retrieve the possibly overridden Manager and the possibly modified
-- Request.
--
-- (In case the Manager is overridden by requestManagerOverride, the Request is
-- being modified by managerModifyRequest of the new Manager, not the old one.)
getModifiedRequestManager :: Manager -> Request -> IO (Manager, Request)
getModifiedRequestManager manager0 req0 = do
  let manager = fromMaybe manager0 (requestManagerOverride req0)
  req <- mModifyRequest manager req0
  return (manager, req)

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
responseOpen inputReq manager' = do
  case validateHeaders (requestHeaders inputReq) of
    GoodHeaders -> return ()
    BadHeaders reason -> throwHttp $ InvalidRequestHeader reason
  (manager, req0) <- getModifiedRequestManager manager' inputReq
  wrapExc req0 $ mWrapException manager req0 $ do
    (req, res) <- go manager (redirectCount req0) req0
    checkResponse req req res
    mModifyResponse manager res
        { responseBody = wrapExc req0 (responseBody res)
        }
  where
    wrapExc :: Request -> IO a -> IO a
    wrapExc req0 = handle $ throwIO . toHttpException req0

    go manager0 count req' = httpRedirect'
      count
      (\req -> do
        (manager, modReq) <- getModifiedRequestManager manager0 req
        (req'', res) <- httpRaw' modReq manager
        let mreq = if redirectCount modReq == 0
              then Nothing
              else getRedirectedRequest req' req'' (responseHeaders res) (responseCookieJar res) (statusCode (responseStatus res))
        return (res, fromMaybe req'' mreq, isJust mreq))
      req'

-- | Redirect loop.
httpRedirect
     :: Int -- ^ 'redirectCount'
     -> (Request -> IO (Response BodyReader, Maybe Request)) -- ^ function which performs a request and returns a response, and possibly another request if there's a redirect.
     -> Request
     -> IO (Response BodyReader)
httpRedirect count0 http0 req0 = fmap snd $ httpRedirect' count0 http' req0
  where
    -- adapt callback API
    http' req' = do
        (res, mbReq) <- http0 req'
        return (res, fromMaybe req0 mbReq, isJust mbReq)

handleClosedRead :: SomeException -> IO L.ByteString
handleClosedRead se
    | Just ConnectionClosed <- fmap unHttpExceptionContentWrapper (fromException se)
        = return L.empty
    | Just (HttpExceptionRequest _ ConnectionClosed) <- fromException se
        = return L.empty
    | Just (IOError _ ResourceVanished _ _ _ _) <- fromException se
        = return L.empty
    | otherwise
        = throwIO se

-- | Redirect loop.
--
-- This extended version of 'httpRaw' also returns the Request potentially modified by @managerModifyRequest@.
httpRedirect'
     :: Int -- ^ 'redirectCount'
     -> (Request -> IO (Response BodyReader, Request, Bool)) -- ^ function which performs a request and returns a response, the potentially modified request, and a Bool indicating if there was a redirect.
     -> Request
     -> IO (Request, Response BodyReader)
httpRedirect' count0 http' req0 = go count0 req0 []
  where
    go count _ ress | count < 0 = throwHttp $ TooManyRedirects ress
    go count req' ress = do
        (res, req, isRedirect) <- http' req'
        if isRedirect then do
            -- Allow the original connection to return to the
            -- connection pool immediately by flushing the body.
            -- If the response body is too large, don't flush, but
            -- instead just close the connection.
            let maxFlush = 1024
            lbs <- brReadSome (responseBody res) maxFlush
                -- The connection may already be closed, e.g.
                -- when using withResponseHistory. See
                -- https://github.com/snoyberg/http-client/issues/169
                `Control.Exception.catch` handleClosedRead
            responseClose res

            -- And now perform the actual redirect
            go (count - 1) req (res { responseBody = lbs }:ress)
        else
            return (req, res)

-- | Close any open resources associated with the given @Response@. In general,
-- this will either close an active @Connection@ or return it to the @Manager@
-- to be reused.
--
-- Since 0.1.0
responseClose :: Response a -> IO ()
responseClose = runResponseClose . responseClose'

-- | Perform an action using a @Connection@ acquired from the given @Manager@.
--
-- You should use this only when you have to read and write interactively
-- through the connection (e.g. connection by the WebSocket protocol).
--
-- @since 0.5.13
withConnection :: Request -> Manager -> (Connection -> IO a) -> IO a
withConnection origReq man action = do
    mHttpConn <- getConn (mSetProxy man origReq) man
    action (managedResource mHttpConn) <* keepAlive mHttpConn
        `finally` managedRelease mHttpConn DontReuse
