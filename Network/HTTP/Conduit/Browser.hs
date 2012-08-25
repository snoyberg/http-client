{-# LANGUAGE CPP #-}
-- | This module is designed to work similarly to the Network.Browser module in the HTTP package.
-- The idea is that there are two new types defined: 'BrowserState' and 'BrowserAction'. The
-- purpose of this module is to make it easy to describe a browsing session, including navigating
-- to multiple pages, and have things like cookie jar updates work as expected as you browse
-- around.
--
-- BrowserAction is a monad that handles all your browser-related activities. This monad is
-- actually implemented as a specialization of the State monad, over the BrowserState type. The
-- BrowserState type has various bits of information that a web browser keeps, such as a current
-- cookie jar, the number of times to retry a request on failure, HTTP proxy information, etc. In
-- the BrowserAction monad, there is one BrowserState at any given time, and you can modify it by
-- using the convenience functions in this module.
--
-- A special kind of modification of the current browser state is the action of making a HTTP
-- request. This will do the request according to the params in the current BrowserState, as well
-- as modifying the current state with, for example, an updated cookie jar.
--
-- To use this module, you would bind together a series of BrowserActions (This simulates the user
-- clicking on links or using a settings dialogue etc.) to describe your browsing session. When
-- you've described your session, you call 'browse' on your top-level BrowserAction to actually
-- convert your actions into the ResourceT IO monad.
--
-- Here is an example program:
--
-- > import qualified Data.ByteString as B
-- > import qualified Data.ByteString.Lazy as LB
-- > import qualified Data.ByteString.UTF8 as UB
-- > import           Data.Conduit
-- > import           Network.HTTP.Conduit
-- > import           Network.HTTP.Conduit.Browser
-- > 
-- > -- The web request to log in to a service
-- > req1 :: IO (Request (ResourceT IO))
-- > req1 = do
-- >   req <- parseUrl "http://www.myurl.com/login.php"
-- >   return $ urlEncodedBody [ (UB.fromString "name", UB.fromString "litherum")
-- >                           , (UB.fromString "pass", UB.fromString "S33kRe7")
-- >                           ] req
-- > 
-- > -- Once authenticated, run this request
-- > req2 :: IO (Request m')
-- > req2 = parseUrl "http://www.myurl.com/main.php"
-- > 
-- > -- Bind two BrowserActions together
-- > action :: Request (ResourceT IO) -> Request (ResourceT IO) -> BrowserAction (Response LB.ByteString)
-- > action r1 r2 = do
-- >   _ <- makeRequestLbs r1
-- >   makeRequestLbs r2
-- > 
-- > main :: IO ()
-- > main = do
-- >   man <- newManager def
-- >   r1 <- req1
-- >   r2 <- req2
-- >   out <- runResourceT $ browse man $ action r1 r2
-- >   putStrLn $ UB.toString $ B.concat $ LB.toChunks $ responseBody out

module Network.HTTP.Conduit.Browser
    ( BrowserState
    , BrowserAction
    , browse
    , makeRequest
    , makeRequestLbs
    , defaultState
    , getBrowserState
    , setBrowserState
    , withBrowserState
    , getMaxRedirects 
    , setMaxRedirects 
    , getMaxRetryCount
    , setMaxRetryCount
    , getAuthorities  
    , setAuthorities  
    , getCookieFilter 
    , setCookieFilter 
    , getCookieJar    
    , setCookieJar    
    , getCurrentProxy 
    , setCurrentProxy 
    , getUserAgent    
    , setUserAgent    
    , getManager      
    , setManager
    )
  where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Control.Monad.State
import Control.Exception
import qualified Control.Exception.Lifted as LE
import Data.Conduit
#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif
import qualified Network.HTTP.Types as HT
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.CaseInsensitive (mk)
import Data.ByteString.UTF8 (fromString)
import Data.List (partition)
import Web.Cookie (parseSetCookie)
import Data.Default (def)
import Data.Maybe (catMaybes)

import Network.HTTP.Conduit.Cookies hiding (updateCookieJar)
import Network.HTTP.Conduit.Request
import Network.HTTP.Conduit.Response
import Network.HTTP.Conduit.Manager
import qualified Network.HTTP.Conduit as HC

data BrowserState = BrowserState
  { maxRedirects        :: Int
  , maxRetryCount       :: Int
  , authorities         :: Request (ResourceT IO) -> Maybe (BS.ByteString, BS.ByteString)
  , cookieFilter        :: Request (ResourceT IO) -> Cookie -> IO Bool
  , cookieJar           :: CookieJar
  , currentProxy        :: Maybe Proxy
  , userAgent           :: BS.ByteString
  , manager             :: Manager
  } 

defaultState :: Manager -> BrowserState
defaultState m = BrowserState { maxRedirects = 10
                              , maxRetryCount = 1
                              , authorities = \ _ -> Nothing
                              , cookieFilter = \ _ _ -> return True
                              , cookieJar = def
                              , currentProxy = Nothing
                              , userAgent = fromString "http-conduit"
                              , manager = m
                              }

type BrowserAction = StateT BrowserState (ResourceT IO)

-- | Do the browser action with the given manager
browse :: Manager -> BrowserAction a -> ResourceT IO a
browse m act = evalStateT act (defaultState m)

-- | Make a request, using all the state in the current BrowserState
makeRequest :: Request (ResourceT IO) -> BrowserAction (Response (ResumableSource (ResourceT IO) BS.ByteString))
makeRequest request = do
  BrowserState
    { maxRetryCount = max_retry_count
    , currentProxy  = current_proxy
    , userAgent     = user_agent
    } <- get
  retryHelper (applyUserAgent user_agent $
    request { redirectCount = 0
            , proxy = current_proxy
            , checkStatus = \ _ _ -> Nothing
            }) max_retry_count Nothing
  where retryHelper request' retry_count e
          | retry_count == 0 = case e of
            Just e' -> throw e'
            Nothing -> throw TooManyRetries
          | otherwise = do
              BrowserState {maxRedirects = max_redirects} <- get
              resp <- LE.catch (if max_redirects==0
                                  then (\(_,a,_) -> a) `fmap` performRequest request'
                                  else runRedirectionChain request' max_redirects [])
                (\ e' -> retryHelper request' (retry_count - 1) (Just (e' :: HttpException)))
              let code = HT.statusCode $ HC.responseStatus resp
              if code < 200 || code >= 300
                then retryHelper request' (retry_count - 1) (Just $ HC.StatusCodeException (HC.responseStatus resp) (HC.responseHeaders resp))
                else return resp
        performRequest request' = do
              s@(BrowserState { manager = manager'
                              , authorities = auths
                              , cookieJar = cookie_jar
                              , cookieFilter = cookie_filter
                              }) <- get
              now <- liftIO getCurrentTime
              let (request'', cookie_jar') = insertCookiesIntoRequest
                                              (applyAuthorities auths request')
                                              (evictExpiredCookies cookie_jar now) now
              res <- lift $ HC.http request'' manager'
              (cookie_jar'', response) <- liftIO $ updateCookieJar res request'' now cookie_jar' cookie_filter
              put $ s {cookieJar = cookie_jar''}
              return (request'', res, response)
        runRedirectionChain request' redirect_count ress
          | redirect_count == (-1) = throw . TooManyRedirects =<< mapM (liftIO . runResourceT . lbsResponse) ress
          | otherwise = do
              (request'', res, response) <- performRequest request'
              let code = HT.statusCode (HC.responseStatus response)
              if code >= 300 && code < 400
                then do request''' <- case HC.getRedirectedRequest request'' (responseHeaders response) code of
                            Just a -> return a
                            Nothing -> throw . HC.UnparseableRedirect =<< (liftIO $ runResourceT $ lbsResponse response)
                        runRedirectionChain request''' (redirect_count - 1) (res:ress)
                else return res
        applyAuthorities auths request' = case auths request' of
          Just (user, pass) -> applyBasicAuth user pass request'
          Nothing -> request'
        applyUserAgent ua request' = request' {requestHeaders = (k, ua) : hs}
          where hs = filter ((/= k) . fst) $ requestHeaders request'
                k = mk $ fromString "User-Agent"

makeRequestLbs :: Request (ResourceT IO) -> BrowserAction (Response L.ByteString)
makeRequestLbs = liftIO . runResourceT . lbsResponse <=< makeRequest

updateCookieJar :: Response a -> Request (ResourceT IO) -> UTCTime -> CookieJar -> (Request (ResourceT IO) -> Cookie -> IO Bool) -> IO (CookieJar, Response a)
updateCookieJar response request' now cookie_jar cookie_filter = do
  filtered_cookies <- filterM (cookie_filter request') $ catMaybes $ map (\ sc -> generateCookie sc request' now True) set_cookies
  return (cookieJar' filtered_cookies, response {HC.responseHeaders = other_headers})
  where (set_cookie_headers, other_headers) = partition ((== (mk $ fromString "Set-Cookie")) . fst) $ HC.responseHeaders response
        set_cookie_data = map snd set_cookie_headers
        set_cookies = map parseSetCookie set_cookie_data
        cookieJar' = foldl (\ cj c -> insertCheckedCookie c cj True) cookie_jar

-- | You can save and restore the state at will
getBrowserState :: BrowserAction BrowserState
getBrowserState = get
setBrowserState :: BrowserState -> BrowserAction ()
setBrowserState = put
withBrowserState :: BrowserState -> BrowserAction a -> BrowserAction a
withBrowserState s a = do
  current <- get
  put s
  out <- a
  put current
  return out

-- | The number of redirects to allow
getMaxRedirects    :: BrowserAction Int
getMaxRedirects    = get >>= \ a -> return $ maxRedirects a
setMaxRedirects    :: Int -> BrowserAction ()
setMaxRedirects  b = get >>= \ a -> put a {maxRedirects = b}
-- | The number of times to retry a failed connection
getMaxRetryCount   :: BrowserAction Int
getMaxRetryCount   = get >>= \ a -> return $ maxRetryCount a
setMaxRetryCount    :: Int -> BrowserAction ()
setMaxRetryCount b = get >>= \ a -> put a {maxRetryCount = b}
-- | A user-provided function that provides optional authorities.
-- This function gets run on all requests before they get sent out.
-- The output of this function is applied to the request.
getAuthorities     :: BrowserAction (Request (ResourceT IO) -> Maybe (BS.ByteString, BS.ByteString))
getAuthorities     = get >>= \ a -> return $ authorities a
setAuthorities     :: (Request (ResourceT IO) -> Maybe (BS.ByteString, BS.ByteString)) -> BrowserAction ()
setAuthorities   b = get >>= \ a -> put a {authorities = b}
-- | Each new Set-Cookie the browser encounters will pass through this filter.
-- Only cookies that pass the filter (and are already valid) will be allowed into the cookie jar
getCookieFilter    :: BrowserAction (Request (ResourceT IO) -> Cookie -> IO Bool)
getCookieFilter    = get >>= \ a -> return $ cookieFilter a
setCookieFilter    :: (Request (ResourceT IO) -> Cookie -> IO Bool) -> BrowserAction ()
setCookieFilter  b = get >>= \ a -> put a {cookieFilter = b}
-- | All the cookies!
getCookieJar       :: BrowserAction CookieJar
getCookieJar       = get >>= \ a -> return $ cookieJar a
setCookieJar       :: CookieJar -> BrowserAction ()
setCookieJar     b = get >>= \ a -> put a {cookieJar = b}
-- | An optional proxy to send all requests through
getCurrentProxy    :: BrowserAction (Maybe Proxy)
getCurrentProxy    = get >>= \ a -> return $ currentProxy a
setCurrentProxy    :: Maybe Proxy -> BrowserAction ()
setCurrentProxy  b = get >>= \ a -> put a {currentProxy = b}
-- | What string to report our user-agent as
getUserAgent       :: BrowserAction BS.ByteString
getUserAgent       = get >>= \ a -> return $ userAgent a
setUserAgent       :: BS.ByteString -> BrowserAction ()
setUserAgent     b = get >>= \ a -> put a {userAgent = b}
-- | The active manager, managing the connection pool
getManager         :: BrowserAction Manager
getManager         = get >>= \ a -> return $ manager a
setManager         :: Manager -> BrowserAction ()
setManager       b = get >>= \ a -> put a {manager = b}
