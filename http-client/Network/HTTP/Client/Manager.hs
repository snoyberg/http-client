{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Network.HTTP.Client.Manager
    ( ManagerSettings (..)
    , newManager
    , closeManager
    , withManager
    , getConn
    , failedConnectionException
    , defaultManagerSettings
    , rawConnectionModifySocket
    , proxyFromRequest
    , noProxy
    , useProxy
    , proxyEnvironment
    , proxyEnvironmentNamed
    , defaultProxy
    , dropProxyAuthSecure
    ) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif
#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif
import Control.Applicative ((<|>))
import Control.Arrow (first)
import Data.Monoid (mappend)
import System.IO (hClose, hFlush, IOMode(..))
import qualified Data.IORef as I
import qualified Data.Map as Map

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L

import qualified Blaze.ByteString.Builder as Blaze

import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (decimal)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (unless, join, when, void, mplus)
import Control.Exception (mask_, SomeException, bracket, catch, throwIO, fromException, mask, IOException, Exception (..), handle)
import Control.Concurrent (forkIO, threadDelay)
import Data.Time (UTCTime (..), Day (..), DiffTime, getCurrentTime, addUTCTime)
import Control.DeepSeq (deepseq)

import qualified Network.Socket as NS

import Data.Maybe (mapMaybe)
import System.IO (Handle)
import System.Mem.Weak (Weak, deRefWeak)
import Network.HTTP.Types (status200)
import Network.HTTP.Client.Types
import Network.HTTP.Client.Connection
import Network.HTTP.Client.Headers (parseStatusHeaders)
import Network.HTTP.Client.Request (applyBasicProxyAuth, extractBasicAuthInfo)
import Control.Concurrent.MVar (MVar, takeMVar, tryPutMVar, newEmptyMVar)
import System.Environment (getEnvironment)
import qualified Network.URI as U
import Control.Monad (guard)

-- | A value for the @managerRawConnection@ setting, but also allows you to
-- modify the underlying @Socket@ to set additional settings. For a motivating
-- use case, see: <https://github.com/snoyberg/http-client/issues/71>.
--
-- Since 0.3.8
rawConnectionModifySocket :: (NS.Socket -> IO ())
                          -> IO (Maybe NS.HostAddress -> String -> Int -> IO Connection)
rawConnectionModifySocket = return . openSocketConnection

-- | Same as @rawConnectionModifySocket@, but also takes in a chunk size.
--
-- Since 0.4.5
rawConnectionModifySocketSize :: (NS.Socket -> IO ())
                              -> IO (Int -> Maybe NS.HostAddress -> String -> Int -> IO Connection)
rawConnectionModifySocketSize = return . openSocketConnectionSize

-- | Default value for @ManagerSettings@.
--
-- Note that this value does /not/ have support for SSL/TLS. If you need to
-- make any https connections, please use the http-client-tls package, which
-- provides a @tlsManagerSettings@ value.
--
-- Since 0.1.0
defaultManagerSettings :: ManagerSettings
defaultManagerSettings = ManagerSettings
    { managerConnCount = 10
    , managerRawConnection = return $ openSocketConnection (const $ return ())
    , managerTlsConnection = return $ \_ _ _ -> throwIO TlsNotSupported
    , managerTlsProxyConnection = return $ \_ _ _ _ _ _ -> throwIO TlsNotSupported
    , managerResponseTimeout = Just 30000000
    , managerRetryableException = \e ->
        case fromException e of
            Just (_ :: IOException) -> True
            _ ->
                case fromException e of
                    -- Note: Some servers will timeout connections by accepting
                    -- the incoming packets for the new request, but closing
                    -- the connection as soon as we try to read. To make sure
                    -- we open a new connection under these circumstances, we
                    -- check for the NoResponseDataReceived exception.
                    Just NoResponseDataReceived -> True
                    Just IncompleteHeaders -> True
                    _ -> False
    , managerWrapIOException =
        let wrapper se =
                case fromException se of
                    Just e -> toException $ InternalIOException e
                    Nothing -> se
         in handle $ throwIO . wrapper
    , managerIdleConnectionCount = 512
    , managerModifyRequest = return
    , managerProxyInsecure = defaultProxy
    , managerProxySecure = defaultProxy
    }

takeSocket :: Manager -> ConnKey -> IO (Maybe Connection)
takeSocket man key =
    I.atomicModifyIORef (mConns man) go
  where
    go ManagerClosed = (ManagerClosed, Nothing)
    go mcOrig@(ManagerOpen idleCount m) =
        case Map.lookup key m of
            Nothing -> (mcOrig, Nothing)
            Just (One a _) ->
                let mc = ManagerOpen (idleCount - 1) (Map.delete key m)
                 in mc `seq` (mc, Just a)
            Just (Cons a _ _ rest) ->
                let mc = ManagerOpen (idleCount - 1) (Map.insert key rest m)
                 in mc `seq` (mc, Just a)

putSocket :: Manager -> ConnKey -> Connection -> IO ()
putSocket man key ci = do
    now <- getCurrentTime
    join $ I.atomicModifyIORef (mConns man) (go now)
    void $ tryPutMVar (mConnsBaton man) ()
  where
    go _ ManagerClosed = (ManagerClosed , connectionClose ci)
    go now mc@(ManagerOpen idleCount m)
        | idleCount >= mIdleConnectionCount man = (mc, connectionClose ci)
        | otherwise = case Map.lookup key m of
            Nothing ->
                let cnt' = idleCount + 1
                    m' = ManagerOpen cnt' (Map.insert key (One ci now) m)
                 in m' `seq` (m', return ())
            Just l ->
                let (l', mx) = addToList now (mMaxConns man) ci l
                    cnt' = idleCount + maybe 1 (const 0) mx
                    m' = ManagerOpen cnt' (Map.insert key l' m)
                 in m' `seq` (m', maybe (return ()) connectionClose mx)

-- | Add a new element to the list, up to the given maximum number. If we're
-- already at the maximum, return the new value as leftover.
addToList :: UTCTime -> Int -> a -> NonEmptyList a -> (NonEmptyList a, Maybe a)
addToList _ i x l | i <= 1 = (l, Just x)
addToList now _ x l@One{} = (Cons x 2 now l, Nothing)
addToList now maxCount x l@(Cons _ currCount _ _)
    | maxCount > currCount = (Cons x (currCount + 1) now l, Nothing)
    | otherwise = (l, Just x)

-- | Create a 'Manager'. The @Manager@ will be shut down automatically via
-- garbage collection.
--
-- Creating a new 'Manager' is a relatively expensive operation, you are
-- advised to share a single 'Manager' between requests instead.
--
-- The first argument to this function is often 'defaultManagerSettings',
-- though add-on libraries may provide a recommended replacement.
--
-- Since 0.1.0
newManager :: ManagerSettings -> IO Manager
newManager ms = do
    NS.withSocketsDo $ return ()
    rawConnection <- managerRawConnection ms
    tlsConnection <- managerTlsConnection ms
    tlsProxyConnection <- managerTlsProxyConnection ms
    mapRef <- I.newIORef $! ManagerOpen 0 Map.empty
    baton <- newEmptyMVar
    wmapRef <- I.mkWeakIORef mapRef $ closeManager' mapRef

    httpProxy <- runProxyOverride (managerProxyInsecure ms) False
    httpsProxy <- runProxyOverride (managerProxySecure ms) True

    _ <- forkIO $ reap baton wmapRef
    let manager = Manager
            { mConns = mapRef
            , mConnsBaton = baton
            , mMaxConns = managerConnCount ms
            , mResponseTimeout = managerResponseTimeout ms
            , mRawConnection = rawConnection
            , mTlsConnection = tlsConnection
            , mTlsProxyConnection = tlsProxyConnection
            , mRetryableException = managerRetryableException ms
            , mWrapIOException = managerWrapIOException ms
            , mIdleConnectionCount = managerIdleConnectionCount ms
            , mModifyRequest = managerModifyRequest ms
            , mSetProxy = \req ->
                if secure req
                    then httpsProxy req
                    else httpProxy req
            }
    return manager

-- | Collect and destroy any stale connections.
reap :: MVar () -> Weak (I.IORef ConnsMap) -> IO ()
reap baton wmapRef =
    mask_ loop
  where
    loop = do
        threadDelay (5 * 1000 * 1000)
        mmapRef <- deRefWeak wmapRef
        case mmapRef of
            Nothing -> return () -- manager is closed
            Just mapRef -> goMapRef mapRef

    goMapRef mapRef = do
        now <- getCurrentTime
        let isNotStale time = 30 `addUTCTime` time >= now
        (newMap, toDestroy) <- I.atomicModifyIORef mapRef $ \m ->
            let (newMap, toDestroy) = findStaleWrap isNotStale m
             in (newMap, (newMap, toDestroy))
        mapM_ safeConnClose toDestroy
        case newMap of
            ManagerOpen _ m | not $ Map.null m -> return ()
            _ -> takeMVar baton
        loop
    findStaleWrap _ ManagerClosed = (ManagerClosed, [])
    findStaleWrap isNotStale (ManagerOpen idleCount m) =
        let (x, y) = findStale isNotStale m
         in (ManagerOpen (idleCount - length y) x, y)
    findStale isNotStale =
        findStale' id id . Map.toList
      where
        findStale' destroy keep [] = (Map.fromList $ keep [], destroy [])
        findStale' destroy keep ((connkey, nelist):rest) =
            findStale' destroy' keep' rest
          where
            -- Note: By definition, the timestamps must be in descending order,
            -- so we don't need to traverse the whole list.
            (notStale, stale) = span (isNotStale . fst) $ neToList nelist
            destroy' = destroy . (map snd stale++)
            keep' =
                case neFromList notStale of
                    Nothing -> keep
                    Just x -> keep . ((connkey, x):)

    flushStaleCerts now =
        Map.fromList . mapMaybe flushStaleCerts' . Map.toList
      where
        flushStaleCerts' (host', inner) =
            case mapMaybe flushStaleCerts'' $ Map.toList inner of
                [] -> Nothing
                pairs ->
                    let x = take 10 pairs
                     in x `seqPairs` Just (host', Map.fromList x)
        flushStaleCerts'' (certs, expires)
            | expires > now = Just (certs, expires)
            | otherwise     = Nothing

        seqPairs :: [(L.ByteString, UTCTime)] -> b -> b
        seqPairs [] b = b
        seqPairs (p:ps) b = p `seqPair` ps `seqPairs` b

        seqPair :: (L.ByteString, UTCTime) -> b -> b
        seqPair (lbs, utc) b = lbs `seqLBS` utc `seqUTC` b

        seqLBS :: L.ByteString -> b -> b
        seqLBS lbs b = L.length lbs `seq` b

        seqUTC :: UTCTime -> b -> b
        seqUTC (UTCTime day dt) b = day `seqDay` dt `seqDT` b

        seqDay :: Day -> b -> b
        seqDay (ModifiedJulianDay i) b = i `deepseq` b

        seqDT :: DiffTime -> b -> b
        seqDT = seq

neToList :: NonEmptyList a -> [(UTCTime, a)]
neToList (One a t) = [(t, a)]
neToList (Cons a _ t nelist) = (t, a) : neToList nelist

neFromList :: [(UTCTime, a)] -> Maybe (NonEmptyList a)
neFromList [] = Nothing
neFromList [(t, a)] = Just (One a t)
neFromList xs =
    Just . snd . go $ xs
  where
    go [] = error "neFromList.go []"
    go [(t, a)] = (2, One a t)
    go ((t, a):rest) =
        let (i, rest') = go rest
            i' = i + 1
         in i' `seq` (i', Cons a i t rest')

-- | Close all connections in a 'Manager'.
--
-- Note that this doesn't affect currently in-flight connections,
-- meaning you can safely use it without hurting any queries you may
-- have concurrently running.
--
-- Since 0.1.0
closeManager :: Manager -> IO ()
closeManager _ = return ()
{-# DEPRECATED closeManager "Manager will be closed for you automatically when no longer in use" #-}

closeManager' :: I.IORef ConnsMap
              -> IO ()
closeManager' connsRef = mask_ $ do
    !m <- I.atomicModifyIORef connsRef $ \x -> (ManagerClosed, x)
    case m of
        ManagerClosed -> return ()
        ManagerOpen _ m -> mapM_ (nonEmptyMapM_ safeConnClose) $ Map.elems m

-- | Create, use and close a 'Manager'.
--
-- Since 0.2.1
withManager :: ManagerSettings -> (Manager -> IO a) -> IO a
withManager settings f = newManager settings >>= f
{-# DEPRECATED withManager "Use newManager instead" #-}

safeConnClose :: Connection -> IO ()
safeConnClose ci = connectionClose ci `catch` \(_ :: IOException) -> return ()

nonEmptyMapM_ :: Monad m => (a -> m ()) -> NonEmptyList a -> m ()
nonEmptyMapM_ f (One x _) = f x
nonEmptyMapM_ f (Cons x _ _ l) = f x >> nonEmptyMapM_ f l

-- | This function needs to acquire a @ConnInfo@- either from the @Manager@ or
-- via I\/O, and register it with the @ResourceT@ so it is guaranteed to be
-- either released or returned to the manager.
getManagedConn
    :: Manager
    -> ConnKey
    -> IO Connection
    -> IO (ConnRelease, Connection, ManagedConn)
-- We want to avoid any holes caused by async exceptions, so let's mask.
getManagedConn man key open = mask $ \restore -> do
    -- Try to take the socket out of the manager.
    mci <- takeSocket man key
    (ci, isManaged) <-
        case mci of
            -- There wasn't a matching connection in the manager, so create a
            -- new one.
            Nothing -> do
                ci <- restore open
                return (ci, Fresh)
            -- Return the existing one
            Just ci -> return (ci, Reused)

    -- When we release this connection, we can either reuse it (put it back in
    -- the manager) or not reuse it (close the socket). We set up a mutable
    -- reference to track what we want to do. By default, we say not to reuse
    -- it, that way if an exception is thrown, the connection won't be reused.
    toReuseRef <- I.newIORef DontReuse
    wasReleasedRef <- I.newIORef False

    -- When the connection is explicitly released, we update our toReuseRef to
    -- indicate what action should be taken, and then call release.
    let connRelease r = do
            I.writeIORef toReuseRef r
            releaseHelper

        releaseHelper = mask $ \restore -> do
            wasReleased <- I.atomicModifyIORef wasReleasedRef $ \x -> (True, x)
            unless wasReleased $ do
                toReuse <- I.readIORef toReuseRef
                restore $ case toReuse of
                    Reuse -> putSocket man key ci
                    DontReuse -> connectionClose ci

    return (connRelease, ci, isManaged)

-- | Create an exception to be thrown if the connection for the given request
-- fails.
failedConnectionException :: Request -> HttpException
failedConnectionException req =
    FailedConnectionException host' port'
  where
    (_, host', port') = getConnDest req

getConnDest :: Request -> (Bool, String, Int)
getConnDest req =
    case proxy req of
        Just p -> (True, S8.unpack (proxyHost p), proxyPort p)
        Nothing -> (False, S8.unpack $ host req, port req)

-- | Drop the Proxy-Authorization header from the request if we're using a
-- secure proxy.
dropProxyAuthSecure :: Request -> Request
dropProxyAuthSecure req
    | secure req && useProxy = req
        { requestHeaders = filter (\(k, _) -> k /= "Proxy-Authorization")
                                  (requestHeaders req)
        }
    | otherwise = req
  where
    (useProxy, _, _) = getConnDest req

getConn :: Request
        -> Manager
        -> IO (ConnRelease, Connection, ManagedConn)
getConn req m
    -- Stop Mac OS X from getting high:
    -- https://github.com/snoyberg/http-client/issues/40#issuecomment-39117909
    | S8.null h = throwIO $ InvalidDestinationHost h
    | otherwise =
        getManagedConn m (ConnKey connKeyHost connport (host req) (port req) (secure req)) $
            wrapConnectExc $ go connaddr connhost connport
  where
    h = host req
    (useProxy, connhost, connport) = getConnDest req
    (connaddr, connKeyHost) =
        case (hostAddress req, useProxy) of
            (Just ha, False) -> (Just ha, HostAddress ha)
            _ -> (Nothing, HostName $ T.pack connhost)

    wrapConnectExc = handle $ \e ->
        throwIO $ FailedConnectionException2 connhost connport (secure req)
            (toException (e :: IOException))
    go =
        case (secure req, useProxy) of
            (False, _) -> mRawConnection m
            (True, False) -> mTlsConnection m
            (True, True) ->
                let ultHost = host req
                    ultPort = port req
                    proxyAuthorizationHeader = maybe "" (\h -> S8.concat ["Proxy-Authorization: ", h, "\r\n"]) . lookup "Proxy-Authorization" $ requestHeaders req
                    connstr = S8.concat
                        [ "CONNECT "
                        , ultHost
                        , ":"
                        , S8.pack $ show ultPort
                        , " HTTP/1.1\r\n"
                        , proxyAuthorizationHeader
                        , "\r\n"
                        ]
                    parse conn = do
                        sh@(StatusHeaders status _ _) <- parseStatusHeaders conn Nothing Nothing
                        unless (status == status200) $
                            throwIO $ ProxyConnectException ultHost ultPort $ Right $ StatusCodeException status [] (CJ [])
                 in mTlsProxyConnection m connstr parse (S8.unpack ultHost)

-- | Get the proxy settings from the @Request@ itself.
--
-- Since 0.4.7
proxyFromRequest :: ProxyOverride
proxyFromRequest = ProxyOverride $ const $ return id

-- | Never connect using a proxy, regardless of the proxy value in the @Request@.
--
-- Since 0.4.7
noProxy :: ProxyOverride
noProxy = ProxyOverride $ const $ return $ \req -> req { proxy = Nothing }

-- | Use the given proxy settings, regardless of the proxy value in the @Request@.
--
-- Since 0.4.7
useProxy :: Proxy -> ProxyOverride
useProxy p = ProxyOverride $ const $ return $ \req -> req { proxy = Just p }

-- | Get the proxy settings from the default environment variable (@http_proxy@
-- for insecure, @https_proxy@ for secure). If no variable is set, then fall
-- back to the given value. @Nothing@ is equivalent to 'noProxy', @Just@ is
-- equivalent to 'useProxy'.
--
-- Since 0.4.7
proxyEnvironment :: Maybe Proxy -- ^ fallback if no environment set
                 -> ProxyOverride
proxyEnvironment mp = ProxyOverride $ \secure ->
    envHelper (envName secure) $ maybe EHNoProxy EHUseProxy mp

envName :: Bool -- ^ secure?
        -> Text
envName False = "http_proxy"
envName True = "https_proxy"

-- | Same as 'proxyEnvironment', but instead of default environment variable
-- names, allows you to set your own name.
--
-- Since 0.4.7
proxyEnvironmentNamed
    :: Text -- ^ environment variable name
    -> Maybe Proxy -- ^ fallback if no environment set
    -> ProxyOverride
proxyEnvironmentNamed name =
    ProxyOverride . const . envHelper name
                  . maybe EHNoProxy EHUseProxy

-- | The default proxy settings for a manager. In particular: if the @http_proxy@ (or @https_proxy@) environment variable is set, use it. Otherwise, use the values in the @Request@.
--
-- Since 0.4.7
defaultProxy :: ProxyOverride
defaultProxy = ProxyOverride $ \secure ->
    envHelper (envName secure) EHFromRequest

data EnvHelper = EHFromRequest
               | EHNoProxy
               | EHUseProxy Proxy

envHelper :: Text -> EnvHelper -> IO (Request -> Request)
envHelper name eh = do
    env <- getEnvironment
    let lenv = Map.fromList $ map (first $ T.toLower . T.pack) env
        lookupEnvVar n = lookup (T.unpack n) env <|> Map.lookup n lenv
        noProxyDomains = domainSuffixes (lookupEnvVar "no_proxy")
    case lookupEnvVar name of
        Nothing  -> return noEnvProxy
        Just ""  -> return noEnvProxy
        Just str -> do
            let invalid = throwIO $ InvalidProxyEnvironmentVariable name (T.pack str)
            (p, muserpass) <- maybe invalid return $ do
                uri <- case U.parseURI str of
                    Just u | U.uriScheme u == "http:" -> return u
                    _ -> U.parseURI $ "http://" ++ str

                guard $ U.uriScheme uri == "http:"
                guard $ null (U.uriPath uri) || U.uriPath uri == "/"
                guard $ null $ U.uriQuery uri
                guard $ null $ U.uriFragment uri

                auth <- U.uriAuthority uri
                port <-
                    case U.uriPort auth of
                        "" -> Just 80
                        ':':rest ->
                            case decimal $ T.pack rest of
                                Right (p, "") -> Just p
                                _ -> Nothing
                        _ -> Nothing

                Just $ (Proxy (S8.pack $ U.uriRegName auth) port, extractBasicAuthInfo uri)
            return $ \req ->
                if host req `hasDomainSuffixIn` noProxyDomains
                then noEnvProxy req
                else maybe id (uncurry applyBasicProxyAuth) muserpass
                     req { proxy = Just p }
    where noEnvProxy = case eh of
            EHFromRequest -> id
            EHNoProxy     -> \req -> req { proxy = Nothing }
            EHUseProxy p  -> \req -> req { proxy = Just p  }
          prefixed s | S8.head s == '.' = s
                     | otherwise = S8.cons '.' s
          domainSuffixes Nothing = []
          domainSuffixes (Just "") = []
          domainSuffixes (Just no_proxy) = [prefixed $ S8.dropWhile (== ' ') suffix | suffix <- S8.split ',' (S8.pack (map toLower no_proxy)), not (S8.null suffix)]
          hasDomainSuffixIn host = any (`S8.isSuffixOf` prefixed (S8.map toLower host))
