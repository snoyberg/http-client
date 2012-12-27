{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Network.HTTP.Conduit.Manager
    ( Manager
    , ManagerSettings (..)
    , ConnKey (..)
    , newManager
    , closeManager
    , getConn
    , ConnReuse (..)
    , withManager
    , withManagerSettings
    , ConnRelease
    , ManagedConn (..)
    , defaultCheckCerts
    , failedConnectionException
    ) where

#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif
import Data.Monoid (mappend)
import System.IO (hClose, hFlush, IOMode(..))
import qualified Data.IORef as I
import qualified Data.Map as Map

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L

import qualified Blaze.ByteString.Builder as Blaze

import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (mask_, SomeException, catch)
import Control.Monad.Trans.Resource
    ( ResourceT, runResourceT, MonadResource
    , MonadThrow, MonadUnsafeIO
    , allocate, resourceMask, register, release
    )
import Control.Concurrent (forkIO, threadDelay)
import Data.Time (UTCTime (..), Day (..), DiffTime, getCurrentTime, addUTCTime)
import Control.DeepSeq (deepseq)

import Network (connectTo, PortID (PortNumber), HostName)
import Network.Socket (socketToHandle)
import Data.Certificate.X509 (X509, encodeCertificate)
import Data.CertificateStore (CertificateStore)
import System.Certificate.X509 (getSystemCertificateStore)

import Network.TLS (PrivateKey)
import Network.TLS.Extra (certificateVerifyChain, certificateVerifyDomain)

import Network.HTTP.Conduit.ConnInfo
import Network.HTTP.Conduit.Types
import Network.HTTP.Conduit.Util (hGetSome)
import Network.HTTP.Conduit.Parser (parserHeadersFromByteString)
import Network.Socks5 (SocksConf, socksConnectWith)
import Data.Default
import Data.Maybe (mapMaybe)
import System.IO (Handle)

-- | Settings for a @Manager@. Please use the 'def' function and then modify
-- individual settings.
data ManagerSettings = ManagerSettings
    { managerConnCount :: Int
      -- ^ Number of connections to a single host to keep alive. Default: 10.
    , managerCheckCerts :: CertificateStore -> S8.ByteString -> [X509] -> IO CertificateUsage
      -- ^ Check if the server certificate is valid. Only relevant for HTTPS.
    , managerCertStore :: IO CertificateStore
      -- ^ Load up the certificate store. By default uses the system store.
    }

type X509Encoded = L.ByteString

instance Default ManagerSettings where
    def = ManagerSettings
        { managerConnCount = 10
        , managerCheckCerts = defaultCheckCerts
        , managerCertStore = getSystemCertificateStore
        }

-- | Check certificates using the operating system's certificate checker.
defaultCheckCerts :: CertificateStore -> S8.ByteString -> [X509] -> IO CertificateUsage
defaultCheckCerts certStore host' certs =
    case certificateVerifyDomain (S8.unpack host') certs of
        CertificateUsageAccept -> certificateVerifyChain certStore certs
        rejected               -> return rejected

-- | Keeps track of open connections for keep-alive.
-- If possible, you should share a single 'Manager' between multiple threads and requests.
data Manager = Manager
    { mConns :: !(I.IORef (Maybe (Map.Map ConnKey (NonEmptyList ConnInfo))))
    -- ^ @Nothing@ indicates that the manager is closed.
    , mMaxConns :: !Int
    -- ^ This is a per-@ConnKey@ value.
    , mCheckCerts :: S8.ByteString -> [X509] -> IO CertificateUsage
    -- ^ Check if a certificate is valid.
    , mCertCache :: !(I.IORef (Map.Map S8.ByteString (Map.Map X509Encoded UTCTime)))
    -- ^ Cache of validated certificates. The @UTCTime@ gives the expiration
    -- time for the validity of the certificate. The @Ascii@ is the hostname.
    }

data NonEmptyList a =
    One !a !UTCTime |
    Cons !a !Int !UTCTime !(NonEmptyList a)

-- | @ConnKey@ consists of a hostname, a port and a @Bool@
-- specifying whether to use SSL.
data ConnKey = ConnKey !Text !Int !Bool
    deriving (Eq, Show, Ord)

takeSocket :: Manager -> ConnKey -> IO (Maybe ConnInfo)
takeSocket man key =
    I.atomicModifyIORef (mConns man) go
  where
    go Nothing = (Nothing, Nothing)
    go (Just m) =
        case Map.lookup key m of
            Nothing -> (Just m, Nothing)
            Just (One a _) -> (Just $ Map.delete key m, Just a)
            Just (Cons a _ _ rest) -> (Just $ Map.insert key rest m, Just a)

putSocket :: Manager -> ConnKey -> ConnInfo -> IO ()
putSocket man key ci = do
    now <- getCurrentTime
    msock <- I.atomicModifyIORef (mConns man) (go now)
    maybe (return ()) connClose msock
  where
    go _ Nothing = (Nothing, Just ci)
    go now (Just m) =
        case Map.lookup key m of
            Nothing -> (Just $ Map.insert key (One ci now) m, Nothing)
            Just l ->
                let (l', mx) = addToList now (mMaxConns man) ci l
                 in (Just $ Map.insert key l' m, mx)

-- | Add a new element to the list, up to the given maximum number. If we're
-- already at the maximum, return the new value as leftover.
addToList :: UTCTime -> Int -> a -> NonEmptyList a -> (NonEmptyList a, Maybe a)
addToList _ i x l | i <= 1 = (l, Just x)
addToList now _ x l@One{} = (Cons x 2 now l, Nothing)
addToList now maxCount x l@(Cons _ currCount _ _)
    | maxCount > currCount = (Cons x (currCount + 1) now l, Nothing)
    | otherwise = (l, Just x)

-- | Create a 'Manager'. You must manually call 'closeManager' to shut it down.
--
-- Creating a new 'Manager' is an expensive operation, you are advised to share
-- a single 'Manager' between requests instead.
newManager :: ManagerSettings -> IO Manager
newManager ms = do
    icertStore <- I.newIORef Nothing
    let getCertStore = do
            mcertStore <- I.readIORef icertStore
            case mcertStore of
                Nothing -> do
                    certStore <- managerCertStore ms
                    I.writeIORef icertStore $ Just certStore
                    return certStore
                Just x -> return x
    mapRef <- I.newIORef (Just Map.empty)
    certCache <- I.newIORef Map.empty
    _ <- forkIO $ reap mapRef certCache
    return $ Manager mapRef (managerConnCount ms) (\x y -> getCertStore >>= \cs -> managerCheckCerts ms cs x y) certCache

-- | Collect and destroy any stale connections.
reap :: I.IORef (Maybe (Map.Map ConnKey (NonEmptyList ConnInfo)))
     -> I.IORef (Map.Map S8.ByteString (Map.Map X509Encoded UTCTime))
     -> IO ()
reap mapRef certCacheRef =
    mask_ loop
  where
    loop = do
        threadDelay (5 * 1000 * 1000)
        now <- getCurrentTime
        let isNotStale time = 30 `addUTCTime` time >= now
        mtoDestroy <- I.atomicModifyIORef mapRef (findStaleWrap isNotStale)
        case mtoDestroy of
            Nothing -> return () -- manager is closed
            Just toDestroy -> do
                mapM_ safeConnClose toDestroy
                !() <- I.atomicModifyIORef certCacheRef $ \x -> let y = flushStaleCerts now x in y `seq` (y, ())
                loop
    findStaleWrap _ Nothing = (Nothing, Nothing)
    findStaleWrap isNotStale (Just m) =
        let (x, y) = findStale isNotStale m
         in (Just x, Just y)
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

-- | Create a new manager, use it in the provided function, and then release it.
--
-- This function uses the default manager settings. For more control, use
-- 'withManagerSettings'.
withManager :: ( MonadIO m
               , MonadBaseControl IO m
               , MonadThrow m
               , MonadUnsafeIO m
               ) => (Manager -> ResourceT m a) -> m a
withManager f = runResourceT $ do
    (_, manager) <- allocate (newManager def) closeManager
    f manager

-- | Create a new manager with provided settings, use it in the provided function, and then release it.
withManagerSettings :: ( MonadIO m
                       , MonadBaseControl IO m
                       , MonadThrow m
                       , MonadUnsafeIO m
                       ) => ManagerSettings -> (Manager -> ResourceT m a) -> m a
withManagerSettings s f = runResourceT $ do
    (_, manager) <- allocate (newManager s) closeManager
    f manager

-- | Close all connections in a 'Manager'. Afterwards, the
-- 'Manager' can be reused if desired.
closeManager :: Manager -> IO ()
closeManager manager = mask_ $ do
    m <- I.atomicModifyIORef (mConns manager) $ \x -> (Nothing, x)
    mapM_ (nonEmptyMapM_ safeConnClose) $ maybe [] Map.elems m

safeConnClose :: ConnInfo -> IO ()
safeConnClose ci = connClose ci `catch` \(_::SomeException) -> return ()

nonEmptyMapM_ :: Monad m => (a -> m ()) -> NonEmptyList a -> m ()
nonEmptyMapM_ f (One x _) = f x
nonEmptyMapM_ f (Cons x _ _ l) = f x >> nonEmptyMapM_ f l

getSocketConn
    :: MonadResource m
    => Manager
    -> String
    -> Int
    -> Maybe SocksConf -- ^ optional socks proxy
    -> m (ConnRelease m, ConnInfo, ManagedConn)
getSocketConn man host' port' socksProxy' =
    getManagedConn man (ConnKey (T.pack host') port' False) $
        getSocket host' port' socksProxy' >>= socketConn desc
  where
    desc = socketDesc host' port' "unsecured"

socketDesc :: String -> Int -> String -> String
socketDesc h p t = unwords [h, show p, t]

getSslConn :: MonadResource m
            => ([X509] -> IO CertificateUsage)
            -> [(X509, Maybe PrivateKey)]
            -> Manager
            -> String -- ^ host
            -> Int -- ^ port
            -> Maybe SocksConf -- ^ optional socks proxy
            -> m (ConnRelease m, ConnInfo, ManagedConn)
getSslConn checkCert clientCerts man host' port' socksProxy' =
    getManagedConn man (ConnKey (T.pack host') port' True) $
        (connectionTo host' (PortNumber $ fromIntegral port') socksProxy' >>= sslClientConn desc checkCert clientCerts)
  where
    desc = socketDesc host' port' "secured"

getSslProxyConn
            :: MonadResource m
            => ([X509] -> IO CertificateUsage)
            -> [(X509, Maybe PrivateKey)]
            -> S8.ByteString -- ^ Target host
            -> Int -- ^ Target port
            -> Manager
            -> String -- ^ Proxy host
            -> Int -- ^ Proxy port
            -> Maybe SocksConf -- ^ optional SOCKS proxy
            -> m (ConnRelease m, ConnInfo, ManagedConn)
getSslProxyConn checkCert clientCerts thost tport man phost pport socksProxy' =
    getManagedConn man (ConnKey (T.pack phost) pport True) $
        doConnect >>= sslClientConn desc checkCert clientCerts
  where
    desc = socketDesc phost pport "secured-proxy"
    doConnect = do
        h <- connectionTo phost (PortNumber $ fromIntegral pport) socksProxy'
        L.hPutStr h $ Blaze.toLazyByteString connectRequest
        hFlush h
        r <- hGetSome h 2048
        res <- parserHeadersFromByteString r
        case res of
            Right ((_, 200, _), _) -> return h
            Right ((_, _, msg), _) -> hClose h >> proxyError (S8.unpack msg)
            Left s -> hClose h >> proxyError s

    connectRequest =
        Blaze.fromByteString "CONNECT "
            `mappend` Blaze.fromByteString thost
            `mappend` Blaze.fromByteString (S8.pack (':' : show tport))
            `mappend` Blaze.fromByteString " HTTP/1.1\r\n\r\n"
    proxyError s =
        error $ "Proxy failed to CONNECT to '"
                ++ S8.unpack thost ++ ":" ++ show tport ++ "' : " ++ s

-- | This function needs to acquire a @ConnInfo@- either from the @Manager@ or
-- via I\/O, and register it with the @ResourceT@ so it is guaranteed to be
-- either released or returned to the manager.
getManagedConn
    :: MonadResource m
    => Manager
    -> ConnKey
    -> IO ConnInfo
    -> m (ConnRelease m, ConnInfo, ManagedConn)
-- We want to avoid any holes caused by async exceptions, so let's mask.
getManagedConn man key open = resourceMask $ \restore -> do
    -- Try to take the socket out of the manager.
    mci <- liftIO $ takeSocket man key
    (ci, isManaged) <-
        case mci of
            -- There wasn't a matching connection in the manager, so create a
            -- new one.
            Nothing -> do
                ci <- restore $ liftIO open
                return (ci, Fresh)
            -- Return the existing one
            Just ci -> return (ci, Reused)

    -- When we release this connection, we can either reuse it (put it back in
    -- the manager) or not reuse it (close the socket). We set up a mutable
    -- reference to track what we want to do. By default, we say not to reuse
    -- it, that way if an exception is thrown, the connection won't be reused.
    toReuseRef <- liftIO $ I.newIORef DontReuse

    -- Now register our release action.
    releaseKey <- register $ do
        toReuse <- I.readIORef toReuseRef
        -- Determine what action to take based on the value stored in the
        -- toReuseRef variable.
        case toReuse of
            Reuse -> putSocket man key ci
            DontReuse -> connClose ci

    -- When the connection is explicitly released, we update our toReuseRef to
    -- indicate what action should be taken, and then call release.
    let connRelease x = do
            liftIO $ I.writeIORef toReuseRef x
            release releaseKey
    return (connRelease, ci, isManaged)

-- | Create an exception to be thrown if the connection for the given request
-- fails.
failedConnectionException :: Request m -> HttpException
failedConnectionException req =
    FailedConnectionException host port
  where
    (_, host, port) = getConnDest req

getConnDest :: Request m -> (Bool, String, Int)
getConnDest req =
    case proxy req of
        Just p -> (True, S8.unpack (proxyHost p), proxyPort p)
        Nothing -> (False, S8.unpack $ host req, port req)

getConn :: MonadResource m
        => Request m
        -> Manager
        -> m (ConnRelease m, ConnInfo, ManagedConn)
getConn req m =
    go m connhost connport (socksProxy req)
  where
    h = host req
    (useProxy, connhost, connport) = getConnDest req
    go =
        case (secure req, useProxy) of
            (False, _) -> getSocketConn
            (True, False) -> getSslConn (checkCerts m h) (clientCertificates req)
            (True, True) -> getSslProxyConn (checkCerts m h) (clientCertificates req) h (port req)

checkCerts :: Manager -> S8.ByteString -> [X509] -> IO CertificateUsage
checkCerts man host' certs = do
#if DEBUG
    putStrLn $ "checkCerts for host: " ++ show host'
#endif
    cache <- I.readIORef $ mCertCache man
    case Map.lookup host' cache >>= Map.lookup encoded of
        Nothing -> do
#if DEBUG
            putStrLn $ concat ["checkCerts ", show host', " no cached certs found"]
#endif
            res <- mCheckCerts man host' certs
            case res of
                CertificateUsageAccept -> do
#if DEBUG
                    putStrLn $ concat ["checkCerts ", show host', " valid cert, adding to cache"]
#endif
                    now <- getCurrentTime
                    -- keep it valid for 1 hour
                    let expire = (60 * 60) `addUTCTime` now
                    I.atomicModifyIORef (mCertCache man) $ addValidCerts expire
                _ -> return ()
            return res
        Just _ -> do
#if DEBUG
            putStrLn $ concat ["checkCerts ", show host', " cert already cached"]
#endif
            return CertificateUsageAccept
  where
    encoded = L.concat $ map encodeCertificate certs
    addValidCerts expire cache =
        (Map.insert host' inner cache, ())
      where
        inner =
            case Map.lookup host' cache of
                Nothing -> Map.singleton encoded expire
                Just m -> Map.insert encoded expire m

connectionTo :: HostName -> PortID -> Maybe SocksConf -> IO Handle
connectionTo host' port' Nothing = connectTo host' port'
connectionTo host' port' (Just socksConf) =
	socksConnectWith socksConf host' port' >>= flip socketToHandle ReadWriteMode
