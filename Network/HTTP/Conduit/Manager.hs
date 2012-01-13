{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.HTTP.Conduit.Manager
    ( Manager
    , ConnKey (..)
    , newManager
    , newManagerCount
    , newManagerIO
    , getConn
    , ConnReuse (..)
    , withManager
    , ConnRelease
    , ManagedConn (..)
    ) where

import Prelude hiding (catch)
import Control.Applicative ((<$>))
import Data.Monoid (mappend)
import System.IO (hClose, hFlush)
import qualified Data.IORef as I
import qualified Data.Map as Map

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L

import qualified Blaze.ByteString.Builder as Blaze

import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad.Base (liftBase)
import Control.Exception.Lifted (mask)
import Control.Exception (mask_, SomeException, catch)
import Control.Monad.Trans.Resource
    ( ResourceT, runResourceT, ResourceIO, withIO
    , register, release
    , newRef, readRef', writeRef
    , safeFromIOBase
    )
import Control.Concurrent (forkIO, threadDelay)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)

import Network (connectTo, PortID (PortNumber))
import Data.Certificate.X509 (X509)

import Network.HTTP.Conduit.ConnInfo
import Network.HTTP.Conduit.Util (hGetSome)
import Network.HTTP.Conduit.Parser (parserHeadersFromByteString)
import Network.HTTP.Conduit.Request

-- | Keeps track of open connections for keep-alive.  May be used
-- concurrently by multiple threads.
data Manager = Manager
    { mConns :: !(I.IORef (Maybe (Map.Map ConnKey (NonEmptyList ConnInfo))))
    -- ^ @Nothing@ indicates that the manager is closed.
    , mMaxConns :: !Int
    -- ^ This is a per-@ConnKey@ value.
    }

data NonEmptyList a =
    One !a !UTCTime |
    Cons !a !Int !UTCTime !(NonEmptyList a)

-- | @ConnKey@ consists of a hostname, a port and a @Bool@
-- specifying whether to use keepalive.
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

-- | Create a new 'Manager' with no open connections and a maximum of 10 open connections..
newManager :: ResourceIO m => ResourceT m Manager
newManager = newManagerCount 10

-- | Create a new 'Manager' with the specified max connection count.
newManagerCount :: ResourceIO m => Int -> ResourceT m Manager
newManagerCount count = snd <$> withIO (newManagerIO count) closeManager

-- | Create a 'Manager' which will never be destroyed.
newManagerIO :: Int -> IO Manager
newManagerIO count = do
    mapRef <- I.newIORef (Just Map.empty)
    _ <- forkIO $ reap mapRef
    return $ Manager mapRef count

-- | Collect and destroy any stale connections.
reap :: I.IORef (Maybe (Map.Map ConnKey (NonEmptyList ConnInfo))) -> IO ()
reap mapRef =
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

withManager :: ResourceIO m => (Manager -> ResourceT m a) -> m a
withManager f = runResourceT $ newManager >>= f

-- | Close all connections in a 'Manager'. Afterwards, the
-- 'Manager' can be reused if desired.
closeManager :: Manager -> IO ()
closeManager manager = do
    m <- I.atomicModifyIORef (mConns manager) $ \x -> (Nothing, x)
    mapM_ (nonEmptyMapM_ safeConnClose) $ maybe [] Map.elems m

safeConnClose :: ConnInfo -> IO ()
safeConnClose ci = connClose ci `catch` \(_::SomeException) -> return ()

nonEmptyMapM_ :: Monad m => (a -> m ()) -> NonEmptyList a -> m ()
nonEmptyMapM_ f (One x _) = f x
nonEmptyMapM_ f (Cons x _ _ l) = f x >> nonEmptyMapM_ f l

getSocketConn
    :: ResourceIO m
    => Manager
    -> String
    -> Int
    -> ResourceT m (ConnRelease m, ConnInfo, ManagedConn)
getSocketConn man host' port' =
    getManagedConn man (ConnKey (T.pack host') port' False) $
        getSocket host' port' >>= socketConn desc
  where
    desc = socketDesc host' port' "unsecured"

socketDesc :: String -> Int -> String -> String
socketDesc h p t = unwords [h, show p, t]

getSslConn :: ResourceIO m
            => ([X509] -> IO TLSCertificateUsage)
            -> Manager
            -> String -- ^ host
            -> Int -- ^ port
            -> ResourceT m (ConnRelease m, ConnInfo, ManagedConn)
getSslConn checkCert man host' port' =
    getManagedConn man (ConnKey (T.pack host') port' True) $
        (connectTo host' (PortNumber $ fromIntegral port') >>= sslClientConn desc checkCert)
  where
    desc = socketDesc host' port' "secured"

getSslProxyConn
            :: ResourceIO m
            => ([X509] -> IO TLSCertificateUsage)
            -> S8.ByteString -- ^ Target host
            -> Int -- ^ Target port
            -> Manager
            -> String -- ^ Proxy host
            -> Int -- ^ Proxy port
            -> ResourceT m (ConnRelease m, ConnInfo, ManagedConn)
getSslProxyConn checkCert thost tport man phost pport =
    getManagedConn man (ConnKey (T.pack phost) pport True) $
        doConnect >>= sslClientConn desc checkCert
  where
    desc = socketDesc phost pport "secured-proxy"
    doConnect = do
        h <- connectTo phost (PortNumber $ fromIntegral pport)
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

data ManagedConn = Fresh | Reused

-- | This function needs to acquire a @ConnInfo@- either from the @Manager@ or
-- via I\/O, and register it with the @ResourceT@ so it is guaranteed to be
-- either released or returned to the manager.
getManagedConn
    :: ResourceIO m
    => Manager
    -> ConnKey
    -> IO ConnInfo
    -> ResourceT m (ConnRelease m, ConnInfo, ManagedConn)
-- We want to avoid any holes caused by async exceptions, so let's mask.
getManagedConn man key open = mask $ \restore -> do
    -- Try to take the socket out of the manager.
    mci <- liftBase $ takeSocket man key
    (ci, isManaged) <-
        case mci of
            -- There wasn't a matching connection in the manager, so create a
            -- new one.
            Nothing -> do
                ci <- restore $ liftBase open
                return (ci, Fresh)
            -- Return the existing one
            Just ci -> return (ci, Reused)

    -- When we release this connection, we can either reuse it (put it back in
    -- the manager) or not reuse it (close the socket). We set up a mutable
    -- reference to track what we want to do. By default, we say not to reuse
    -- it, that way if an exception is thrown, the connection won't be reused.
    toReuseRef <- newRef DontReuse

    -- Now register our release action.
    releaseKey <- register $ do
        toReuse <- readRef' toReuseRef
        -- Determine what action to take based on the value stored in the
        -- toReuseRef variable.
        case toReuse of
            Reuse -> safeFromIOBase $ putSocket man key ci
            DontReuse -> safeFromIOBase $ connClose ci

    -- When the connection is explicitly released, we update our toReuseRef to
    -- indicate what action should be taken, and then call release.
    let connRelease x = do
            writeRef toReuseRef x
            release releaseKey
    return (connRelease, ci, isManaged)

data ConnReuse = Reuse | DontReuse

type ConnRelease m = ConnReuse -> ResourceT m ()

getConn :: ResourceIO m
        => Request m
        -> Manager
        -> ResourceT m (ConnRelease m, ConnInfo, ManagedConn)
getConn req m =
    go m connhost connport
  where
    h = host req
    (useProxy, connhost, connport) =
        case proxy req of
            Just p -> (True, S8.unpack (proxyHost p), proxyPort p)
            Nothing -> (False, S8.unpack h, port req)
    go =
        case (secure req, useProxy) of
            (False, _) -> getSocketConn
            (True, False) -> getSslConn $ checkCerts req h
            (True, True) -> getSslProxyConn (checkCerts req h) h (port req)
