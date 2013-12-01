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
    , getConn
    , failedConnectionException
    , defaultManagerSettings
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

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (mask_, SomeException, catch, throwIO, fromException, mask, IOException, Exception (..), handle)
import Control.Concurrent (forkIO, threadDelay)
import Data.Time (UTCTime (..), Day (..), DiffTime, getCurrentTime, addUTCTime)
import Control.DeepSeq (deepseq)

import qualified Network.Socket as NS

import Data.Default
import Data.Maybe (mapMaybe)
import System.IO (Handle)
import System.Mem.Weak (Weak, deRefWeak)
import Network.HTTP.Client.Types
import Network.HTTP.Client.Connection

-- | Default value for @ManagerSettings@.
--
-- Since 0.1.0
defaultManagerSettings :: ManagerSettings
defaultManagerSettings = ManagerSettings
    { managerConnCount = 10
    , managerRawConnection = return openSocketConnection
    , managerTlsConnection = return $ \_ _ _ -> throwIO TlsNotSupported
    , managerResponseTimeout = Just 5000000
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
                    _ -> False
    , managerWrapIOException =
        let wrapper se =
                case fromException se of
                    Just e -> toException $ InternalIOException e
                    Nothing -> se
         in handle $ throwIO . wrapper
    }

takeSocket :: Manager -> ConnKey -> IO (Maybe Connection)
takeSocket man key =
    I.atomicModifyIORef (mConns man) go
  where
    go Nothing = (Nothing, Nothing)
    go (Just m) =
        case Map.lookup key m of
            Nothing -> (Just m, Nothing)
            Just (One a _) -> (Just $ Map.delete key m, Just a)
            Just (Cons a _ _ rest) -> (Just $ Map.insert key rest m, Just a)

putSocket :: Manager -> ConnKey -> Connection -> IO ()
putSocket man key ci = do
    now <- getCurrentTime
    msock <- I.atomicModifyIORef (mConns man) (go now)
    maybe (return ()) connectionClose msock
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

-- | Create a 'Manager'. You may manually call 'closeManager' to shut it down,
-- or allow the @Manager@ to be shut down automatically based on garbage
-- collection.
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
    rawConnection <- managerRawConnection ms
    tlsConnection <- managerTlsConnection ms
    mapRef <- I.newIORef (Just Map.empty)
    wmapRef <- I.mkWeakIORef mapRef $ closeManager' mapRef
    _ <- forkIO $ reap wmapRef
    let manager = Manager
            { mConns = mapRef
            , mMaxConns = managerConnCount ms
            , mResponseTimeout = managerResponseTimeout ms
            , mRawConnection = rawConnection
            , mTlsConnection = tlsConnection
            , mRetryableException = managerRetryableException ms
            , mWrapIOException = managerWrapIOException ms
            }
    return manager

-- | Collect and destroy any stale connections.
reap :: Weak (I.IORef (Maybe (Map.Map ConnKey (NonEmptyList Connection))))
     -> IO ()
reap wmapRef =
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
closeManager = closeManager' . mConns

closeManager' :: I.IORef (Maybe (Map.Map ConnKey (NonEmptyList Connection)))
              -> IO ()
closeManager' connsRef = mask_ $ do
    m <- I.atomicModifyIORef connsRef $ \x -> (Nothing, x)
    mapM_ (nonEmptyMapM_ safeConnClose) $ maybe [] Map.elems m

safeConnClose :: Connection -> IO ()
safeConnClose ci = connectionClose ci `catch` \(_::SomeException) -> return ()

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

    -- When the connection is explicitly released, we update our toReuseRef to
    -- indicate what action should be taken, and then call release.
    let connRelease Reuse = putSocket man key ci
        connRelease DontReuse = connectionClose ci

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

getConn :: Request
        -> Manager
        -> IO (ConnRelease, Connection, ManagedConn)
getConn req m =
    getManagedConn m (ConnKey connKeyHost connport (secure req)) $
        go connaddr connhost connport
  where
    h = host req
    (useProxy, connhost, connport) = getConnDest req
    (connaddr, connKeyHost) =
        case (hostAddress req, useProxy{-, socksProxy req-}) of
            (Just ha, False{-, Nothing-}) -> (Just ha, HostAddress ha)
            _ -> (Nothing, HostName $ T.pack connhost)
    go =
        case (secure req, useProxy) of
            (False, _) -> mRawConnection m
            (True, False) -> mTlsConnection m
            -- FIXME (True, True) -> getSslProxyConn (checkCerts m h) (clientCertificates req) h (port req)
