{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Conduit.Manager
    ( Manager
    , ConnKey (..)
    , newManager
    , getConn
    , ConnReuse (..)
    , withManager
    , ConnRelease
    , ManagedConn (..)
    ) where

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
import Control.Monad.Trans.Resource
    ( ResourceT, runResourceT, ResourceIO, withIO
    , register, release
    , newRef, readRef', writeRef
    , safeFromIOBase
    )

import Network (connectTo, PortID (PortNumber))
import Data.Certificate.X509 (X509)

import Network.HTTP.Conduit.ConnInfo
import Network.HTTP.Conduit.Util (hGetSome)
import Network.HTTP.Conduit.Parser (parserHeadersFromByteString)
import Network.HTTP.Conduit.Request


-- | Keeps track of open connections for keep-alive.  May be used
-- concurrently by multiple threads.
newtype Manager = Manager
    { mConns :: I.IORef (Map.Map ConnKey ConnInfo)
    }

-- | @ConnKey@ consists of a hostname, a port and a @Bool@
-- specifying whether to use keepalive.
data ConnKey = ConnKey !Text !Int !Bool
    deriving (Eq, Show, Ord)

takeSocket :: Manager -> ConnKey -> IO (Maybe ConnInfo)
takeSocket man key =
    I.atomicModifyIORef (mConns man) go
  where
    go m = (Map.delete key m, Map.lookup key m)

putSocket :: Manager -> ConnKey -> ConnInfo -> IO ()
putSocket man key ci = do
    msock <- I.atomicModifyIORef (mConns man) go
    maybe (return ()) connClose msock
  where
    go m = (Map.insert key ci m, Map.lookup key m)

-- | Create a new 'Manager' with no open connections.
newManager :: ResourceIO m => ResourceT m Manager
newManager = snd <$> withIO
    (Manager <$> I.newIORef Map.empty)
    closeManager

withManager :: ResourceIO m => (Manager -> ResourceT m a) -> m a
withManager f = runResourceT $ newManager >>= f

-- | Close all connections in a 'Manager'. Afterwards, the
-- 'Manager' can be reused if desired.
closeManager :: Manager -> IO ()
closeManager (Manager i) = do
    m <- I.atomicModifyIORef i $ \x -> (Map.empty, x)
    mapM_ connClose $ Map.elems m

getSocketConn
    :: ResourceIO m
    => Manager
    -> String
    -> Int
    -> ResourceT m (ConnRelease m, ConnInfo, ManagedConn)
getSocketConn man host' port' =
    getManagedConn man (ConnKey (T.pack host') port' False) $
        fmap socketConn $ getSocket host' port'

getSslConn :: ResourceIO m
            => ([X509] -> IO TLSCertificateUsage)
            -> Manager
            -> String -- ^ host
            -> Int -- ^ port
            -> ResourceT m (ConnRelease m, ConnInfo, ManagedConn)
getSslConn checkCert man host' port' =
    getManagedConn man (ConnKey (T.pack host') port' True) $
        (connectTo host' (PortNumber $ fromIntegral port') >>= sslClientConn checkCert)

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
        doConnect >>= sslClientConn checkCert
  where
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

getManagedConn
    :: ResourceIO m
    => Manager
    -> ConnKey
    -> IO ConnInfo
    -> ResourceT m (ConnRelease m, ConnInfo, ManagedConn)
getManagedConn man key open = mask $ \restore -> do
    mci <- liftBase $ takeSocket man key
    (ci, isManaged) <-
        case mci of
            Nothing -> do
                ci <- restore $ liftBase open
                return (ci, Fresh)
            Just ci -> return (ci, Reused)
    toReuseRef <- newRef DontReuse
    releaseKey <- register $ do
        toReuse <- readRef' toReuseRef
        case toReuse of
            Reuse -> safeFromIOBase $ putSocket man key ci
            DontReuse -> safeFromIOBase $ connClose ci
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
