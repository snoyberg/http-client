{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Conduit.Manager
    ( Manager
    , ConnKey (..)
    , newManager
    , withConn
    , WithConnResponse (..)
    , ConnReuse (..)
    , UseConn
    , withManager
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
import Control.Exception.Lifted (mask, try, throwIO, SomeException)
import Control.Monad.Trans.Resource

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

type UseConn m a = ConnInfo -> ResourceT m (WithConnResponse a)

withSocketConn
    :: ResourceIO m
    => Manager
    -> String
    -> Int
    -> UseConn m a
    -> ResourceT m a
withSocketConn man host' port' =
    withManagedConn man (ConnKey (T.pack host') port' False) $
        fmap socketConn $ getSocket host' port'

withSslConn :: ResourceIO m
            => ([X509] -> IO TLSCertificateUsage)
            -> Manager
            -> String -- ^ host
            -> Int -- ^ port
            -> UseConn m a
            -> ResourceT m a
withSslConn checkCert man host' port' =
    withManagedConn man (ConnKey (T.pack host') port' True) $
        (connectTo host' (PortNumber $ fromIntegral port') >>= sslClientConn checkCert)

withSslProxyConn
            :: ResourceIO m
            => ([X509] -> IO TLSCertificateUsage)
            -> S8.ByteString -- ^ Target host
            -> Int -- ^ Target port
            -> Manager
            -> String -- ^ Proxy host
            -> Int -- ^ Proxy port
            -> UseConn m a
            -> ResourceT m a
withSslProxyConn checkCert thost tport man phost pport =
    withManagedConn man (ConnKey (T.pack phost) pport True) $
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

withManagedConn
    :: ResourceIO m
    => Manager
    -> ConnKey
    -> IO ConnInfo
    -> UseConn m a
    -> ResourceT m a
withManagedConn man key open f = mask $ \restore -> do
    mci <- liftBase $ takeSocket man key
    (ci, isManaged) <-
        case mci of
            Nothing -> do
                ci <- liftBase $ restore open
                return (ci, False)
            Just ci -> return (ci, True)
    ea <- try $ restore $ f ci
    case ea of
        Left e -> do
            liftBase $ connClose ci
            if isManaged
                then restore $ withManagedConn man key open f
                else throwIO (e :: SomeException)
        Right (WithConnResponse cr a) -> do
            case cr of
                Reuse -> liftBase $ putSocket man key ci
                DontReuse -> liftBase $ connClose ci
            return a

data WithConnResponse a = WithConnResponse !ConnReuse !a

data ConnReuse = Reuse | DontReuse

withConn :: ResourceIO m
         => Request m
         -> Manager
         -> UseConn m a
         -> ResourceT m a
withConn req m =
    go m connhost connport
  where
    h = host req
    (useProxy, connhost, connport) =
        case proxy req of
            Just p -> (True, S8.unpack (proxyHost p), proxyPort p)
            Nothing -> (False, S8.unpack h, port req)
    go =
        case (secure req, useProxy) of
            (False, _) -> withSocketConn
            (True, False) -> withSslConn $ checkCerts req h
            (True, True) -> withSslProxyConn (checkCerts req h) h (port req)
