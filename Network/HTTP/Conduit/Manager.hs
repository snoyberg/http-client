{-# LANGUAGE FlexibleContexts #-}
module Network.HTTP.Conduit.Manager
    ( Manager
    , ConnKey (..)
    , newManager
    , withConn
    ) where

import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import qualified Data.Map as Map
import Network.HTTP.Conduit.ConnInfo
import qualified Data.IORef as I
import Control.Applicative ((<$>))
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Base (liftBase)
import Control.Exception.Lifted (mask, try, throwIO, SomeException)
import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Conduit.Request

-- | Keeps track of open connections for keep-alive.
newtype Manager = Manager
    { mConns :: I.IORef (Map.Map ConnKey ConnInfo)
    }

-- | ConnKey consists of a hostname, a port and a Bool specifying whether to
--   use keepalive.
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

-- | Create a new 'Manager' with no open connection.
newManager :: MonadBaseControl IO m => ResourceT m Manager
newManager = snd <$> with
    (Manager <$> I.newIORef Map.empty)
    closeManager

-- | Close all connections in a 'Manager'. Afterwards, the 'Manager' can be
-- reused if desired.
closeManager :: Manager -> IO ()
closeManager (Manager i) = do
    m <- I.atomicModifyIORef i $ \x -> (Map.empty, x)
    mapM_ connClose $ Map.elems m

withSocketConn
    :: MonadBaseControl IO m
    => Manager
    -> String
    -> Int
    -> (ConnInfo -> ResourceT m (Bool, a))
    -> ResourceT m a
withSocketConn man host' port' =
    withManagedConn man (ConnKey (T.pack host') port' False) $
        fmap socketConn $ getSocket host' port'

withSslConn, withSslProxyConn :: a
withSslConn = error "withSslConn"
withSslProxyConn = error "withSslProxyConn"

withManagedConn
    :: MonadBaseControl IO m
    => Manager
    -> ConnKey
    -> IO ConnInfo
    -> (ConnInfo -> ResourceT m (Bool, a))
    -> ResourceT m a
withManagedConn man key open f = mask $ \restore -> do
    mci <- restore $ liftBase $ takeSocket man key
    (ci, isManaged) <-
        case mci of
            Nothing -> do
                ci <- restore $ liftBase open
                return (ci, False)
            Just ci -> return (ci, True)
    ea <- restore $ try $ f ci
    case ea of
        Left e -> do
            liftBase $ connClose ci
            if isManaged
                then restore $ withManagedConn man key open f
                else throwIO (e :: SomeException)
        Right (toPut, a) -> do
            if toPut
                then restore $ liftBase $ putSocket man key ci
                else restore $ liftBase $ connClose ci
            return a

{- FIXME
    {-
    -> Enumerator Blaze.Builder m ()
    -> Step S.ByteString m (Bool, a) -- ^ Bool indicates if the connection should go back in the manager
    -> Iteratee S.ByteString m a
    -}
withManagedConn man key open req step = do
    mci <- liftBase $ takeInsecureSocket man key
    (ci, isManaged) <-
        case mci of
            Nothing -> do
                ci <- liftBase open
                return (ci, False)
            Just ci -> return (ci, True)
    catchError
        (do
            (toPut, a) <- withCI ci req step
            liftBase $ if toPut
                then putInsecureSocket man key ci
                else connClose ci
            return a)
        (\se -> liftBase (connClose ci) >>
                if isManaged
                    then withManagedConn man key open req step
                    else throwError se)
-}

{-
withSslConn :: MonadBaseControl IO m
            => ([X509] -> IO TLSCertificateUsage)
            -> Manager
            -> String -- ^ host
            -> Int -- ^ port
            -> Enumerator Blaze.Builder m () -- ^ request
            -> Step S.ByteString m (Bool, a) -- ^ response
            -> Iteratee S.ByteString m a -- ^ response
withSslConn checkCert man host' port' =
    withManagedConn man (host', port', True) $
        (connectTo host' (PortNumber $ fromIntegral port') >>= sslClientConn checkCert)

withSslProxyConn :: MonadBaseControl IO m
            => ([X509] -> IO TLSCertificateUsage)
            -> S8.ByteString -- ^ Target host
            -> Int -- ^ Target port
            -> Manager
            -> String -- ^ Proxy host
            -> Int -- ^ Proxy port
            -> Enumerator Blaze.Builder m () -- ^ request
            -> Step S.ByteString m (Bool, a) -- ^ response
            -> Iteratee S.ByteString m a -- ^ response
withSslProxyConn checkCert thost tport man phost pport =
    withManagedConn man (phost, pport, True) $
        doConnect >>= sslClientConn checkCert
  where
    doConnect = do
        h <- connectTo phost (PortNumber $ fromIntegral pport)
        S8.hPutStr h $ toByteString connectRequest
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

withCI :: MonadBaseControl IO m => ConnInfo -> Enumerator Blaze.Builder m () -> Enumerator S.ByteString m a
withCI ci req step0 = do
    lift $ run_ $ req $$ joinI $ error "builderToByteString" $$ connIter ci
    a <- connEnum ci step0
    -- FIXME liftBase $ hClose handle
    return a
-}

withConn :: MonadBaseControl IO m
         => Request m
         -> Manager
         -> (ConnInfo -> ResourceT m (Bool, a))
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
