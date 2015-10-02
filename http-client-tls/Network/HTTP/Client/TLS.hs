{-# LANGUAGE ScopedTypeVariables #-}
-- | Support for making connections via the connection package and, in turn,
-- the tls package suite.
module Network.HTTP.Client.TLS
    ( tlsManagerSettings
    , mkManagerSettings
    , getTlsConnection
    ) where

import Data.Default.Class
import Network.HTTP.Client
import Network.HTTP.Client.Internal
import Control.Exception
import qualified Network.Connection as NC
import Network.Socket (HostAddress)
import qualified Network.TLS as TLS
import qualified Data.ByteString as S

mkManagerSettings :: NC.TLSSettings
                  -> Maybe NC.SockSettings
                  -> ManagerSettings
mkManagerSettings tls sock = defaultManagerSettings
    { managerTlsConnection = getTlsConnection (Just tls) sock
    , managerTlsProxyConnection = getTlsProxyConnection tls sock
    , managerRawConnection =
        case sock of
            Nothing -> managerRawConnection defaultManagerSettings
            Just _ -> getTlsConnection Nothing sock
    , managerRetryableException = \e ->
        case () of
            ()
                | ((fromException e)::(Maybe TLS.TLSError))==Just TLS.Error_EOF -> True
                | otherwise -> case fromException e of
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
                maybe
                    (maybe
                        se
                        (toException . TlsException)
                        (fromException se)
                    )
                    (toException . InternalIOException)
                    (fromException se)
                    
         in handle $ throwIO . wrapper
    }

tlsManagerSettings :: ManagerSettings
tlsManagerSettings = mkManagerSettings def Nothing

getTlsConnection :: Maybe NC.TLSSettings
                 -> Maybe NC.SockSettings
                 -> IO (Maybe HostAddress -> String -> Int -> IO Connection)
getTlsConnection tls sock = do
    context <- NC.initConnectionContext
    return $ \_ha host port -> do
        conn <- NC.connectTo context NC.ConnectionParams
            { NC.connectionHostname = host
            , NC.connectionPort = fromIntegral port
            , NC.connectionUseSecure = tls
            , NC.connectionUseSocks = sock
            }
        convertConnection conn

getTlsProxyConnection
    :: NC.TLSSettings
    -> Maybe NC.SockSettings
    -> IO (S.ByteString -> (Connection -> IO ()) -> String -> Maybe HostAddress -> String -> Int -> IO Connection)
getTlsProxyConnection tls sock = do
    context <- NC.initConnectionContext
    return $ \connstr checkConn serverName _ha host port -> do
        --error $ show (connstr, host, port)
        conn <- NC.connectTo context NC.ConnectionParams
            { NC.connectionHostname = serverName
            , NC.connectionPort = fromIntegral port
            , NC.connectionUseSecure = Nothing
            , NC.connectionUseSocks =
                case sock of
                    Just _ -> error "Cannot use SOCKS and TLS proxying together"
                    Nothing -> Just $ NC.OtherProxy host $ fromIntegral port
            }

        NC.connectionPut conn connstr
        conn' <- convertConnection conn

        checkConn conn'

        NC.connectionSetSecure context conn tls

        return conn'

convertConnection :: NC.Connection -> IO Connection
convertConnection conn = makeConnection
    (NC.connectionGetChunk conn)
    (NC.connectionPut conn)
    -- Closing an SSL connection gracefully involves writing/reading
    -- on the socket.  But when this is called the socket might be
    -- already closed, and we get a @ResourceVanished@.
    (NC.connectionClose conn `Control.Exception.catch` \(_ :: IOException) -> return ())
