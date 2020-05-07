{-# LANGUAGE ScopedTypeVariables #-}
-- | Support for making connections via the OpenSSL library.
module Network.HTTP.Client.OpenSSL
    ( opensslManagerSettings
    -- , defaultMakeContext
    , withOpenSSL
    ) where

import Network.HTTP.Client
import Network.HTTP.Client.Internal
import Control.Exception
import Network.Socket.ByteString (sendAll, recv)
import OpenSSL
import qualified Data.ByteString as S
import qualified Network.Socket as N
import qualified OpenSSL.Session       as SSL

-- | Note that it is the caller's responsibility to pass in an appropriate
-- context. Future versions of http-client-openssl will hopefully include a
-- sane, safe default.
opensslManagerSettings :: IO SSL.SSLContext -> ManagerSettings
opensslManagerSettings mkContext = defaultManagerSettings
    { managerTlsConnection = do
        ctx <- mkContext
        return $ \_ha host' port' -> do
            -- Copied/modified from openssl-streams
            let hints      = N.defaultHints
                                { N.addrFlags      = [N.AI_ADDRCONFIG, N.AI_NUMERICSERV]
                                , N.addrFamily     = N.AF_INET
                                , N.addrSocketType = N.Stream
                                }
            (addrInfo:_) <- N.getAddrInfo (Just hints) (Just host') (Just $ show port')

            let family     = N.addrFamily addrInfo
            let socketType = N.addrSocketType addrInfo
            let protocol   = N.addrProtocol addrInfo
            let address    = N.addrAddress addrInfo

            bracketOnError (N.socket family socketType protocol) (N.close)
                $ \sock -> do
                    N.connect sock address
                    ssl <- SSL.connection ctx sock
                    SSL.setTlsextHostName ssl host'
                    SSL.connect ssl
                    makeConnection
                        (SSL.read ssl 32752 `catch` \(_ :: SSL.ConnectionAbruptlyTerminated) -> return S.empty)
                        (SSL.write ssl)
                        (N.close sock)
    , managerTlsProxyConnection = do
        ctx <- mkContext
        return $ \connstr checkConn serverName _ha host' port' -> do
            -- Copied/modified from openssl-streams
            let hints      = N.defaultHints
                                { N.addrFlags      = [N.AI_ADDRCONFIG, N.AI_NUMERICSERV]
                                , N.addrFamily     = N.AF_INET
                                , N.addrSocketType = N.Stream
                                }
            (addrInfo:_) <- N.getAddrInfo (Just hints) (Just host') (Just $ show port')

            let family     = N.addrFamily addrInfo
            let socketType = N.addrSocketType addrInfo
            let protocol   = N.addrProtocol addrInfo
            let address    = N.addrAddress addrInfo

            bracketOnError (N.socket family socketType protocol) (N.close)
                $ \sock -> do
                    N.connect sock address
                    conn <- makeConnection
                            (recv sock 32752)
                            (sendAll sock)
                            (return ())
                    connectionWrite conn connstr
                    checkConn conn
                    ssl <- SSL.connection ctx sock
                    SSL.setTlsextHostName ssl serverName
                    SSL.connect ssl
                    makeConnection
                        (SSL.read ssl 32752 `catch` \(_ :: SSL.ConnectionAbruptlyTerminated) -> return S.empty)
                        (SSL.write ssl)
                        (N.close sock)

    , managerRetryableException = \se ->
        case () of
          ()
            | Just (_ :: SSL.ConnectionAbruptlyTerminated) <- fromException se -> True
            | otherwise -> managerRetryableException defaultManagerSettings se

    , managerWrapException = \req ->
        let
          wrap se
            | Just (_ :: IOException)                      <- fromException se = se'
            | Just (_ :: SSL.SomeSSLException)             <- fromException se = se'
            | Just (_ :: SSL.ConnectionAbruptlyTerminated) <- fromException se = se'
            | Just (_ :: SSL.ProtocolError)                <- fromException se = se'
            | otherwise                                                        = se
            where
              se' = toException (HttpExceptionRequest req (InternalException se))
        in
          handle (throwIO . wrap)
    }
