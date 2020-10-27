{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
-- | Support for making connections via the OpenSSL library.
module Network.HTTP.Client.OpenSSL
    ( withOpenSSL
    , newOpenSSLManager
    , opensslManagerSettings
    , defaultMakeContext
    , OpenSSLSettings(..)
    , defaultOpenSSLSettings
    ) where

import Network.HTTP.Client
import Network.HTTP.Client.Internal
import Control.Exception
import Control.Monad.IO.Class
import Network.Socket.ByteString (sendAll, recv)
import OpenSSL
import qualified Data.ByteString as S
import qualified Network.Socket as N
import qualified OpenSSL.Session as SSL
import qualified OpenSSL.X509.SystemStore as SSL (contextLoadSystemCerts)

-- | Create a new 'Manager' using 'opensslManagerSettings' and 'defaultMakeContext'
-- with 'defaultOpenSSLSettings'.
newOpenSSLManager :: MonadIO m => m Manager
newOpenSSLManager = liftIO $ do
  -- sharing an SSL context between threads (without modifying it) is safe:
  -- https://github.com/openssl/openssl/issues/2165
  ctx <- defaultMakeContext defaultOpenSSLSettings
  newManager $ opensslManagerSettings (pure ctx)

-- | Note that it is the caller's responsibility to pass in an appropriate context.
opensslManagerSettings :: IO SSL.SSLContext -> ManagerSettings
opensslManagerSettings mkContext = defaultManagerSettings
    { managerTlsConnection = do
        ctx <- mkContext
        return $ \_ha host' port' ->
            withSocket host' port' $ \sock ->
                makeSSLConnection ctx sock host'
    , managerTlsProxyConnection = do
        ctx <- mkContext
        return $ \connstr checkConn serverName _ha host' port' ->
            withSocket host' port' $ \sock -> do
                conn <- makeConnection
                        (recv sock 32752)
                        (sendAll sock)
                        (return ())
                connectionWrite conn connstr
                checkConn conn
                makeSSLConnection ctx sock serverName

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
  where
    withSocket host port use = do
        -- Copied/modified from openssl-streams
        let hints      = N.defaultHints
                            { N.addrFlags      = [N.AI_ADDRCONFIG, N.AI_NUMERICSERV]
                            , N.addrFamily     = N.AF_INET
                            , N.addrSocketType = N.Stream
                            }

        (addrInfo:_) <- N.getAddrInfo (Just hints) (Just host) (Just $ show port)

        let family     = N.addrFamily addrInfo
        let socketType = N.addrSocketType addrInfo
        let protocol   = N.addrProtocol addrInfo
        let address    = N.addrAddress addrInfo

        bracketOnError (N.socket family socketType protocol) (N.close) $
            \sock -> N.connect sock address *> use sock

    makeSSLConnection ctx sock host = do
        ssl <- SSL.connection ctx sock
        SSL.setTlsextHostName ssl host
        SSL.enableHostnameValidation ssl host
        SSL.connect ssl
        makeConnection
           (SSL.read ssl 32752 `catch` \(_ :: SSL.ConnectionAbruptlyTerminated) -> return S.empty)
           (SSL.write ssl)
           (N.close sock)

defaultMakeContext :: OpenSSLSettings -> IO SSL.SSLContext
defaultMakeContext OpenSSLSettings{..} = do
    ctx <- SSL.context
    SSL.contextSetVerificationMode ctx osslSettingsVerifyMode
    SSL.contextSetCiphers ctx osslSettingsCiphers
    mapM_ (SSL.contextAddOption ctx) osslSettingsOptions
    osslSettingsLoadCerts ctx
    return ctx

data OpenSSLSettings = OpenSSLSettings
    { osslSettingsOptions :: [SSL.SSLOption]
    , osslSettingsVerifyMode :: SSL.VerificationMode
    , osslSettingsCiphers :: String
    , osslSettingsLoadCerts :: SSL.SSLContext -> IO ()
    }

-- | Default OpenSSL settings. In particular:
--
--  * SSLv2 and SSLv3 are disabled
--  * Hostname validation
--  * @DEFAULT@ cipher list
--  * Certificates loaded from OS-specific store
--
-- Note that these settings might change in the future.
defaultOpenSSLSettings :: OpenSSLSettings
defaultOpenSSLSettings = OpenSSLSettings
    { osslSettingsOptions =
        [ SSL.SSL_OP_ALL -- enable bug workarounds
        , SSL.SSL_OP_NO_SSLv2
        , SSL.SSL_OP_NO_SSLv3
        ]
    , osslSettingsVerifyMode = SSL.VerifyPeer
        -- vpFailIfNoPeerCert and vpClientOnce are only relevant for servers
        { SSL.vpFailIfNoPeerCert = False
        , SSL.vpClientOnce = False
        , SSL.vpCallback = Nothing
        }
    , osslSettingsCiphers = "DEFAULT"
    , osslSettingsLoadCerts = SSL.contextLoadSystemCerts
    }
