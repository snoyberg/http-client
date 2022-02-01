{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Support for making connections via the OpenSSL library.
module Network.HTTP.Client.OpenSSL
    ( -- * Settings
      newOpenSSLManager
    , opensslManagerSettings
    , defaultMakeContext
    , OpenSSLSettings(..)
    , defaultOpenSSLSettings
      -- * Re-exports from OpenSSL
    , OpenSSL.withOpenSSL
    ) where

import Network.HTTP.Client
import Network.HTTP.Client.Internal
import Control.Exception
import Control.Monad.IO.Class
import Network.Socket.ByteString (sendAll, recv)
import qualified Data.ByteString as S
import qualified Network.Socket as N
import qualified OpenSSL
import qualified OpenSSL.Session as SSL
import qualified OpenSSL.X509.SystemStore as SSL (contextLoadSystemCerts)
import Foreign.Storable (sizeOf)

-- | Create a new 'Manager' using 'opensslManagerSettings' and
-- 'defaultOpenSSLSettings'. The 'SSL.SSLContext' is created once
-- and shared between connections.
newOpenSSLManager :: MonadIO m => m Manager
newOpenSSLManager = liftIO $ do
  -- sharing an SSL context between threads (without modifying it) is safe:
  -- https://github.com/openssl/openssl/issues/2165
  ctx <- defaultMakeContext defaultOpenSSLSettings
  newManager $ opensslManagerSettings (pure ctx)

-- | Create a TLS-enabled 'ManagerSettings' using "OpenSSL" that obtains its
-- 'SSL.SSLContext' from the given action.
--
-- Note that 'mkContext' is run whenever a connection is created.
opensslManagerSettings :: IO SSL.SSLContext -> ManagerSettings
opensslManagerSettings mkContext = defaultManagerSettings
    { managerTlsConnection = do
        ctx <- mkContext
        return $ \ha' host' port' ->
            withSocket (const $ return ()) ha' host' port' $ \sock ->
                makeSSLConnection ctx sock host'
    , managerTlsProxyConnection = do
        ctx <- mkContext
        return $ \connstr checkConn serverName _ha host' port' ->
            withSocket (const $ return ()) Nothing host' port' $ \sock -> do
                conn <- makeConnection
                        (recv sock bufSize)
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
    makeSSLConnection ctx sock host = do
        ssl <- SSL.connection ctx sock
        SSL.setTlsextHostName ssl (strippedHostName host)
        SSL.enableHostnameValidation ssl (strippedHostName host)
        SSL.connect ssl
        makeConnection
           (SSL.read ssl bufSize `catch` \(_ :: SSL.ConnectionAbruptlyTerminated) -> return S.empty)
           -- Handling SSL.ConnectionAbruptlyTerminated as a stream end
           -- (some sites terminate SSL connection right after returning the data).
           (SSL.write ssl)
           (N.close sock)

    -- same as Data.ByteString.Lazy.Internal.defaultChunkSize
    bufSize :: Int
    bufSize = 32 * 1024 - overhead
        where overhead = 2 * sizeOf (undefined :: Int)

-- | Returns an action that sets up a 'SSL.SSLContext' with the given
-- 'OpenSSLSettings'.
defaultMakeContext :: OpenSSLSettings -> IO SSL.SSLContext
defaultMakeContext OpenSSLSettings{..} = do
    ctx <- SSL.context
    SSL.contextSetVerificationMode ctx osslSettingsVerifyMode
    SSL.contextSetCiphers ctx osslSettingsCiphers
    mapM_ (SSL.contextAddOption ctx) osslSettingsOptions
    osslSettingsLoadCerts ctx
    return ctx

-- | SSL settings as used by 'defaultMakeContext' to set up an 'SSL.SSLContext'.
data OpenSSLSettings = OpenSSLSettings
    { osslSettingsOptions :: [SSL.SSLOption]
      -- ^ SSL options, as passed to 'SSL.contextAddOption'
    , osslSettingsVerifyMode :: SSL.VerificationMode
      -- ^ SSL verification mode, as passed to 'SSL.contextSetVerificationMode'
    , osslSettingsCiphers :: String
      -- ^ SSL cipher list, as passed to 'SSL.contextSetCiphers'
    , osslSettingsLoadCerts :: SSL.SSLContext -> IO ()
      -- ^ An action to load certificates into the context, typically using
      -- 'SSL.contextSetCAFile' or 'SSL.contextSetCaDirectory'.
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
