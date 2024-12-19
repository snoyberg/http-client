{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.HTTP.Client.Manager
    ( ManagerSettings (..)
    , newManager
    , closeManager
    , withManager
    , getConn
    , defaultManagerSettings
    , rawConnectionModifySocket
    , rawConnectionModifySocketSize
    , proxyFromRequest
    , noProxy
    , useProxy
    , proxyEnvironment
    , proxyEnvironmentNamed
    , defaultProxy
    , dropProxyAuthSecure
    , useProxySecureWithoutConnect
    ) where

import qualified Data.ByteString.Char8 as S8

import Data.Text (Text)

import Control.Monad (unless)
import Control.Exception (throwIO, fromException, IOException, Exception (..), handle)

import qualified Network.Socket as NS

import Network.HTTP.Types (status200)
import Network.HTTP.Client.Types
import Network.HTTP.Client.Connection
import Network.HTTP.Client.Headers (parseStatusHeaders)
import Network.HTTP.Proxy
import Data.KeyedPool
import Data.Maybe (isJust)

-- | A value for the @managerRawConnection@ setting, but also allows you to
-- modify the underlying @Socket@ to set additional settings. For a motivating
-- use case, see: <https://github.com/snoyberg/http-client/issues/71>.
--
-- Since 0.3.8
rawConnectionModifySocket :: (NS.Socket -> IO ())
                          -> IO (Maybe NS.HostAddress -> String -> Int -> IO Connection)
rawConnectionModifySocket = return . openSocketConnection

-- | Same as @rawConnectionModifySocket@, but also takes in a chunk size.
--
-- @since 0.5.2
rawConnectionModifySocketSize :: (NS.Socket -> IO ())
                              -> IO (Int -> Maybe NS.HostAddress -> String -> Int -> IO Connection)
rawConnectionModifySocketSize = return . openSocketConnectionSize


-- | Default value for @ManagerSettings@.
--
-- Note that this value does /not/ have support for SSL/TLS. If you need to
-- make any https connections, please use the http-client-tls package, which
-- provides a @tlsManagerSettings@ value.
--
-- Since 0.1.0
defaultManagerSettings :: ManagerSettings
defaultManagerSettings = ManagerSettings
    { managerConnCount = 10
    , managerRawConnection = return $ openSocketConnection (const $ return ())
    , managerTlsConnection = return $ \_ _ _ -> throwHttp TlsNotSupported
    , managerTlsProxyConnection = return $ \_ _ _ _ _ _ -> throwHttp TlsNotSupported
    , managerResponseTimeout = ResponseTimeoutDefault
    , managerRetryableException = \e ->
        case fromException e of
            Just (_ :: IOException) -> True
            _ ->
                case fmap unHttpExceptionContentWrapper $ fromException e of
                    -- Note: Some servers will timeout connections by accepting
                    -- the incoming packets for the new request, but closing
                    -- the connection as soon as we try to read. To make sure
                    -- we open a new connection under these circumstances, we
                    -- check for the NoResponseDataReceived exception.
                    Just NoResponseDataReceived -> True
                    Just IncompleteHeaders -> True
                    _ -> False
    , managerWrapException = \_req ->
        let wrapper se =
                case fromException se of
                    Just (_ :: IOException) -> throwHttp $ InternalException se
                    Nothing -> throwIO se
         in handle wrapper
    , managerIdleConnectionCount = 512
    , managerModifyRequest = return
    , managerModifyResponse = return
    , managerProxyInsecure = defaultProxy
    , managerProxySecure = defaultProxy
    , managerMaxHeaderLength = Just $ MaxHeaderLength 4096
    , managerMaxNumberHeaders = Just $ MaxNumberHeaders 100
    }

-- | Create a 'Manager'. The @Manager@ will be shut down automatically via
-- garbage collection.
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
    NS.withSocketsDo $ return ()

    httpProxy <- runProxyOverride (managerProxyInsecure ms) False
    httpsProxy <- runProxyOverride (managerProxySecure ms) True

    createConnection <- mkCreateConnection ms

    keyedPool <- createKeyedPool
        createConnection
        connectionClose
        (managerConnCount ms)
        (managerIdleConnectionCount ms)
        (const (return ())) -- could allow something in ManagerSettings to handle exceptions more nicely

    let manager = Manager
            { mConns = keyedPool
            , mResponseTimeout = managerResponseTimeout ms
            , mRetryableException = managerRetryableException ms
            , mWrapException = managerWrapException ms
            , mModifyRequest = managerModifyRequest ms
            , mModifyResponse = managerModifyResponse ms
            , mSetProxy = \req ->
                if secure req
                    then httpsProxy req
                    else httpProxy req
            , mMaxHeaderLength = managerMaxHeaderLength ms
            , mMaxNumberHeaders = managerMaxNumberHeaders ms
            }
    return manager

    {- FIXME why isn't this being used anymore?
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
    -}

-- | Close all connections in a 'Manager'.
--
-- Note that this doesn't affect currently in-flight connections,
-- meaning you can safely use it without hurting any queries you may
-- have concurrently running.
--
-- Since 0.1.0
closeManager :: Manager -> IO ()
closeManager _ = return ()
{-# DEPRECATED closeManager "Manager will be closed for you automatically when no longer in use" #-}

-- | Create, use and close a 'Manager'.
--
-- Since 0.2.1
withManager :: ManagerSettings -> (Manager -> IO a) -> IO a
withManager settings f = newManager settings >>= f
{-# DEPRECATED withManager "Use newManager instead" #-}

-- | Drop the Proxy-Authorization header from the request if we're using a
-- secure proxy.
dropProxyAuthSecure :: Request -> Request
dropProxyAuthSecure req
    | secure req && useProxy' = req
        { requestHeaders = filter (\(k, _) -> k /= "Proxy-Authorization")
                                  (requestHeaders req)
        }
    | otherwise = req
  where
    useProxy' = isJust (proxy req)

getConn :: Request
        -> Manager
        -> IO (Managed Connection)
getConn req m
    -- Stop Mac OS X from getting high:
    -- https://github.com/snoyberg/http-client/issues/40#issuecomment-39117909
    | S8.null h = throwHttp $ InvalidDestinationHost h
    | otherwise = takeKeyedPool (mConns m) connkey
  where
    h = host req
    connkey = connKey req

connKey :: Request -> ConnKey
connKey req@Request { proxy = Nothing, secure = False } =
  CKRaw (hostAddress req) (host req) (port req)
connKey req@Request { proxy = Nothing, secure = True  } =
  CKSecure (hostAddress req) (host req) (port req)
connKey Request { proxy = Just p, secure = False } =
  CKRaw Nothing (proxyHost p) (proxyPort p)
connKey req@Request { proxy = Just p, secure = True,
                      proxySecureMode = ProxySecureWithConnect  } =
  CKProxy
    (proxyHost p)
    (proxyPort p)
    (lookup "Proxy-Authorization" (requestHeaders req))
    (host req)
    (port req)
connKey Request { proxy = Just p, secure = True,
                  proxySecureMode = ProxySecureWithoutConnect  } =
  CKRaw Nothing (proxyHost p) (proxyPort p)

mkCreateConnection :: ManagerSettings -> IO (ConnKey -> IO Connection)
mkCreateConnection ms = do
    rawConnection <- managerRawConnection ms
    tlsConnection <- managerTlsConnection ms
    tlsProxyConnection <- managerTlsProxyConnection ms

    return $ \ck -> wrapConnectExc $ case ck of
        CKRaw connaddr connhost connport ->
            rawConnection connaddr (S8.unpack connhost) connport
        CKSecure connaddr connhost connport ->
            tlsConnection connaddr (S8.unpack connhost) connport
        CKProxy connhost connport mProxyAuthHeader ultHost ultPort ->
            let proxyAuthorizationHeader = maybe
                    ""
                    (\h' -> S8.concat ["Proxy-Authorization: ", h', "\r\n"])
                    mProxyAuthHeader
                hostHeader = S8.concat ["Host: ", ultHost, ":", (S8.pack $ show ultPort), "\r\n"]
                connstr = S8.concat
                    [ "CONNECT "
                    , ultHost
                    , ":"
                    , S8.pack $ show ultPort
                    , " HTTP/1.1\r\n"
                    , proxyAuthorizationHeader
                    , hostHeader
                    , "\r\n"
                    ]
                parse conn = do
                    let mhl = managerMaxHeaderLength ms
                        mnh = managerMaxNumberHeaders ms
                    StatusHeaders status _ _ _ <- parseStatusHeaders mhl mnh conn Nothing (\_ -> return ()) Nothing
                    unless (status == status200) $
                        throwHttp $ ProxyConnectException ultHost ultPort status
                in tlsProxyConnection
                        connstr
                        parse
                        (S8.unpack ultHost)
                        Nothing -- we never have a HostAddress we can use
                        (S8.unpack connhost)
                        connport
  where
    wrapConnectExc = handle $ \e ->
        throwHttp $ ConnectionFailure (toException (e :: IOException))

-- | Get the proxy settings from the @Request@ itself.
--
-- Since 0.4.7
proxyFromRequest :: ProxyOverride
proxyFromRequest = ProxyOverride $ const $ return id

-- | Never connect using a proxy, regardless of the proxy value in the @Request@.
--
-- Since 0.4.7
noProxy :: ProxyOverride
noProxy = ProxyOverride $ const $ return $ \req -> req { proxy = Nothing }

-- | Use the given proxy settings, regardless of the proxy value in the @Request@.
--
-- Since 0.4.7
useProxy :: Proxy -> ProxyOverride
useProxy p = ProxyOverride $ const $ return $ \req -> req { proxy = Just p }

-- | Send secure requests to the proxy in plain text rather than using CONNECT,
-- regardless of the value in the @Request@.
--
-- @since 0.7.2
useProxySecureWithoutConnect :: Proxy -> ProxyOverride
useProxySecureWithoutConnect p = ProxyOverride $
  const $ return $ \req -> req { proxy = Just p,
                                 proxySecureMode = ProxySecureWithoutConnect }

-- | Get the proxy settings from the default environment variable (@http_proxy@
-- for insecure, @https_proxy@ for secure). If no variable is set, then fall
-- back to the given value. @Nothing@ is equivalent to 'noProxy', @Just@ is
-- equivalent to 'useProxy'.
--
-- Since 0.4.7
proxyEnvironment :: Maybe Proxy -- ^ fallback if no environment set
                 -> ProxyOverride
proxyEnvironment mp = ProxyOverride $ \secure' ->
    systemProxyHelper Nothing (httpProtocol secure') $ maybe EHNoProxy EHUseProxy mp

-- | Same as 'proxyEnvironment', but instead of default environment variable
-- names, allows you to set your own name.
--
-- Since 0.4.7
proxyEnvironmentNamed
    :: Text -- ^ environment variable name
    -> Maybe Proxy -- ^ fallback if no environment set
    -> ProxyOverride
proxyEnvironmentNamed name mp = ProxyOverride $ \secure' ->
    systemProxyHelper (Just name) (httpProtocol secure') $ maybe EHNoProxy EHUseProxy mp

-- | The default proxy settings for a manager. In particular: if the @http_proxy@ (or @https_proxy@) environment variable is set, use it. Otherwise, use the values in the @Request@.
--
-- Since 0.4.7
defaultProxy :: ProxyOverride
defaultProxy = ProxyOverride $ \secure' ->
    systemProxyHelper Nothing (httpProtocol secure') EHFromRequest
