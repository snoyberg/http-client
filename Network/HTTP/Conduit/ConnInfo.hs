{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module Network.HTTP.Conduit.ConnInfo
    ( ConnInfo
    , connClose
    , connSink
    , connSource
    , sslClientConn
    , socketConn
    , CertificateRejectReason(..)
    , CertificateUsage(..)
    , getSocket
#if DEBUG
    , printOpenSockets
    , requireAllSocketsClosed
    , clearSocketsList
#endif
    ) where

import Control.Exception (IOException, bracketOnError, throwIO)
import qualified Control.Exception as E
import System.IO (Handle, hClose)

import Control.Monad.IO.Class (liftIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Network (PortID(..))
import Network.Socket (Socket, sClose, AddrInfo)
import Network.Socket.ByteString (recv, sendAll)
import qualified Network.Socket as NS
import Network.Socks5 (socksConnectWith, SocksConf)

import Network.TLS
import Network.TLS.Extra (ciphersuite_all)

import Data.Certificate.X509 (X509)

import Crypto.Random.AESCtr (makeSystem)

import Data.Conduit

#if DEBUG
import qualified Data.IntMap as IntMap
import qualified Data.IORef as I
import System.IO.Unsafe (unsafePerformIO)
#endif

data ConnInfo = ConnInfo
    { connRead :: IO ByteString
    , connWrite :: ByteString -> IO ()
    , connClose :: IO ()
    }

connSink :: MonadResource m => ConnInfo -> Sink ByteString m ()
connSink ConnInfo { connWrite = write } =
    self
  where
    self = await >>= maybe (return ()) (\x -> liftIO (write x) >> self)

connSource :: MonadResource m => ConnInfo -> Source m ByteString
connSource ConnInfo { connRead = read' } =
    self
  where
    self = do
        bs <- liftIO read'
        if S.null bs
            then return ()
            else yield bs >> self

#if DEBUG
allOpenSockets :: I.IORef (Int, IntMap.IntMap String)
allOpenSockets = unsafePerformIO $ I.newIORef (0, IntMap.empty)

addSocket :: String -> IO Int
addSocket desc = I.atomicModifyIORef allOpenSockets $ \(next, m) ->
    ((next + 1, IntMap.insert next desc m), next)

removeSocket :: Int -> IO ()
removeSocket i = I.atomicModifyIORef allOpenSockets $ \(next, m) ->
    ((next, IntMap.delete i m), ())

printOpenSockets :: IO ()
printOpenSockets = do
    (_, m) <- I.readIORef allOpenSockets
    putStrLn "\n\nOpen sockets:"
    if IntMap.null m
        then putStrLn "** No open sockets!"
        else mapM_ putStrLn $ IntMap.elems m

requireAllSocketsClosed :: IO ()
requireAllSocketsClosed = do
    (_, m) <- I.readIORef allOpenSockets
    if IntMap.null m
        then return ()
        else error $ unlines
            $ "requireAllSocketsClosed: there are open sockets"
            : IntMap.elems m

clearSocketsList :: IO ()
clearSocketsList = I.writeIORef allOpenSockets (0, IntMap.empty)
#endif

socketConn :: String -> Socket -> IO ConnInfo
socketConn _desc sock = do
#if DEBUG
    i <- addSocket _desc
#endif
    return ConnInfo
        { connRead  = recv sock 4096
        , connWrite = sendAll sock
        , connClose = do
#if DEBUG
            removeSocket i
#endif
            sClose sock
        }

sslClientConn :: String -> String -> ([X509] -> IO CertificateUsage) -> [(X509, Maybe PrivateKey)] -> Handle -> IO ConnInfo
sslClientConn _desc host onCerts clientCerts h = do
#if DEBUG
    i <- addSocket _desc
#endif
    let setCParams cparams = cparams
            { onCertificateRequest = const (return clientCerts)
            , clientUseServerName = Just host
            }
        tcp = updateClientParams setCParams $ defaultParamsClient
            { pConnectVersion = TLS10
            , pAllowedVersions = [ TLS10, TLS11, TLS12 ]
            , pCiphers = ciphersuite_all
            , onCertificatesRecv = onCerts
            , pCertificates = clientCerts
            }
    gen <- makeSystem
    istate <- contextNewOnHandle h tcp gen
    handshake istate
    return ConnInfo
        { connRead = recvD istate
        , connWrite = sendData istate . L.fromChunks . (:[])
        , connClose = do
#if DEBUG
            removeSocket i
#endif
            bye istate
            hClose h
        }
  where
    recvD istate = do
        x <- recvData istate
        if S.null x
            then recvD istate
            else return x

getSocket :: Maybe NS.HostAddress -> String -> Int -> Maybe SocksConf -> IO NS.Socket
getSocket _ host' port' (Just socksConf) = do
    socksConnectWith socksConf host' (PortNumber $ fromIntegral port')
getSocket hostAddress' host' port' Nothing = do
    let hints = NS.defaultHints {
                          NS.addrFlags = [NS.AI_ADDRCONFIG]
                        , NS.addrSocketType = NS.Stream
                        }
    addrs <- case hostAddress' of
        Nothing ->
            NS.getAddrInfo (Just hints) (Just host') (Just $ show port')
        Just ha ->
            return
                [NS.AddrInfo
                 { NS.addrFlags = []
                 , NS.addrFamily = NS.AF_INET
                 , NS.addrSocketType = NS.Stream
                 , NS.addrProtocol = 6 -- tcp
                 , NS.addrAddress = NS.SockAddrInet (toEnum port') ha
                 , NS.addrCanonName = Nothing
                 }]

    firstSuccessful addrs $ \addr ->
        bracketOnError
            (NS.socket (NS.addrFamily addr) (NS.addrSocketType addr)
                       (NS.addrProtocol addr))
            (NS.sClose)
            (\sock -> do
                NS.setSocketOption sock NS.NoDelay 1
                NS.connect sock (NS.addrAddress addr)
                return sock)

firstSuccessful :: [AddrInfo] -> (AddrInfo -> IO a) -> IO a
firstSuccessful []     _  = error "getAddrInfo returned empty list"
firstSuccessful (a:as) cb =
    cb a `E.catch` \(e :: IOException) ->
        case as of
            [] -> throwIO e
            _  -> firstSuccessful as cb
