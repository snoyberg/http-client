{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.HTTP.Conduit.ConnInfo
    ( ConnInfo
    , connClose
    , connSink
    , connSource
    , sslClientConn
    , socketConn
    , TLSCertificateRejectReason(..)
    , TLSCertificateUsage(..)
    , getSocket
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import System.IO (Handle, hClose)
import Network.Socket (Socket, sClose)
import Network.Socket.ByteString (recv, sendAll)
import Network.TLS
import Data.Certificate.X509 (X509)
import Network.TLS.Extra (ciphersuite_all)
import Crypto.Random.AESCtr (makeSystem)
import qualified Data.Conduit as C
import Control.Monad.Base (MonadBase, liftBase)
import Control.Exception (SomeException, throwIO, try)
import qualified Network.Socket as NS

data ConnInfo = ConnInfo
    { connRead :: IO [ByteString]
    , connWrite :: [ByteString] -> IO ()
    , connClose :: IO ()
    }

connSink :: MonadBase IO m => ConnInfo -> C.SinkM ByteString m ()
connSink ConnInfo { connWrite = write } = C.SinkM $ return $ C.SinkData
    { C.sinkPush = \bss -> liftBase (write bss) >> return (C.SinkResult [] Nothing)
    , C.sinkClose = \bss -> liftBase (write bss) >> return (C.SinkResult [] ())
    }

connSource :: MonadBase IO m => ConnInfo -> C.SourceM m ByteString
connSource ConnInfo { connRead = read' } = C.sourceM
    (return ())
    return
    (const $ do
        bs <- liftBase read'
        if all S.null bs
            then return $ C.EOF []
            else return $ C.Chunks bs)

socketConn :: Socket -> ConnInfo
socketConn sock = ConnInfo
    { connRead = fmap return $ recv sock 4096
    , connWrite = mapM_ (sendAll sock)
    , connClose = sClose sock
    }

sslClientConn :: ([X509] -> IO TLSCertificateUsage) -> Handle -> IO ConnInfo
sslClientConn onCerts h = do
    let tcp = defaultParams
            { pConnectVersion = TLS10
            , pAllowedVersions = [ TLS10, TLS11 ]
            , pCiphers = ciphersuite_all
            , onCertificatesRecv = onCerts
            }
    gen <- makeSystem
    istate <- client tcp gen h
    _ <- handshake istate
    return ConnInfo
        { connRead = recvD istate
        , connWrite = sendData istate . L.fromChunks
        , connClose = bye istate >> hClose h
        }
  where
    recvD istate = do
        x <- recvData istate
        if L.null x
            then recvD istate
            else return $ L.toChunks x

getSocket :: String -> Int -> IO NS.Socket
getSocket host' port' = do
    let hints = NS.defaultHints {
                          NS.addrFlags = [NS.AI_ADDRCONFIG]
                        , NS.addrSocketType = NS.Stream
                        }
    (addr:_) <- NS.getAddrInfo (Just hints) (Just host') (Just $ show port')
    sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr)
                      (NS.addrProtocol addr)
    ee <- try' $ NS.connect sock (NS.addrAddress addr)
    case ee of
        Left e -> NS.sClose sock >> throwIO e
        Right () -> return sock
  where
    try' :: IO a -> IO (Either SomeException a)
    try' = try
