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

import Control.Exception (SomeException, throwIO, try)
import System.IO (Handle, hClose)

import Control.Monad.Base (MonadBase, liftBase)

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Network.Socket (Socket, sClose)
import Network.Socket.ByteString (recv, sendAll)
import qualified Network.Socket as NS

import Network.TLS
import Network.TLS.Extra (ciphersuite_all)

import Data.Certificate.X509 (X509)

import Crypto.Random.AESCtr (makeSystem)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL


data ConnInfo = ConnInfo
    { connRead :: IO ByteString
    , connWrite :: ByteString -> IO ()
    , connClose :: IO ()
    }

connSink :: C.ResourceIO m => ConnInfo -> C.Sink ByteString m ()
connSink ConnInfo { connWrite = write } = C.Sink $ return $ C.SinkData
    { C.sinkPush = \bss -> liftBase (write bss]) >> return C.Processing
    , C.sinkClose = return ()
    }

connSource :: C.ResourceIO m => ConnInfo -> C.Source m ByteString
connSource ConnInfo { connRead = read' } = C.Source $ return $ C.PreparedSource
    { C.sourcePull = do
        bs <- liftBase read'
        if S.null bs
            then return C.Closed
            else return $ C.Open bs
    , C.sourceClose = return ()
    }

socketConn :: Socket -> ConnInfo
socketConn sock = ConnInfo
    { connRead  = recv sock 4096
    , connWrite = sendAll sock
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
        , connWrite = sendData istate . L.fromChunks . (:[])
        , connClose = bye istate >> hClose h
        }
  where
    recvD istate = do
        x <- recvData istate
        if L.null x
            then recvD istate
            else return $ S.concat $ L.toChunks x
            -- Although a 'concat' seems like a bad idea, at
            -- least on tls-0.8.4 it's guaranteed to always
            -- return a lazy bytestring with a single chunk.

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
