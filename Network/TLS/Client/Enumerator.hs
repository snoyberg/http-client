{-# LANGUAGE ScopedTypeVariables #-}
module Network.TLS.Client.Enumerator
    ( ConnInfo
    , connClose
    , connIter
    , connEnum
    , sslClientConn
    , socketConn
    , TLSCertificateRejectReason(..)
    , TLSCertificateUsage(..)
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import System.IO (Handle, hClose)
import Network.Socket (Socket, sClose)
import Network.Socket.ByteString (recv, sendAll)
import Network.TLS
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Data.Enumerator
    ( Iteratee (..), Enumerator, Step (..), Stream (..), continue, returnI
    , tryIO
    )
import Data.Certificate.X509 (X509)
import Network.TLS.Extra (ciphersuite_all)
import Crypto.Random.AESCtr (makeSystem)

data ConnInfo = ConnInfo
    { connRead :: IO [ByteString]
    , connWrite :: [ByteString] -> IO ()
    , connClose :: IO ()
    }

connIter :: MonadIO m => ConnInfo -> Iteratee ByteString m ()
connIter ConnInfo { connWrite = write } =
    continue go
  where
    go EOF = return ()
    go (Chunks bss) = do
        tryIO $ write bss
        continue go

connEnum :: MonadIO m => ConnInfo -> Enumerator ByteString m b
connEnum ConnInfo { connRead = read' } =
    go
  where
    go (Continue k) = do
        bs <- tryIO read'
        if all S.null bs
            then continue k
            else do
                step <- lift $ runIteratee $ k $ Chunks bs
                go step
    go step = returnI step

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
