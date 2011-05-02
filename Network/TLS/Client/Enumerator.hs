{-# LANGUAGE ScopedTypeVariables #-}
module Network.TLS.Client.Enumerator
    ( ConnInfo
    , connClose
    , connIter
    , connEnum
    , sslClientConn
    , socketConn
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import System.IO (Handle, hClose)
import Network.Socket (Socket, sClose)
import Network.Socket.ByteString (recv, sendAll)
import Network.TLS
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Enumerator
    ( Iteratee (..), Enumerator, Step (..), Stream (..), continue, returnI
    )
import Data.Certificate.X509 (X509)
import Network.TLS.Extra (ciphersuite_all)
import Crypto.Random (SystemRandom, newGenIO)

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
        liftIO $ write bss
        continue go

connEnum :: MonadIO m => ConnInfo -> Enumerator ByteString m b
connEnum ConnInfo { connRead = read' } =
    go
  where
    go (Continue k) = do
        bs <- liftIO read'
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

sslClientConn :: ([X509] -> IO Bool) -> Handle -> IO ConnInfo
sslClientConn onCerts h = do
    let tcp = defaultParams
            { pConnectVersion = TLS10
            , pAllowedVersions = [ TLS10, TLS11 ]
            , pCiphers = ciphersuite_all
            , onCertificatesRecv = onCerts
            }
    gen :: SystemRandom <- newGenIO
    istate <- liftIO $ client tcp gen h
    liftIO $ handshake istate
    return ConnInfo
        { connRead = recvD istate
        , connWrite = liftIO . sendData istate . L.fromChunks
        , connClose = liftIO $ bye istate >> hClose h
        }
  where
    recvD istate = do
        x <- liftIO $ recvData istate
        if L.null x
            then recvD istate
            else return $ L.toChunks x
