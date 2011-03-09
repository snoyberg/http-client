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

sslClientConn :: Handle -> IO ConnInfo
sslClientConn h = do
    let tcp = defaultParams
            { pConnectVersion = TLS10
            , pAllowedVersions = [ TLS10, TLS11 ]
            , pCiphers = ciphers
            }
    esrand <- liftIO makeSRandomGen
    let srg = either (error . show) id esrand
    istate <- liftIO $ client tcp srg h
    liftIO $ handshake istate
    return ConnInfo
        { connRead = liftIO $ fmap L.toChunks $ recvData istate
        , connWrite = liftIO . sendData istate . L.fromChunks
        , connClose = liftIO $ bye istate >> hClose h
        }
  where
    ciphers =
        [ cipher_AES128_SHA1
        , cipher_AES256_SHA1
        , cipher_RC4_128_MD5
        , cipher_RC4_128_SHA1
        ]
