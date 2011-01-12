module Network.TLS.Client.Enumerator
    ( clientEnumSimple
    , clientEnum
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Enumerator as E
import Data.Enumerator (($$), joinI)
import qualified Control.Monad.IO.Class as Trans
import Control.Monad.Trans.Class (lift)
import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.Enumerator (builderToByteString)

import Network.TLS.Client
import Network.TLS.SRandom
import Network.TLS.Struct
import Network.TLS.Cipher

import Control.Monad.State (runStateT)

import Data.IORef
import System.IO (Handle)

type IState = IORef TLSStateClient

newIState :: TLSClientParams -> SRandomGen -> IO IState
newIState params rng = do
    ((), tsc) <- runTLSClient (return ()) params rng
    newIORef tsc

clientEnumSimple
    :: Trans.MonadIO m
    => Handle
    -> E.Enumerator Builder m () -- ^ request
    -> E.Enumerator B.ByteString m a -- ^ response
clientEnumSimple h req step = do
    let clientstate = TLSClientParams
            { cpConnectVersion = TLS10
            , cpAllowedVersions = [ TLS10, TLS11 ]
            , cpSession = Nothing
            , cpCiphers = ciphers
            , cpCertificate = Nothing
            , cpCallbacks = TLSClientCallbacks
                { cbCertificates = Nothing
                }
            }
    esrand <- Trans.liftIO makeSRandomGen
    let srand = either (error . show) id esrand
    clientEnum clientstate srand h req step
  where
    ciphers =
        [ cipher_AES128_SHA1
        , cipher_AES256_SHA1
        , cipher_RC4_128_MD5
        , cipher_RC4_128_SHA1
        ]

clientEnum :: Trans.MonadIO m
           => TLSClientParams -> SRandomGen -> Handle
           -> E.Enumerator Builder m ()
           -> E.Enumerator B.ByteString m a
clientEnum tcp srg h req step0 = do
    istate <- Trans.liftIO $ newIState tcp srg
    tlsHelper istate $ connect h
    lift $ E.run_ $ req $$ joinI $ builderToByteString $$ iter istate
    res <- enum istate step0
    tlsHelper istate $ close h
    return res
  where
    iter :: Trans.MonadIO m => IState -> E.Iteratee B.ByteString m ()
    iter istate =
        E.continue go
      where
        go E.EOF = return ()
        go (E.Chunks xs) = do
            tlsHelper istate $ sendData h $ L.fromChunks xs
            E.continue go
    enum :: Trans.MonadIO m => IState -> E.Enumerator B.ByteString m a
    enum istate (E.Continue k) = E.Iteratee $ do
        lbs <- tlsHelper istate $ recvData h
        let chunks = E.Chunks $ L.toChunks lbs
        step <- E.runIteratee $ k chunks
        E.runIteratee $ enum istate step
    enum _ step = E.returnI step

tlsHelper :: Trans.MonadIO m => IState -> TLSClient IO a -> m a
tlsHelper istate (TLSClient client) = do
    state <- Trans.liftIO $ readIORef istate
    (ret, state') <- Trans.liftIO $ runStateT client state
    Trans.liftIO $ writeIORef istate state'
    return ret
