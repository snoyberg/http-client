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

import Network.TLS
import System.IO (Handle)

clientEnumSimple
    :: Trans.MonadIO m
    => Handle
    -> E.Enumerator Builder m () -- ^ request
    -> E.Enumerator B.ByteString m a -- ^ response
clientEnumSimple h req step = do
    let params = defaultParams
            { pConnectVersion = TLS10
            , pAllowedVersions = [ TLS10, TLS11 ]
            , pCiphers = ciphers
            }
    esrand <- Trans.liftIO makeSRandomGen
    let srand = either (error . show) id esrand
    clientEnum params srand h req step
  where
    ciphers =
        [ cipher_AES128_SHA1
        , cipher_AES256_SHA1
        , cipher_RC4_128_MD5
        , cipher_RC4_128_SHA1
        ]

clientEnum :: Trans.MonadIO m
           => TLSParams -> SRandomGen -> Handle
           -> E.Enumerator Builder m ()
           -> E.Enumerator B.ByteString m a
clientEnum tcp srg h req step0 = do
    istate <- Trans.liftIO $ client tcp srg h
    Trans.liftIO $ handshake istate
    lift $ E.run_ $ req $$ joinI $ builderToByteString $$ iter istate
    res <- enum istate step0
    Trans.liftIO $ bye istate
    return res
  where
    iter :: Trans.MonadIO m => TLSCtx -> E.Iteratee B.ByteString m ()
    iter istate =
        E.continue go
      where
        go E.EOF = return ()
        go (E.Chunks xs) = do
            Trans.liftIO $ sendData istate $ L.fromChunks xs
            E.continue go
    enum :: Trans.MonadIO m => TLSCtx -> E.Enumerator B.ByteString m a
    enum istate (E.Continue k) = E.Iteratee $ do
        lbs <- Trans.liftIO $ recvData istate
        let chunks = E.Chunks $ L.toChunks lbs
        step <- E.runIteratee $ k chunks
        E.runIteratee $ enum istate step
    enum _ step = E.returnI step
