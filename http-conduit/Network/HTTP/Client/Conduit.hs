{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
-- | A new, experimental API to replace "Network.HTTP.Conduit".
--
-- For most users, "Network.HTTP.Simple" is probably a better choice. For more
-- information, see:
--
-- <https://github.com/commercialhaskell/jump/blob/master/doc/http-client.md>.
--
-- For more information on using this module, please be sure to read the
-- documentation in the "Network.HTTP.Client" module.
module Network.HTTP.Client.Conduit
    ( -- * Conduit-specific interface
      withResponse
    , responseOpen
    , responseClose
    , acquireResponse
      -- * Manager helpers
    , defaultManagerSettings
    , newManager
    , withManager
    , withManagerSettings
    , newManagerSettings
      -- * General HTTP client interface
    , module Network.HTTP.Client
    , httpLbs
    , httpNoBody
      -- * Lower-level conduit functions
    , requestBodySource
    , requestBodySourceChunked
    , bodyReaderSource
    ) where

import           Control.Monad                (unless)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Reader         (MonadReader (..), ReaderT (..))
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Data.Acquire                 (Acquire, mkAcquire, with)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as S
import qualified Data.ByteString.Lazy         as L
import           Data.Conduit                 (ConduitM, Producer, Source,
                                               await, yield, ($$+), ($$++))
import           Data.Int                     (Int64)
import           Data.IORef                   (newIORef, readIORef, writeIORef)
import           Network.HTTP.Client          hiding (closeManager,
                                               defaultManagerSettings, httpLbs,
                                               newManager, responseClose,
                                               responseOpen, withManager,
                                               withResponse, BodyReader, brRead, brConsume, httpNoBody)
import           Network.HTTP.Client          (HasHttpManager(..))
import qualified Network.HTTP.Client          as H
import           Network.HTTP.Client.TLS      (tlsManagerSettings)

-- | Conduit powered version of 'H.withResponse'. Differences are:
--
-- * Response body is represented as a @Producer@.
--
-- * Generalized to any instance of @MonadBaseControl@, not just @IO@.
--
-- * The @Manager@ is contained by a @MonadReader@ context.
--
-- Since 2.1.0
withResponse :: (MonadBaseControl IO m, MonadIO n, MonadReader env m, HasHttpManager env)
             => Request
             -> (Response (ConduitM i ByteString n ()) -> m a)
             -> m a
withResponse req f = do
    env <- ask
    with (acquireResponse req env) f

-- | An @Acquire@ for getting a @Response@.
--
-- Since 2.1.0
acquireResponse :: (MonadIO n, MonadReader env m, HasHttpManager env)
                => Request
                -> m (Acquire (Response (ConduitM i ByteString n ())))
acquireResponse req = do
    env <- ask
    let man = getHttpManager env
    return $ do
        res <- mkAcquire (H.responseOpen req man) H.responseClose
        return $ fmap bodyReaderSource res

-- | TLS-powered manager settings.
--
-- Since 2.1.0
defaultManagerSettings :: ManagerSettings
defaultManagerSettings = tlsManagerSettings

-- | Get a new manager using 'defaultManagerSettings'.
--
-- Since 2.1.0
newManager :: MonadIO m => m Manager
newManager = newManagerSettings defaultManagerSettings

-- | Get a new manager using the given settings.
--
-- Since 2.1.0
newManagerSettings :: MonadIO m => ManagerSettings -> m Manager
newManagerSettings = liftIO . H.newManager

-- | Get a new manager with 'defaultManagerSettings' and construct a @ReaderT@ containing it.
--
-- Since 2.1.0
withManager :: MonadIO m => (ReaderT Manager m a) -> m a
withManager = withManagerSettings defaultManagerSettings

-- | Get a new manager with the given settings and construct a @ReaderT@ containing it.
--
-- Since 2.1.0
withManagerSettings :: MonadIO m => ManagerSettings -> (ReaderT Manager m a) -> m a
withManagerSettings settings (ReaderT inner) = newManagerSettings settings >>= inner

-- | Conduit-powered version of 'H.responseOpen'.
--
-- See 'withResponse' for the differences with 'H.responseOpen'.
--
-- Since 2.1.0
responseOpen :: (MonadIO m, MonadIO n, MonadReader env m, HasHttpManager env)
             => Request
             -> m (Response (ConduitM i ByteString n ()))
responseOpen req = do
    env <- ask
    liftIO $ fmap bodyReaderSource `fmap` H.responseOpen req (getHttpManager env)

-- | Generalized version of 'H.responseClose'.
--
-- Since 2.1.0
responseClose :: MonadIO m => Response body -> m ()
responseClose = liftIO . H.responseClose

bodyReaderSource :: MonadIO m
                 => H.BodyReader
                 -> Producer m ByteString
bodyReaderSource br =
    loop
  where
    loop = do
        bs <- liftIO $ H.brRead br
        unless (S.null bs) $ do
            yield bs
            loop

requestBodySource :: Int64 -> Source IO ByteString -> RequestBody
requestBodySource size = RequestBodyStream size . srcToPopperIO

requestBodySourceChunked :: Source IO ByteString -> RequestBody
requestBodySourceChunked = RequestBodyStreamChunked . srcToPopperIO

srcToPopperIO :: Source IO ByteString -> GivesPopper ()
srcToPopperIO src f = do
    (rsrc0, ()) <- src $$+ return ()
    irsrc <- newIORef rsrc0
    let popper :: IO ByteString
        popper = do
            rsrc <- readIORef irsrc
            (rsrc', mres) <- rsrc $$++ await
            writeIORef irsrc rsrc'
            case mres of
                Nothing -> return S.empty
                Just bs
                    | S.null bs -> popper
                    | otherwise -> return bs
    f popper

-- | Same as 'H.httpLbs', except it uses the @Manager@ in the reader environment.
--
-- Since 2.1.1
httpLbs :: (MonadIO m, HasHttpManager env, MonadReader env m)
        => Request
        -> m (Response L.ByteString)
httpLbs req = do
    env <- ask
    let man = getHttpManager env
    liftIO $ H.httpLbs req man

-- | Same as 'H.httpNoBody', except it uses the @Manager@ in the reader environment.
--
-- This can be more convenient that using 'withManager' as it avoids the need
-- to specify the base monad for the response body.
--
-- Since 2.1.2
httpNoBody :: (MonadIO m, HasHttpManager env, MonadReader env m)
           => Request
           -> m (Response ())
httpNoBody req = do
    env <- ask
    let man = getHttpManager env
    liftIO $ H.httpNoBody req man
