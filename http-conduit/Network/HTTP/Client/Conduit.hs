{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
-- | A new, experimental API to replace "Network.HTTP.Conduit".
module Network.HTTP.Client.Conduit
    ( -- * Conduit-specific interface
      withResponse
    , responseOpen
    , responseClose
    , responseResource
      -- * Manager helpers
    , defaultManagerSettings
    , newManager
    , withManager
    , withManagerSettings
    , newManagerSettings
    , HasHttpManager (..)
      -- * General HTTP client interface
    , module Network.HTTP.Client
      -- * Lower-level conduit functions
    , requestBodySource
    , requestBodySourceChunked
    , bodyReaderSource
    ) where

import           Control.Monad                (unless)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Reader         (MonadReader (..), ReaderT (..))
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Control.Monad.Trans.Resource (Resource, mkResource, with)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as S
import           Data.Conduit                 (ConduitM, Producer, Source,
                                               await, yield, ($$+), ($$++))
import           Data.Int                     (Int64)
import           Data.IORef                   (newIORef, readIORef, writeIORef)
import           Network.HTTP.Client          hiding (closeManager,
                                               defaultManagerSettings, httpLbs,
                                               newManager, responseClose,
                                               responseOpen, withManager,
                                               withResponse, BodyReader, brRead, brConsume)
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
    with (responseResource req env) f

-- | A @Resource@ for getting a @Response@.
--
-- Since 2.1.0
responseResource :: (MonadIO n, MonadReader env m, HasHttpManager env)
                 => Request
                 -> m (Resource (Response (ConduitM i ByteString n ())))
responseResource req = do
    env <- ask
    let man = getHttpManager env
    return $ do
        res <- mkResource (H.responseOpen req man) H.responseClose
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

class HasHttpManager a where
    getHttpManager :: a -> Manager
instance HasHttpManager Manager where
    getHttpManager = id

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
