-- | Frontend support for using http-client with conduit. Intended for use with
-- higher-level libraries like http-conduit.
module Network.HTTP.Client.Conduit
    ( requestBodySource
    , requestBodySourceChunked
    , bodyReaderSource
    , http
    ) where

import Data.Conduit
import qualified Data.Conduit.Internal as CI
import Control.Monad.Trans.Resource
import Network.HTTP.Client
import Network.HTTP.Client.Body
import Network.HTTP.Client.Types
import Network.HTTP.Client.Connection
import Network.HTTP.Client.Manager
import Data.Int (Int64)
import qualified Data.ByteString as S
import Data.ByteString (ByteString)
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)

bodyReaderSource br =
    loop
  where
    loop = do
        bs <- liftIO $ brRead br
        unless (S.null bs) $ do
            yield bs
            loop

http :: MonadResource m
     => Request
     -> Manager
     -> m (Response (ResumableSource m ByteString))
http req man = do
    (key, res) <- allocate (responseOpen req man) responseClose
    let rsrc = CI.ResumableSource
            (bodyReaderSource $ responseBody res)
            (release key)
    return res { responseBody = rsrc }

requestBodySource :: Int64 -> Source (ResourceT IO) ByteString -> RequestBody
requestBodySource size = RequestBodyStream size . srcToPopper

requestBodySourceChunked :: Source (ResourceT IO) ByteString -> RequestBody
requestBodySourceChunked = RequestBodyStreamChunked . srcToPopper

srcToPopper :: Source (ResourceT IO) ByteString -> GivesPopper ()
srcToPopper src f = runResourceT $ do
    (rsrc0, ()) <- src $$+ return ()
    irsrc <- liftIO $ newIORef rsrc0
    is <- getInternalState
    let popper :: IO ByteString
        popper = do
            rsrc <- readIORef irsrc
            (rsrc', mres) <- runInternalState (rsrc $$++ await) is
            writeIORef irsrc rsrc'
            case mres of
                Nothing -> return S.empty
                Just bs
                    | S.null bs -> popper
                    | otherwise -> return bs
    liftIO $ f popper
