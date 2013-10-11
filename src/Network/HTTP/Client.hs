{-# LANGUAGE FlexibleContexts #-}
module Network.HTTP.Client where

import Network.HTTP.Client.Manager
import Network.HTTP.Client.Types
import Network.HTTP.Client.Body
import Network.HTTP.Client.Request
import Network.HTTP.Client.Response
import Control.Exception
import Control.Monad.Trans.Control
import qualified Data.ByteString.Lazy as L

withResponse :: MonadBaseControl IO m
             => Request
             -> Manager
             -> (Response BodyReader -> m a)
             -> m a
withResponse req man f = control $ \run -> bracket
    (responseOpen req man)
    responseClose
    (run . f)

responseOpen :: Request -> Manager -> IO (Response BodyReader)
responseOpen req man = do
    -- (ConnRelease, Connection, ManagedConn)
    (release, conn, mconn) <- getConn req man
    requestBuilder req conn
    let mtimeout = Nothing -- FIXME
    getResponse release mtimeout req conn

httpLbs :: Request -> Manager -> IO (Response L.ByteString)
httpLbs req man = bracket (responseOpen req man) responseClose $ \res -> do
    bss <- brConsume $ responseBody res
    return res { responseBody = L.fromChunks bss }