{-# LANGUAGE FlexibleContexts #-}
module Network.HTTP.Client where

import Network.HTTP.Client.Manager
import Network.HTTP.Client.Types
import Network.HTTP.Client.Body
import Network.HTTP.Client.Request
import Network.HTTP.Client.Response
import Control.Exception
import qualified Data.ByteString.Lazy as L

withResponse :: Request
             -> Manager
             -> (Response BodyReader -> IO a)
             -> IO a
withResponse req man f = bracket (responseOpen req man) responseClose f

responseOpen :: Request -> Manager -> IO (Response BodyReader)
responseOpen req man = do
    -- FIXME need to copy a bunch of logic from Network.HTTP.Conduit
    -- (ConnRelease, Connection, ManagedConn)
    (release, conn, mconn) <- getConn req man
    requestBuilder req conn
    let mtimeout = Nothing -- FIXME
    getResponse release mtimeout req conn

httpLbs :: Request -> Manager -> IO (Response L.ByteString)
httpLbs req man = withResponse req man $ \res -> do
    bss <- brConsume $ responseBody res
    return res { responseBody = L.fromChunks bss }
