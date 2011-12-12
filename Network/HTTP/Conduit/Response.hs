{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Conduit.Response
    ( lbsConsumer
    , Response (..)
    , ResponseConsumer
    , getResponse
    ) where

import Control.Monad.Trans.Resource (ResourceT)
import qualified Data.Conduit as C
import qualified Data.Conduit.Zlib as CZ
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Network.HTTP.Types as W
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Data.Typeable (Typeable)
import Network.HTTP.Conduit.Manager
import Network.HTTP.Conduit.Request
import Network.HTTP.Conduit.Util
import Network.HTTP.Conduit.Parser
import Network.HTTP.Conduit.Chunk
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString.Char8 as S8
import Control.Arrow (first)
import qualified Data.CaseInsensitive as CI

-- | Convert the HTTP response into a 'Response' value.
--
-- Even though a 'Response' contains a lazy bytestring, this function does
-- /not/ utilize lazy I/O, and therefore the entire response body will live in
-- memory. If you want constant memory usage, you'll need to write your own
-- iteratee and use 'http' or 'httpRedirect' directly.
lbsConsumer :: C.MonadBaseControl IO m => ResponseConsumer m Response
lbsConsumer (W.Status sc _) hs bsrc = do
    lbs <- fmap L.fromChunks $ bsrc C.$$ CL.consume
    return $ Response sc hs lbs

-- | A simple representation of the HTTP response created by 'lbsConsumer'.
data Response = Response
    { statusCode :: Int
    , responseHeaders :: W.ResponseHeaders
    , responseBody :: L.ByteString
    }
    deriving (Show, Read, Eq, Typeable)

type ResponseConsumer m a
    = W.Status
   -> W.ResponseHeaders
   -> C.BSource m S.ByteString
   -> ResourceT m a

getResponse :: MonadBaseControl IO m
            => Request m
            -> ResponseConsumer m a
            -> C.BSource m S8.ByteString
            -> ResourceT m (WithConnResponse a)
getResponse req@(Request {..}) bodyStep bsrc = do
    ((_, sc, sm), hs) <- bsrc C.$$ sinkHeaders
    let s = W.Status sc sm
    let hs' = map (first CI.mk) hs
    let mcl = lookup "content-length" hs'
    body' <-
        case (rawBody, ("transfer-encoding", "chunked") `elem` hs') of
            (False, True) -> do
                c <- C.bconduitM chunkedConduit
                return (C.$= c)
            (True , True) -> error "chunk2" -- (chunkedTerminator =$)
            (_    , False) ->
                case mcl >>= readDec . S8.unpack of
                    Just len -> do
                        i <- C.bconduitM $ CB.isolate len
                        return (C.$= i)
                    Nothing  -> return id
    decompressor <-
        if needsGunzip req hs'
            then do
                ug <- C.bconduitM CZ.ungzip
                return (C.$= ug)
            else return id
    -- RFC 2616 section 4.4_1 defines responses that must not include a body
    res <-
        if hasNoBody method sc
            then do
                bsrcNull <- C.bsourceM $ CL.fromList []
                bodyStep s hs' bsrcNull
            else do
                x <- bodyStep s hs' $ decompressor $ body' bsrc
                bsrc C.$$ flushStream
                return x

    -- should we put this connection back into the connection manager?
    let toPut = Just "close" /= lookup "connection" hs'
    return $ WithConnResponse (if toPut then Reuse else DontReuse) res
