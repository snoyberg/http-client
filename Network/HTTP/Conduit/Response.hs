{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Conduit.Response
    ( Response (..)
    , getResponse
    , lbsResponse
    ) where

import Control.Arrow (first)
import Data.Typeable (Typeable)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L

import qualified Data.CaseInsensitive as CI

import Control.Monad.Trans.Resource (ResourceT, ResourceIO)
import qualified Data.Conduit as C
import qualified Data.Conduit.Zlib as CZ
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

import qualified Network.HTTP.Types as W

import Network.HTTP.Conduit.Manager
import Network.HTTP.Conduit.Request
import Network.HTTP.Conduit.Util
import Network.HTTP.Conduit.Parser
import Network.HTTP.Conduit.Chunk

-- | A simple representation of the HTTP response created by 'lbsConsumer'.
data Response body = Response
    { statusCode :: W.Status
    , responseHeaders :: W.ResponseHeaders
    , responseBody :: body
    }
    deriving (Show, Eq, Typeable)

-- | Convert a 'Response' that has a 'C.BufferedSource' body to one with a lazy
-- 'L.ByteString' body.
lbsResponse :: C.Resource m
            => ResourceT m (Response (C.BufferedSource m S8.ByteString))
            -> ResourceT m (Response L.ByteString)
lbsResponse mres = do
    res <- mres
    bss <- responseBody res C.$$ CL.consume
    return res
        { responseBody = L.fromChunks bss
        }

getResponse :: ResourceIO m
            => ConnRelease m
            -> Request m
            -> C.BufferedSource m S8.ByteString
            -> ResourceT m (Response (C.BufferedSource m S8.ByteString))
getResponse connRelease req@(Request {..}) bsrc = do
    ((_, sc, sm), hs) <- bsrc C.$$ sinkHeaders
    let s = W.Status sc sm
    let hs' = map (first CI.mk) hs
    let mcl = lookup "content-length" hs' >>= readDec . S8.unpack

    -- RFC 2616 section 4.4_1 defines responses that must not include a body
    body <- if hasNoBody method sc || mcl == Just 0
        then do
            -- FIXME clean up socket
            C.bufferSource $ CL.sourceList []
        else do
            bsrc' <-
                if ("transfer-encoding", "chunked") `elem` hs'
                    then C.bufferSource $ bsrc C.$= chunkedConduit rawBody
                    else
                        case mcl of
                            Just len -> C.bufferSource $ bsrc C.$= CB.isolate len
                            Nothing  -> return bsrc
            if needsGunzip req hs'
                then C.bufferSource $ bsrc' C.$= CZ.ungzip
                else return bsrc'

    -- should we put this connection back into the connection manager?
    let toPut = Just "close" /= lookup "connection" hs'
    let cleanup = connRelease $ if toPut then Reuse else DontReuse

    return $ Response s hs' $ addCleanup cleanup body

-- | Add some cleanup code to the given 'C.BufferedSource'. General purpose
-- function, could be included in conduit itself.
addCleanup :: C.ResourceIO m
           => ResourceT m ()
           -> C.BufferedSource m a
           -> C.BufferedSource m a
addCleanup cleanup bsrc = C.BufferedSource
    { C.bsourcePull = do
        res <- C.bsourcePull bsrc
        case res of
            C.Closed -> cleanup
            C.Open _ -> return ()
        return res
    , C.bsourceUnpull = C.bsourceUnpull bsrc
    , C.bsourceClose = do
        C.bsourceClose bsrc
        cleanup
    }
