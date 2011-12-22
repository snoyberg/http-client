{-# LANGUAGE FlexibleContexts #-}
module Network.HTTP.Conduit.Chunk
    ( chunkedConduit
    , chunkIt
    ) where

import qualified Data.Conduit as C
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString as A
import Network.HTTP.Conduit.Parser
import Data.Int (Int64)
import Data.Monoid (mconcat)
import qualified Blaze.ByteString.Builder as Blaze
import Blaze.ByteString.Builder.HTTP
import Control.Exception (assert)
import Data.Conduit.Attoparsec (ParseError (ParseError))
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Char8 as S8
import Numeric (showHex)

data CState = NeedHeader (S.ByteString -> A.Result Int64)
            | Isolate Int64
            | NeedNewline (S.ByteString -> A.Result ())
            | Complete

chunkedConduit :: C.ResourceThrow m
               => Bool -- ^ send the headers as well, necessary for a proxy
               -> C.Conduit S.ByteString m S.ByteString
chunkedConduit sendHeaders = C.conduitState
    (NeedHeader $ A.parse parseChunkHeader)
    (push id)
    close
  where
    push front state [] =
        return (state, C.ConduitResult C.Processing $ front [])
    push front (NeedHeader f) (x:xs) =
        case f x of
            A.Done x' i
                | i == 0 -> push front Complete (x':xs)
                | otherwise -> do
                    let header = S8.pack $ showHex i "\r\n"
                    let addHeader = if sendHeaders then (header:) else id
                    push (front . addHeader) (Isolate i) (x':xs)
            A.Partial f' -> push front (NeedHeader f') xs
            A.Fail _ contexts msg -> lift $ C.resourceThrow $ ParseError contexts msg
    push front (Isolate i) xs = do
        let lbs = L.fromChunks xs
            (a, b) = L.splitAt i lbs
            i' = i - L.length a
        if i' == 0
            then push
                    (front . (L.toChunks a ++))
                    (NeedNewline $ A.parse newline)
                    (L.toChunks b)
            else assert (L.null b) $ return
                ( Isolate i'
                , C.ConduitResult
                    C.Processing
                    (front $ L.toChunks a)
                )
    push front (NeedNewline f) (x:xs) =
        case f x of
            A.Done x' () -> do
                let header = S8.pack "\r\n"
                let addHeader = if sendHeaders then (header:) else id
                push
                    (front . addHeader)
                    (NeedHeader $ A.parse parseChunkHeader)
                    (x':xs)
            A.Partial f' -> push front (NeedNewline f') xs
            A.Fail _ contexts msg -> lift $ C.resourceThrow $ ParseError contexts msg
    push front Complete leftover = do
        let end = if sendHeaders then [S8.pack "0\r\n"] else []
        return (Complete, C.ConduitResult (C.Done leftover) $ front end)
    close state bss = do
        (_, C.ConduitResult x rest) <- push id state bss
        let leftover = C.result [] id x
        return $ C.ConduitResult leftover rest

chunkIt :: C.Resource m => C.Conduit Blaze.Builder m Blaze.Builder
chunkIt = C.Conduit $ return $ C.PreparedConduit
    { C.conduitPush = \xs ->
        return $ C.ConduitResult C.Processing [go xs]
    , C.conduitClose = \xs ->
        return $ C.ConduitResult [] [go xs, chunkedTransferTerminator]
    }
  where
    go = chunkedTransferEncoding . mconcat
