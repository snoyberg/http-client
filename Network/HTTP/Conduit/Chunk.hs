{-# LANGUAGE FlexibleContexts #-}
module Network.HTTP.Conduit.Chunk
    ( chunkedConduit
    , chunkIt
    , chunkedTerminator
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

data CState = NeedHeader (S.ByteString -> A.Result Int64)
            | Isolate Int64
            | NeedNewline (S.ByteString -> A.Result ())
            | Complete

chunkedConduit :: C.ResourceThrow m
               => C.ConduitM S.ByteString m S.ByteString
chunkedConduit = C.conduitMState
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
                | otherwise -> push front (Isolate i) (x':xs)
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
            A.Done x' () -> push
                front
                (NeedHeader $ A.parse parseChunkHeader)
                (x':xs)
            A.Partial f' -> push front (NeedNewline f') xs
            A.Fail _ contexts msg -> lift $ C.resourceThrow $ ParseError contexts msg
    push front Complete leftover =
        return (Complete, C.ConduitResult (C.Done leftover) $ front [])
    close state bss = do
        (_, C.ConduitResult x rest) <- push id state bss
        let leftover = C.result [] id x
        return $ C.ConduitResult leftover rest

chunkedTerminator :: C.Resource m => C.ConduitM S.ByteString m S.ByteString
chunkedTerminator = error "FIXME chunkedTerminator" {- do
    len <- catchParser "Chunk header" iterChunkHeader
    k' <- sendCont k $ S8.pack $ showHex len "\r\n"
    if len == 0
        then return k'
        else do
            step <- CB.isolate len k'
            catchParser "End of chunk newline" iterNewline
            case step of
                Continue k'' -> do
                    k''' <- sendCont k'' "\r\n"
                    chunkedTerminator k'''
                _ -> return step
chunkedTerminator step = return step -}

chunkIt :: C.Resource m => C.ConduitM Blaze.Builder m Blaze.Builder
chunkIt = C.ConduitM $ return $ C.Conduit
    { C.conduitPush = \xs ->
        return $ C.ConduitResult C.Processing [go xs]
    , C.conduitClose = \xs ->
        return $ C.ConduitResult [] [go xs, chunkedTransferTerminator]
    }
  where
    go = chunkedTransferEncoding . mconcat
