{-# LANGUAGE FlexibleContexts #-}
module Network.HTTP.Conduit.Chunk
    ( chunkedConduit
    ) where

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString as A
import Network.HTTP.Conduit.Parser
import Data.Int (Int64)

data CState = NeedHeader (S.ByteString -> A.Result Int64)
            | Isolate Int64
            | NeedNewline (S.ByteString -> A.Result ())
            | Complete

chunkedConduit :: C.Resource m
               => C.ConduitM S.ByteString m S.ByteString
chunkedConduit = C.conduitMState
    (NeedHeader $ A.parse parseChunkHeader)
    (push id)
    close
  where
    push front state [] = return (state, C.ConduitResult C.Processing $ front [])
    push front (NeedHeader f) (x:xs) =
        case f x of
            A.Done x' i
                | i == 0 -> push front Complete (x':xs)
                | otherwise -> push front (Isolate i) (x':xs)
    push front (Isolate i) xs = do
        let lbs = L.fromChunks xs
            (a, b) = L.splitAt i lbs
            i' = i - L.length a
        if i' == 0
            then push (front . (L.toChunks a ++)) (NeedNewline $ A.parse newline) (L.toChunks b)
            else return (Isolate i', C.ConduitResult (C.Done $ L.toChunks b) $ front $ L.toChunks a)
    push front (NeedNewline f) (x:xs) =
        case f x of
            A.Done x' () -> push front (NeedHeader $ A.parse parseChunkHeader) (x':xs)
    push front Complete leftover = return (Complete, C.ConduitResult (C.Done leftover) $ front [])
    close = error "close 22"
  {-
    len <- {-catchParser "Chunk header"-} iterChunkHeader
    if len == 0
        then C.SinkM $ return $ C.Sink
                { C.sinkPush = \input -> return $ C.SinkResult input Nothing
                , C.sinkClose = return $ C.sinkResult [] (Just ())
                }
        else do
            CB.isolate len k
            catchParser "End of chunk newline" iterNewline
            chunkedEnumeratee k'
            -}

{- FIXME
chunkedTerminator :: MonadBaseControl IO m => Enumeratee S.ByteString S.ByteString m a
chunkedTerminator (Continue k) = do
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
chunkedTerminator step = return step

sendCont :: Monad m
         => (Stream S8.ByteString -> Iteratee S8.ByteString m a)
         -> S8.ByteString
         -> Iteratee S8.ByteString m (Step S8.ByteString m a)
sendCont k bs = lift $ runIteratee $ k $ Chunks [bs]

chunkIt :: Monad m => Enumeratee Blaze.Builder Blaze.Builder m a
chunkIt = checkDone $ continue . step
  where
    step k EOF = k (Chunks [chunkedTransferTerminator]) >>== return
    step k (Chunks []) = continue $ step k
    step k (Chunks xs) = k (Chunks [chunkedTransferEncoding $ mconcat xs])
                         >>== chunkIt
-}

chunkIt :: a
chunkIt = error "561 chunkIt"
