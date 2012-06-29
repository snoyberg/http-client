{-# LANGUAGE FlexibleContexts #-}
module Network.HTTP.Conduit.Chunk
    ( chunkedConduit
    , chunkIt
    ) where

import Numeric (showHex)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

import Blaze.ByteString.Builder.HTTP
import qualified Blaze.ByteString.Builder as Blaze

import qualified Data.Attoparsec.ByteString as A

import Data.Conduit hiding (Source, Sink, Conduit)
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Attoparsec (ParseError (ParseError), Position (..))

import Network.HTTP.Conduit.Parser
import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)

chunkedConduit :: MonadThrow m
               => Bool -- ^ send the headers as well, necessary for a proxy
               -> Pipe S.ByteString S.ByteString S.ByteString u m ()
chunkedConduit sendHeaders =
    await >>= maybe (return ()) (needHeader $ A.parse parseChunkHeader)
  where
    needHeader f x =
        case f x of
            A.Done x' i
                | i == 0 -> unless (S.null x') (leftover x') >> complete
                | otherwise -> do
                    let header = S8.pack $ showHex i "\r\n"
                    when sendHeaders $ yield header
                    unless (S.null x') $ leftover x'
                    CB.isolate i
            A.Partial f' -> await >>= maybe (return ()) (needHeader f')
            A.Fail _ contexts msg -> lift $ monadThrow $ ParseError contexts msg $ Position 0 0
    complete = when sendHeaders $ yield $ S8.pack "0\r\n"

chunkIt :: Monad m => Pipe l Blaze.Builder Blaze.Builder r m r
chunkIt =
    awaitE >>= either
        (\u -> yield chunkedTransferTerminator >> return u)
        (\x -> yield (chunkedTransferEncoding x) >> chunkIt)
