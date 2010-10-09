module Network.HTTP.Enumerator.Zlib
    ( ungzip
    ) where

import Prelude hiding (head)
import Data.Enumerator
import qualified Data.ByteString as S
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Codec.Zlib

ungzip :: MonadIO m => Enumeratee S.ByteString S.ByteString m b
ungzip inner = do
    fzstr <- liftIO $ initInflate $ WindowBits 31
    ungzip' fzstr inner

ungzip' :: MonadIO m => ZStream -> Enumeratee S.ByteString S.ByteString m b
ungzip' fzstr (Continue k) = do
    x <- head
    case x of
        Nothing -> return $ Continue k
        Just bs -> do
            chunks <- liftIO $ withInflateInput fzstr bs $ go id
            step <- lift $ runIteratee $ k $ Chunks chunks
            ungzip' fzstr step
  where
    go front pop = do
        x <- pop
        case x of
            Nothing -> return $ front []
            Just y -> go (front . (:) y) pop
ungzip' _ step = return step
