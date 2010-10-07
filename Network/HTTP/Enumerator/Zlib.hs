{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
module Network.HTTP.Enumerator.Zlib
    ( ungzip
    ) where

import Prelude hiding (head)
import Data.Enumerator
import qualified Data.ByteString as S
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (allocaBytes)
import Data.ByteString.Unsafe
import Control.Monad (unless)

ungzip :: MonadIO m => Enumeratee S.ByteString S.ByteString m b
ungzip inner = do
    zstr <- liftIO c_create_z_stream
    fzstr <- liftIO $ newForeignPtr c_free_z_stream zstr
    ungzip' fzstr inner

ungzip' :: MonadIO m
        => ForeignPtr ZStreamStruct
        -> Enumeratee S.ByteString S.ByteString m b
ungzip' fzstr (Continue k) = do
    x <- head
    case x of
        Nothing -> return $ Continue k
        Just bs -> do
            chunks <- liftIO $ unsafeUseAsCStringLen bs $ \(cstr, len) ->
                withForeignPtr fzstr $ \zstr -> do
                    c_set_avail_in zstr cstr $ fromIntegral len
                    drain zstr
            lift $ runIteratee $ k $ Chunks chunks
ungzip' _ step = return step

drain :: ZStream -> IO [S.ByteString]
drain zstr = allocaBytes defaultChunkSize $ \buff -> do
    x <- go buff id
    return $ x []
  where
    go buff front = do
        c_set_avail_out zstr buff $ fromIntegral defaultChunkSize
        res <- c_call_inflate_noflush zstr
        unless (res == 0) $ error "Error in underlying stream"
        avail <- c_get_avail_out zstr
        let size = defaultChunkSize - fromIntegral avail
        if size == 0
            then return front
            else do
                bs <- unsafePackCStringLen (buff, size)
                go buff (front . (:) bs)

foreign import ccall unsafe "create_z_stream"
    c_create_z_stream :: IO ZStream

foreign import ccall unsafe "&free_z_stream"
    c_free_z_stream :: FunPtr (ZStream -> IO ())

foreign import ccall unsafe "set_avail_in"
    c_set_avail_in :: ZStream -> Ptr CChar -> CUInt -> IO ()

foreign import ccall unsafe "set_avail_out"
    c_set_avail_out :: ZStream -> Ptr CChar -> CUInt -> IO ()

foreign import ccall unsafe "call_inflate_noflush"
    c_call_inflate_noflush :: ZStream -> IO CInt

foreign import ccall unsafe "get_avail_out"
    c_get_avail_out :: ZStream -> IO CUInt

data ZStreamStruct
type ZStream = Ptr ZStreamStruct
