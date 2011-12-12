{-# LANGUAGE CPP #-}
module Network.HTTP.Conduit.Util
    ( hGetSome
    ) where

#if 1
-- FIXME MIN_VERSION_base(4,3,0)
import Data.ByteString (hGetSome)
#else
import GHC.IO.Handle.Types
import System.IO                (hWaitForInput, hIsEOF)
import System.IO.Error          (mkIOError, illegalOperationErrorType)

-- | Like 'hGet', except that a shorter 'ByteString' may be returned
-- if there are not enough bytes immediately available to satisfy the
-- whole request.  'hGetSome' only blocks if there is no data
-- available, and EOF has not yet been reached.
--
hGetSome :: Handle -> Int -> IO S.ByteString
hGetSome hh i
    | i >  0    = let
                   loop = do
                     s <- S.hGetNonBlocking hh i
                     if not (S.null s)
                        then return s
                        else do eof <- hIsEOF hh
                                if eof then return s
                                       else hWaitForInput hh (-1) >> loop
                                         -- for this to work correctly, the
                                         -- Handle should be in binary mode
                                         -- (see GHC ticket #3808)
                  in loop
    | i == 0    = return S.empty
    | otherwise = illegalBufferSize hh "hGetSome" i

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn sz =
    ioError (mkIOError illegalOperationErrorType msg (Just handle) Nothing)
    --TODO: System.IO uses InvalidArgument here, but it's not exported :-(
    where
      msg = fn ++ ": illegal ByteString size " ++ showsPrec 9 sz []
#endif
