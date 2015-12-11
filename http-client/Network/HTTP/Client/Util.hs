{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Network.HTTP.Client.Util
    ( hGetSome
    , (<>)
    , readDec
    , hasNoBody
    , fromStrict
    , timeout
    ) where

import Data.Monoid (Monoid, mappend)

import qualified Data.ByteString.Char8 as S8

#ifndef MIN_VERSION_bytestring
#define MIN_VERSION_bytestring(x,y,z) 1
#endif

#if MIN_VERSION_bytestring(0,10,0)
import Data.ByteString.Lazy (fromStrict)
#else
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
#endif

import qualified Data.Text as T
import qualified Data.Text.Read
import System.Timeout (timeout)

import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (mask_, Exception, throwTo, try, finally, SomeException, assert)
import Control.Monad (join, when, void)
import Control.Concurrent (myThreadId, threadDelay, forkIO)
import Data.IORef
import Data.Function (fix)
import Data.Typeable (Typeable)

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif
#if MIN_VERSION_base(4,3,0)
import Data.ByteString (hGetSome)
#else
import GHC.IO.Handle.Types
import System.IO                (hWaitForInput, hIsEOF)
import System.IO.Error          (mkIOError, illegalOperationErrorType)

-- | Like 'hGet', except that a shorter 'ByteString' may be returned
-- if there are not enough bytes immediately available to satisfy the
-- whole request.  'hGetSome' only blocks if there is no data
-- available, and EOF has not yet been reached.
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

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

readDec :: Integral i => String -> Maybe i
readDec s =
    case Data.Text.Read.decimal $ T.pack s of
        Right (i, t)
            | T.null t -> Just i
        _ -> Nothing

hasNoBody :: S8.ByteString -- ^ request method
          -> Int -- ^ status code
          -> Bool
hasNoBody "HEAD" _ = True
hasNoBody _ 204 = True
hasNoBody _ 304 = True
hasNoBody _ i = 100 <= i && i < 200

#if !MIN_VERSION_bytestring(0,10,0)
{-# INLINE fromStrict #-}
fromStrict :: S.ByteString -> L.ByteString
fromStrict x = L.fromChunks [x]
#endif

-- Disabling the custom timeout code for now. See: https://github.com/snoyberg/http-client/issues/116
{-
data TimeoutHandler = TimeoutHandler {-# UNPACK #-} !TimeSpec (IO ())
newtype TimeoutManager = TimeoutManager (IORef ([TimeoutHandler], Bool))

newTimeoutManager :: IO TimeoutManager
newTimeoutManager = fmap TimeoutManager $ newIORef ([], False)

timeoutManager :: TimeoutManager
timeoutManager = unsafePerformIO newTimeoutManager
{-# NOINLINE timeoutManager #-}

spawnWorker :: TimeoutManager -> IO ()
spawnWorker (TimeoutManager ref) = void $ forkIO $ fix $ \loop -> do
    threadDelay 500000
    join $ atomicModifyIORef ref $ \(hs, isCleaning) -> assert (not isCleaning) $
        if null hs
            then (([], False), return ())
            else (([], True), ) $ do
                now <- getTime Monotonic
                front <- go now id hs
                atomicModifyIORef ref $ \(hs', isCleaning') ->
                    assert isCleaning' $ ((front hs', False), ())
                loop
  where
    go now =
        go'
      where
        go' front [] = return front
        go' front (h@(TimeoutHandler time action):hs)
            | time < now = do
                _ :: Either SomeException () <- try action
                go' front hs
            | otherwise = go' (front . (h:)) hs

addHandler :: TimeoutManager -> TimeoutHandler -> IO ()
addHandler man@(TimeoutManager ref) h = mask_ $ join $ atomicModifyIORef ref
    $ \(hs, isCleaning) ->
        let hs' = h : hs
            action
                | isCleaning || not (null hs) = return ()
                | otherwise = spawnWorker man
         in ((hs', isCleaning), action)

-- | Has same semantics as @System.Timeout.timeout@, but implemented in such a
-- way to avoid high-concurrency contention issues. See:
--
-- https://github.com/snoyberg/http-client/issues/98
timeout :: Int -> IO a -> IO (Maybe a)
timeout delayU inner = do
    TimeSpec nowS nowN <- getTime Monotonic
    let (delayS, delayU') = delayU `quotRem` 1000000
        delayN = delayU' * 1000
        stopN' = nowN + delayN
        stopS' = nowS + delayS
        (stopN, stopS)
            | stopN' > 1000000000 = (stopN' - 1000000000, stopS' + 1)
            | otherwise = (stopN', stopS')
        toStop = TimeSpec stopS stopN
    toThrow <- newIORef True
    tid <- myThreadId
    let handler = TimeoutHandler toStop $ do
            toThrow' <- readIORef toThrow
            when toThrow' $ throwTo tid TimeoutTriggered
    eres <- try $ do
        addHandler timeoutManager handler
        inner `finally` writeIORef toThrow False
    return $ case eres of
        Left TimeoutTriggered -> Nothing
        Right x -> Just x

data TimeoutTriggered = TimeoutTriggered
    deriving (Show, Typeable)
instance Exception TimeoutTriggered
-}
