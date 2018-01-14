{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Network.HTTP.Client.Connection
    ( connectionReadLine
    , connectionReadLineWith
    , connectionDropTillBlankLine
    , dummyConnection
    , openSocketConnection
    , openSocketConnectionSize
    , makeConnection
    , socketConnection
    ) where

import Data.ByteString (ByteString, empty)
import Data.IORef
import Control.Monad
import Network.HTTP.Client.Types
import qualified Network.HTTP.Client.Socket as NS
import qualified Control.Exception as E
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Word (Word8)
import Data.Function (fix)

connectionReadLine :: Connection -> IO ByteString
connectionReadLine conn = do
    bs <- connectionRead conn
    when (S.null bs) $ throwHttp IncompleteHeaders
    connectionReadLineWith conn bs

-- | Keep dropping input until a blank line is found.
connectionDropTillBlankLine :: Connection -> IO ()
connectionDropTillBlankLine conn = fix $ \loop -> do
    bs <- connectionReadLine conn
    unless (S.null bs) loop

connectionReadLineWith :: Connection -> ByteString -> IO ByteString
connectionReadLineWith conn bs0 =
    go bs0 id 0
  where
    go bs front total =
        case S.break (== charLF) bs of
            (_, "") -> do
                let total' = total + S.length bs
                when (total' > 4096) $ throwHttp OverlongHeaders
                bs' <- connectionRead conn
                when (S.null bs') $ throwHttp IncompleteHeaders
                go bs' (front . (bs:)) total'
            (x, S.drop 1 -> y) -> do
                unless (S.null y) $! connectionUnread conn y
                return $! killCR $! S.concat $! front [x]

charLF, charCR :: Word8
charLF = 10
charCR = 13

killCR :: ByteString -> ByteString
killCR bs
    | S.null bs = bs
    | S.last bs == charCR = S.init bs
    | otherwise = bs

-- | For testing
dummyConnection :: [ByteString] -- ^ input
                -> IO (Connection, IO [ByteString], IO [ByteString]) -- ^ conn, output, input
dummyConnection input0 = do
    iinput <- newIORef input0
    ioutput <- newIORef []
    return (Connection
        { connectionRead = atomicModifyIORef iinput $ \input ->
            case input of
                [] -> ([], empty)
                x:xs -> (xs, x)
        , connectionUnread = \x -> atomicModifyIORef iinput $ \input -> (x:input, ())
        , connectionWrite = \x -> atomicModifyIORef ioutput $ \output -> (output ++ [x], ())
        , connectionClose = return ()
        }, atomicModifyIORef ioutput $ \output -> ([], output), readIORef iinput)

-- | Create a new 'Connection' from a read, write, and close function.
--
-- @since 0.5.3
makeConnection :: IO ByteString -- ^ read
               -> (ByteString -> IO ()) -- ^ write
               -> IO () -- ^ close
               -> IO Connection
makeConnection r w c = do
    istack <- newIORef []

    -- it is necessary to make sure we never read from or write to
    -- already closed connection.
    closedVar <- newIORef False

    let close = do
          closed <- atomicModifyIORef closedVar (\closed -> (True, closed))
          unless closed $
            c

    _ <- mkWeakIORef istack close
    return $! Connection
        { connectionRead = do
            closed <- readIORef closedVar
            when closed $ throwHttp ConnectionClosed
            join $ atomicModifyIORef istack $ \stack ->
              case stack of
                  x:xs -> (xs, return x)
                  [] -> ([], r)

        , connectionUnread = \x -> do
            closed <- readIORef closedVar
            when closed $ throwHttp ConnectionClosed
            atomicModifyIORef istack $ \stack -> (x:stack, ())

        , connectionWrite = \x -> do
            closed <- readIORef closedVar
            when closed $ throwHttp ConnectionClosed
            w x

        , connectionClose = close
        }

-- | Create a new 'Connection' from a 'Socket'.
--
-- @since 0.5.3
socketConnection :: NS.Socket'
                 -> Int -- ^ chunk size
                 -> IO Connection
socketConnection socket chunksize = makeConnection
    (NS.receive socket chunksize mempty)
    (\chunk -> void (NS.sendAll socket chunk mempty))
    (NS.close socket)

openSocketConnection :: (NS.Socket' -> IO ())
                     -> Maybe NS.HostAddress
                     -> String -- ^ host
                     -> Int -- ^ port
                     -> IO Connection
openSocketConnection f = openSocketConnectionSize f 8192

openSocketConnectionSize :: (NS.Socket' -> IO ())
                         -> Int -- ^ chunk size
                         -> Maybe NS.HostAddress
                         -> String -- ^ host
                         -> Int -- ^ port
                         -> IO Connection
openSocketConnectionSize tweakSocket chunksize hostAddress' host' port' = do
    let hints = mconcat [NS.aiAddressConfig]
    addrs <- case hostAddress' of
        Nothing ->
            NS.getAddressInfo (Just (S8.pack host')) (Just (S8.pack (show port'))) hints
        Just ha ->
            return
              [NS.AddressInfo
                { NS.addressInfoFlags = mempty
                , NS.socketAddress = NS.SocketAddressInet (NS.inetAddressFromTuple ha) (fromIntegral port')
                , NS.canonicalName = Nothing
                }]

    firstSuccessful addrs $ \addr ->
        E.bracketOnError
            NS.socket
            NS.close
            (\sock -> do
                NS.setSocketOption sock (NS.NoDelay True)
                tweakSocket sock
                NS.connect sock (NS.socketAddress addr)
                socketConnection sock chunksize)

firstSuccessful :: [NS.AddrInfo] -> (NS.AddrInfo -> IO a) -> IO a
firstSuccessful []     _  = error "getAddrInfo returned empty list"
firstSuccessful (a:as) cb =
    cb a `E.catch` \(e :: E.IOException) ->
        case as of
            [] -> E.throwIO e
            _  -> firstSuccessful as cb
