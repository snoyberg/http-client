{-# LANGUAGE ScopedTypeVariables #-}
module Network.HTTP.Client.Connection where

import Data.ByteString (ByteString, empty)
import Data.IORef
import Control.Monad (join)
import Network.Socket (Socket, sClose, HostAddress, defaultHints, addrFlags)
import qualified Network.Socket as NS
import Network.Socket.ByteString (sendAll, recv)
import qualified Control.Exception as E

data Connection = Connection
    { connectionRead :: !(IO ByteString)
      -- ^ If no more data, return empty.
    , connectionUnread :: !(ByteString -> IO ())
      -- ^ Return data to be read next time.
    , connectionWrite :: !(ByteString -> IO ())
      -- ^ Send data to server
    , connectionClose :: !(IO ())
    }

-- | For testing
dummyConnection :: [ByteString] -- ^ input
                -> IO (Connection, IO [ByteString], IO [ByteString])
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

makeConnection :: IO ByteString -- ^ read
               -> (ByteString -> IO ()) -- ^ write
               -> IO () -- ^ close
               -> IO Connection
makeConnection r w c = do
    istack <- newIORef []
    _ <- mkWeakIORef istack c
    return $! Connection
        { connectionRead = join $ atomicModifyIORef istack $ \stack ->
            case stack of
                x:xs -> (xs, return x)
                [] -> ([], r)
        , connectionUnread = \x -> atomicModifyIORef istack $ \stack -> (x:stack, ())
        , connectionWrite = w
        , connectionClose = c
        }

socketConnection :: Socket -> IO Connection
socketConnection socket = makeConnection
    (recv socket 4096)
    (sendAll socket)
    (sClose socket)

openSocketConnection :: Maybe HostAddress
                     -> String -- ^ host
                     -> Int -- ^ port
                     -> IO Connection
openSocketConnection hostAddress host port = do
    let hints = NS.defaultHints {
                          NS.addrFlags = [NS.AI_ADDRCONFIG]
                        , NS.addrSocketType = NS.Stream
                        }
    addrs <- case hostAddress of
        Nothing ->
            NS.getAddrInfo (Just hints) (Just host) (Just $ show port)
        Just ha ->
            return
                [NS.AddrInfo
                 { NS.addrFlags = []
                 , NS.addrFamily = NS.AF_INET
                 , NS.addrSocketType = NS.Stream
                 , NS.addrProtocol = 6 -- tcp
                 , NS.addrAddress = NS.SockAddrInet (toEnum port) ha
                 , NS.addrCanonName = Nothing
                 }]

    firstSuccessful addrs $ \addr ->
        E.bracketOnError
            (NS.socket (NS.addrFamily addr) (NS.addrSocketType addr)
                       (NS.addrProtocol addr))
            (NS.sClose)
            (\sock -> do
                NS.setSocketOption sock NS.NoDelay 1
                NS.connect sock (NS.addrAddress addr)
                socketConnection sock)

firstSuccessful :: [NS.AddrInfo] -> (NS.AddrInfo -> IO a) -> IO a
firstSuccessful []     _  = error "getAddrInfo returned empty list"
firstSuccessful (a:as) cb =
    cb a `E.catch` \(e :: E.IOException) ->
        case as of
            [] -> E.throwIO e
            _  -> firstSuccessful as cb