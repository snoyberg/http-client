{-# LANGUAGE OverloadedStrings #-}
import qualified OpenSSL.Session as SSL
import OpenSSL
import Network.Socket
import Network.BSD
import qualified Network.Socket.ByteString as B
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Enumerator hiding (head)
import qualified Data.Enumerator as E
import Http
import Safe

getSocket :: String -> Int -> IO Socket
getSocket host port = do
    proto <- getProtocolNumber "tcp"
    let hints = defaultHints { addrFlags = [AI_PASSIVE] }
    addrs <- getAddrInfo Nothing (Just host) (Just $ show port)
    let addr = head addrs
    sock <- socket (addrFamily addr) Stream defaultProtocol
    connect sock (addrAddress addr)
    return sock

withSocketConn :: String -> Int -> (HttpConn -> IO a) -> IO a
withSocketConn host port f = do
    sock <- getSocket host port
    a <- f HttpConn
        { hcRead = B.recv sock
        , hcWrite = B.sendAll sock
        }
    sClose sock
    return a

withOpenSslConn :: String -> Int -> (HttpConn -> IO a) -> IO a
withOpenSslConn host port f = do
    ctx <- SSL.context
    sock <- getSocket host port
    ssl <- SSL.connection ctx sock
    SSL.connect ssl
    a <- f HttpConn
        { hcRead = SSL.read ssl
        , hcWrite = SSL.write ssl
        }
    SSL.shutdown ssl SSL.Unidirectional
    return a

data HttpConn = HttpConn
    { hcRead :: Int -> IO S.ByteString
    , hcWrite :: S.ByteString -> IO ()
    }

connToEnum :: HttpConn -> Enumerator S.ByteString IO a
connToEnum (HttpConn r _) =
    Iteratee . loop
  where
    loop (Continue k) = do
        bs <- r 2 -- FIXME better size
        if S.null bs
            then return $ Continue k
            else do
                runIteratee (k $ Chunks [bs]) >>= loop
    loop step = return step

main = do
    Right (hs, b) <-
        withSocketsDo $ withOpenSSL $ withSocketConn "www.google.co.il" 80 go
    mapM_ (\(x, y) -> do
        S.putStr x
        putStr ": "
        S.putStr y
        putStrLn "") hs
    putStrLn ""
    S.putStr b

go hc = do
    hcWrite hc "GET / HTTP/1.1\nHost: www.google.co.il\n\n"
    run $ connToEnum hc $$ do
        (status, hs) <- iterHeaders
        body <-
            if ("Transfer-Encoding", "chunked") `elem` hs
                then iterChunks
                else case lookup "Content-Length" hs >>= readMay . S8.unpack of
                    Just len -> takeLBS len
                    Nothing -> return []
        return (hs, S.concat body)

takeLBS 0 = return []
takeLBS len = do
    mbs <- E.head
    case mbs of
        Nothing -> return []
        Just bs -> do
            let len' = len - S.length bs
            rest <- takeLBS len'
            return $ bs : rest
