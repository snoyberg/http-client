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
        putStrLn "reading 2 bytes"
        bs <- r 2 -- FIXME better size
        if S.null bs
            then return $ Continue k
            else do
                S.putStr bs
                runIteratee (k $ Chunks [bs]) >>= loop
    loop step = do
        putStrLn "here"
        return step

main = withSocketsDo $ withOpenSSL $ withOpenSslConn "localhost" 443 go

go hc = do
    hcWrite hc "GET / HTTP/1.1\nHost: www.google.com\n\n"
    run $ connToEnum hc $$ do
        (status, hs) <- iterHeaders
        let Just lenBS = lookup "Content-Length" hs
        let len = read $ S8.unpack lenBS
        body <- takeLBS len
        return (hs, body)
    --run $ connToEnum hc $$ E.head
    --run $ enumEOF $$ E.head

takeLBS 0 = return []
takeLBS len = do
    mbs <- E.head
    case mbs of
        Nothing -> return []
        Just bs -> do
            let len' = len - S.length bs
            rest <- takeLBS len'
            return $ bs : rest
