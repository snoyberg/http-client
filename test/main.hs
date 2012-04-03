{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.Hspec.Monadic
import qualified Data.ByteString as S
import Test.Hspec.HUnit ()
import Test.HUnit
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Conduit
import Control.Concurrent (forkIO, killThread, threadDelay)
import Network.HTTP.Types
import Control.Exception.Lifted (try, SomeException)
import Network.HTTP.Conduit.ConnInfo
import CookieTest (cookieTest)
import Data.Conduit.Network (runTCPServer, ServerSettings (..), HostPreference (HostAny))
import Data.Conduit (($$))
import Control.Monad.Trans.Resource (register)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.UTF8 (fromString)
import Data.Conduit.List (sourceList)
import Data.CaseInsensitive (mk)
import Data.List (partition)
import qualified Data.Conduit.List as CL

app :: Application
app req =
    case pathInfo req of
        [] -> return $ responseLBS status200 [] "homepage"
        ["cookies"] -> return $ responseLBS status200 [tastyCookie] "cookies"
        _ -> return $ responseLBS status404 [] "not found"

    where tastyCookie = (mk (fromString "Set-Cookie"), fromString "flavor=chocolate-chip;")

main :: IO ()
main = hspecX $ do
    cookieTest
    describe "simpleHttp" $ do
        it "gets homepage" $ do
            tid <- forkIO $ run 3000 app
            lbs <- simpleHttp "http://127.0.0.1:3000/"
            killThread tid
            lbs @?= "homepage"
        it "throws exception on 404" $ do
            tid <- forkIO $ run 3001 app
            elbs <- try $ simpleHttp "http://127.0.0.1:3001/404"
            killThread tid
            case elbs of
                Left (_ :: SomeException) -> return ()
                Right _ -> error "Expected an exception"
    describe "httpLbs" $ do
        it "preserves 'set-cookie' headers" $ do
            tid <- forkIO $ run 3010 app
            request <- parseUrl "http://127.0.0.1:3010/cookies"
            withManager $ \manager -> do
                Response _ _ headers _ <- httpLbs request manager
                let setCookie = mk (fromString "Set-Cookie")
                    (setCookieHeaders, _) = partition ((== setCookie) . fst) headers
                liftIO $ assertBool "response contains a 'set-cookie' header" $ length setCookieHeaders > 0
            killThread tid
    describe "manager" $ do
        it "closes all connections" $ do
            clearSocketsList
            tid1 <- forkIO $ run 3002 app
            tid2 <- forkIO $ run 3003 app
            threadDelay 1000
            withManager $ \manager -> do
                let Just req1 = parseUrl "http://127.0.0.1:3002/"
                let Just req2 = parseUrl "http://127.0.0.1:3003/"
                _res1a <- http req1 manager
                _res1b <- http req1 manager
                _res2 <- http req2 manager
                return ()
            requireAllSocketsClosed
            killThread tid2
            killThread tid1
    describe "DOS protection" $ do
        it "overlong headers" $ do
            tid1 <- forkIO overLongHeaders
            threadDelay 1000
            withManager $ \manager -> do
                _ <- register $ killThread tid1
                let Just req1 = parseUrl "http://127.0.0.1:3004/"
                res1 <- try $ http req1 manager
                case res1 of
                    Left e -> liftIO $ show (e :: SomeException) @?= show OverlongHeaders
                    _ -> error "Shouldn't have worked"
        it "not overlong headers" $ do
            tid1 <- forkIO notOverLongHeaders
            threadDelay 1000
            withManager $ \manager -> do
                _ <- register $ killThread tid1
                let Just req1 = parseUrl "http://127.0.0.1:3005/"
                _ <- httpLbs req1 manager
                return ()

overLongHeaders :: IO ()
overLongHeaders = runTCPServer (ServerSettings 3004 HostAny) $ \_ sink ->
    src $$ sink
  where
    src = sourceList $ "HTTP/1.0 200 OK\r\nfoo: " : repeat "bar"

notOverLongHeaders :: IO ()
notOverLongHeaders = runTCPServer (ServerSettings 3005 HostAny) $ \src' sink -> do
    src' $$ CL.drop 1
    src $$ sink
  where
    src = sourceList $ [S.concat $ "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 16384\r\n\r\n" : ( take 16384 $ repeat "x")]
