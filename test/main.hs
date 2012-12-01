{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.Hspec
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Test.HUnit
import Network.Wai hiding (requestBody)
import qualified Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Conduit
import Data.ByteString.Base64 (encode)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Network.HTTP.Types
import Control.Exception.Lifted (try, SomeException)
import Network.HTTP.Conduit.ConnInfo
import Network (withSocketsDo)
import CookieTest (cookieTest)
import Data.Conduit.Network (runTCPServer, serverSettings, HostPreference (HostAny), appSink, appSource)
import Data.Conduit (($$), yield, Flush (Chunk))
import Control.Monad.Trans.Resource (register)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever)
import Data.ByteString.UTF8 (fromString)
import Data.Conduit.List (sourceList)
import Data.CaseInsensitive (mk)
import Data.List (partition)
import qualified Data.Conduit.List as CL
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as L
import Blaze.ByteString.Builder (fromByteString)

app :: Application
app req =
    case pathInfo req of
        [] -> return $ responseLBS status200 [] "homepage"
        ["cookies"] -> return $ responseLBS status200 [tastyCookie] "cookies"
        _ -> return $ responseLBS status404 [] "not found"

    where tastyCookie = (mk (fromString "Set-Cookie"), fromString "flavor=chocolate-chip;")

main :: IO ()
main = withSocketsDo $ hspec $ do
    cookieTest
    describe "simpleHttp" $ do
        it "gets homepage" $ do
            tid <- forkIO $ run 13000 app
            threadDelay 10000
            lbs <- simpleHttp "http://127.0.0.1:13000/"
            killThread tid
            lbs @?= "homepage"
        it "throws exception on 404" $ do
            tid <- forkIO $ run 13001 app
            threadDelay 10000
            elbs <- try $ simpleHttp "http://127.0.0.1:13001/404"
            killThread tid
            case elbs of
                Left (_ :: SomeException) -> return ()
                Right _ -> error "Expected an exception"
    describe "httpLbs" $ do
        it "preserves 'set-cookie' headers" $ do
            tid <- forkIO $ run 13010 app
            threadDelay 10000
            request <- parseUrl "http://127.0.0.1:13010/cookies"
            withManager $ \manager -> do
                Response _ _ headers _ <- httpLbs request manager
                let setCookie = mk (fromString "Set-Cookie")
                    (setCookieHeaders, _) = partition ((== setCookie) . fst) headers
                liftIO $ assertBool "response contains a 'set-cookie' header" $ length setCookieHeaders > 0
            killThread tid
    describe "manager" $ do
        it "closes all connections" $ do
            clearSocketsList
            tid1 <- forkIO $ run 13002 app
            tid2 <- forkIO $ run 13003 app
            threadDelay 1000
            withManager $ \manager -> do
                let Just req1 = parseUrl "http://127.0.0.1:13002/"
                let Just req2 = parseUrl "http://127.0.0.1:13003/"
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
                let Just req1 = parseUrl "http://127.0.0.1:13004/"
                res1 <- try $ http req1 manager
                case res1 of
                    Left e -> liftIO $ show (e :: SomeException) @?= show OverlongHeaders
                    _ -> error "Shouldn't have worked"
        it "not overlong headers" $ do
            tid1 <- forkIO notOverLongHeaders
            threadDelay 1000
            withManager $ \manager -> do
                _ <- register $ killThread tid1
                let Just req1 = parseUrl "http://127.0.0.1:13005/"
                _ <- httpLbs req1 manager
                return ()
    describe "redirects" $ do
        it "doesn't double escape" $ do
            tid <- forkIO redir
            threadDelay 1000000
            withManager $ \manager -> do
                _ <- register $ killThread tid
                let go (encoded, final) = do
                        let Just req1 = parseUrl $ "http://127.0.0.1:13006/redir/" ++ encoded
                        res <- httpLbs req1 manager
                        liftIO $ Network.HTTP.Conduit.responseStatus res @?= status200
                        liftIO $ responseBody res @?= L.fromChunks [TE.encodeUtf8 final]
                mapM_ go
                    [ ("hello world%2F", "hello world/")
                    , ("%D7%A9%D7%9C%D7%95%D7%9D", "שלום")
                    , ("simple", "simple")
                    , ("hello%20world", "hello world")
                    , ("hello%20world%3f%23", "hello world?#")
                    ]

    describe "chunked request body" $ do
        it "works" $ do
            tid <- forkIO echo
            threadDelay 1000000
            withManager $ \manager -> do
                _ <- register $ killThread tid
                let go bss = do
                        let Just req1 = parseUrl "http://127.0.0.1:13007"
                            src = sourceList $ map fromByteString bss
                            lbs = L.fromChunks bss
                        res <- httpLbs req1
                            { method = "POST"
                            , requestBody = RequestBodySourceChunked src
                            } manager
                        liftIO $ Network.HTTP.Conduit.responseStatus res @?= status200
                        let ts = S.concat . L.toChunks
                        liftIO $ ts (responseBody res) @?= ts lbs
                mapM_ go
                    [ ["hello", "world"]
                    , replicate 500 "foo\003\n\r"
                    ]
    describe "no status message" $ do
        it "works" $ do
            tid <- forkIO noStatusMessage
            threadDelay 1000000
            withManager $ \manager -> do
                _ <- register $ killThread tid
                req <- parseUrl "http://127.0.0.1:13008"
                res <- httpLbs req manager
                liftIO $ do
                    Network.HTTP.Conduit.responseStatus res `shouldBe` status200
                    responseBody res `shouldBe` "foo"

    describe "redirect" $ do
        it "ignores large response bodies" $ do
            tid <- forkIO $ run 13100 $ \req ->
                case pathInfo req of
                    ["foo"] -> return $ responseLBS status200 [] "Hello World!"
                    _ -> return $ ResponseSource status301 [("location", "http://localhost:13100/foo")] $ forever $ yield $ Chunk $ fromByteString "hello\n"
            threadDelay 1000000
            withManager $ \manager -> do
                _ <- register $ killThread tid
                req <- parseUrl "http://127.0.0.1:13100"
                res <- httpLbs req manager
                liftIO $ do
                    Network.HTTP.Conduit.responseStatus res `shouldBe` status200
                    responseBody res `shouldBe` "Hello World!"

overLongHeaders :: IO ()
overLongHeaders = runTCPServer (serverSettings 13004 HostAny) $ \app ->
    src $$ appSink app
  where
    src = sourceList $ "HTTP/1.0 200 OK\r\nfoo: " : repeat "bar"

notOverLongHeaders :: IO ()
notOverLongHeaders = runTCPServer (serverSettings 13005 HostAny) $ \app -> do
    appSource app  $$ CL.drop 1
    src $$ appSink app
  where
    src = sourceList $ [S.concat $ "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 16384\r\n\r\n" : ( take 16384 $ repeat "x")]

redir :: IO ()
redir =
    run 13006 redirApp
  where
    redirApp req =
        case pathInfo req of
            ["redir", foo] -> return $ responseLBS status301
                [ ("Location", "http://127.0.0.1:13006/content/" `S.append` escape foo)
                ]
                ""
            ["content", foo] -> return $ responseLBS status200 [] $ L.fromChunks [TE.encodeUtf8 foo]
            _ -> return $ responseLBS status404 [] ""
    escape = S8.concatMap (S8.pack . encodeUrlChar) . TE.encodeUtf8

    encodeUrlChar :: Char -> String
    encodeUrlChar c
        -- List of unreserved characters per RFC 3986
        -- Gleaned from http://en.wikipedia.org/wiki/Percent-encoding
        | 'A' <= c && c <= 'Z' = [c]
        | 'a' <= c && c <= 'z' = [c]
        | '0' <= c && c <= '9' = [c]
    encodeUrlChar c@'-' = [c]
    encodeUrlChar c@'_' = [c]
    encodeUrlChar c@'.' = [c]
    encodeUrlChar c@'~' = [c]
    encodeUrlChar y =
        let (a, c) = fromEnum y `divMod` 16
            b = a `mod` 16
            showHex' x
                | x < 10 = toEnum $ x + (fromEnum '0')
                | x < 16 = toEnum $ x - 10 + (fromEnum 'A')
                | otherwise = error $ "Invalid argument to showHex: " ++ show x
         in ['%', showHex' b, showHex' c]

echo :: IO ()
echo = run 13007 $ \req -> do
    bss <- Network.Wai.requestBody req $$ CL.consume
    return $ responseLBS status200 [] $ L.fromChunks bss

noStatusMessage :: IO ()
noStatusMessage = runTCPServer (serverSettings 13008 HostAny) $ \app ->
    src $$ appSink app
  where
    src = yield "HTTP/1.0 200\r\nContent-Length: 3\r\n\r\nfoo: barbazbin"
