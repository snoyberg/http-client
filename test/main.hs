{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.Hspec.Monadic
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Test.Hspec.HUnit ()
import Test.HUnit
import Network.Wai hiding (requestBody)
import qualified Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Conduit
import Network.HTTP.Conduit.Browser
import Data.ByteString.Base64 (encode)
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
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as L
import Blaze.ByteString.Builder (fromByteString)

strictToLazy :: S.ByteString -> L.ByteString
strictToLazy = L.fromChunks . replicate 1

lazyToStrict :: L.ByteString -> S.ByteString
lazyToStrict = S.concat . L.toChunks

dummy :: S.ByteString
dummy = "dummy"

user :: S.ByteString
user = "user"

pass :: S.ByteString
pass = "pass"

app :: Application
app req =
    case pathInfo req of
        [] -> return $ responseLBS status200 [] "homepage"
        ["cookies"] -> return $ responseLBS status200 [tastyCookie] "cookies"
        ["print-cookies"] -> return $ responseLBS status200 [] $ getHeader "Cookie"
        ["useragent"] -> return $ responseLBS status200 [] $ getHeader "User-Agent"
        ["authorities"] -> return $ responseLBS status200 [] $ getHeader "Authorization"
        ["redir1"] -> return $ responseLBS temporaryRedirect307 [redir2] L.empty
        ["redir2"] -> return $ responseLBS temporaryRedirect307 [redir3] L.empty
        ["redir3"] -> return $ responseLBS status200 [] $ strictToLazy dummy
        _ -> return $ responseLBS status404 [] "not found"

    where tastyCookie = (mk (fromString "Set-Cookie"), fromString "flavor=chocolate-chip;")
          getHeader s = strictToLazy $ case lookup s $ Network.Wai.requestHeaders req of
                            Just a -> a
                            Nothing -> S.empty
          redir2 = (mk (fromString "Location"), fromString "/redir2")
          redir3 = (mk (fromString "Location"), fromString "/redir3")

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
    describe "browser" $ do
        it "cookie jar works" $ do
            tid <- forkIO $ run 3011 app
            request1 <- parseUrl "http://127.0.0.1:3011/cookies"
            request2 <- parseUrl "http://127.0.0.1:3011/print-cookies"
            elbs <- withManager $ \manager -> do
                browse manager $ do
                    _ <- makeRequestLbs request1
                    makeRequestLbs request2
            killThread tid
            if (lazyToStrict $ responseBody elbs) /= fromString "flavor=chocolate-chip"
                 then error "Should have gotten the cookie back!"
                 else return ()
        it "cookie filter can deny cookies" $ do
            tid <- forkIO $ run 3011 app
            request1 <- parseUrl "http://127.0.0.1:3011/cookies"
            request2 <- parseUrl "http://127.0.0.1:3011/print-cookies"
            elbs <- withManager $ \manager -> do
                browse manager $ do
                    setCookieFilter $ const $ const $ return False
                    _ <- makeRequestLbs request1
                    makeRequestLbs request2
            killThread tid
            if (lazyToStrict $ responseBody elbs) /= S.empty
                 then error "Shouldn't have gotten the cookie back!"
                 else return ()
        it "can save and load cookie jar" $ do
            tid <- forkIO $ run 3011 app
            request1 <- parseUrl "http://127.0.0.1:3011/cookies"
            request2 <- parseUrl "http://127.0.0.1:3011/print-cookies"
            (elbs1, elbs2) <- withManager $ \manager -> do
                browse manager $ do
                    _ <- makeRequestLbs request1
                    cookie_jar <- getCookieJar
                    setCookieJar def
                    elbs1 <- makeRequestLbs request2
                    setCookieJar cookie_jar
                    elbs2 <- makeRequestLbs request2
                    return (elbs1, elbs2)
            killThread tid
            if (((lazyToStrict $ responseBody elbs1) /= S.empty) ||
                ((lazyToStrict $ responseBody elbs2) /= fromString "flavor=chocolate-chip"))
                 then error "Cookie jar got garbled up!"
                 else return ()
        it "user agent sets correctly" $ do
            tid <- forkIO $ run 3012 app
            request <- parseUrl "http://127.0.0.1:3012/useragent"
            elbs <- withManager $ \manager -> do
                browse manager $ do
                    setUserAgent $ fromString "abcd"
                    makeRequestLbs request
            killThread tid
            if (lazyToStrict $ responseBody elbs) /= fromString "abcd"
                 then error "Should have gotten the user agent back!"
                 else return ()
        it "authorities get set correctly" $ do
            tid <- forkIO $ run 3013 app
            request <- parseUrl "http://127.0.0.1:3013/authorities"
            elbs <- withManager $ \manager -> do
                browse manager $ do
                    setAuthorities $ const $ Just (user, pass)
                    makeRequestLbs request
            killThread tid
            if (lazyToStrict $ responseBody elbs) /= (fromString "Basic " `S.append` (encode $ user `S.append` ":" `S.append` pass))
                 then error "Authorities didn't get set correctly!"
                 else return ()
        it "can follow redirects" $ do
            tid <- forkIO $ run 3014 app
            request <- parseUrl "http://127.0.0.1:3014/redir1"
            elbs <- withManager $ \manager -> do
                browse manager $ do
                    setMaxRedirects 2
                    makeRequestLbs request
            killThread tid
            if (lazyToStrict $ responseBody elbs) /= dummy
                 then error "Should be able to follow 2 redirects"
                 else return ()
        it "max redirects fails correctly" $ do
            tid <- forkIO $ run 3015 app
            request <- parseUrl "http://127.0.0.1:3015/redir1"
            elbs <- try $ withManager $ \manager -> do
                browse manager $ do
                    setMaxRedirects 1
                    makeRequestLbs request
            killThread tid
            case elbs of
                 Left (TooManyRedirects _) -> return ()
                 _ -> error "Shouldn't have followed all those redirects!"
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
    describe "redirects" $ do
        it "doesn't double escape" $ do
            tid <- forkIO redir
            threadDelay 1000000
            withManager $ \manager -> do
                _ <- register $ killThread tid
                let go (encoded, final) = do
                        let Just req1 = parseUrl $ "http://127.0.0.1:3006/redir/" ++ encoded
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
                        let Just req1 = parseUrl "http://127.0.0.1:3007"
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

redir :: IO ()
redir =
    run 3006 redirApp
  where
    redirApp req =
        case pathInfo req of
            ["redir", foo] -> return $ responseLBS status301
                [ ("Location", "http://127.0.0.1:3006/content/" `S.append` escape foo)
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
echo = run 3007 $ \req -> do
    bss <- Network.Wai.requestBody req $$ CL.consume
    return $ responseLBS status200 [] $ L.fromChunks bss
