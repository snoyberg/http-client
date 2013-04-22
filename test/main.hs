{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.Hspec
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Test.HUnit
import Network.Wai hiding (requestBody)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsPort, settingsBeforeMainLoop)
import Network.HTTP.Conduit hiding (port)
import qualified Network.HTTP.Conduit as NHC
import Network.HTTP.Conduit.MultipartFormData
import Control.Concurrent (forkIO, killThread, putMVar, takeMVar, newEmptyMVar, threadDelay)
import Network.HTTP.Types
import Control.Exception.Lifted (try, SomeException, bracket, onException, IOException)
import qualified Data.IORef as I
import qualified Control.Exception as E (catch)
import Network.HTTP.Conduit.ConnInfo
import Network (withSocketsDo)
import Network.Socket (sClose)
import qualified Network.BSD
import CookieTest (cookieTest)
import Data.Conduit.Network (runTCPServer, serverSettings, HostPreference (..), appSink, appSource, bindPort, serverAfterBind, ServerSettings)
import qualified Data.Conduit.Network
import System.IO.Unsafe (unsafePerformIO)
import Data.Conduit (($$), yield, Flush (Chunk, Flush), runResourceT, await)
import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.UTF8 (fromString)
import Data.Conduit.List (sourceList)
import Data.CaseInsensitive (mk)
import Data.List (partition)
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as L
import Blaze.ByteString.Builder (fromByteString, toByteString)
import System.IO
import Data.Monoid (mconcat)
import Data.Time.Clock
import Data.Time.Calendar

past :: UTCTime
past = UTCTime (ModifiedJulianDay 56200) (secondsToDiffTime 0)

future :: UTCTime
future = UTCTime (ModifiedJulianDay 562000) (secondsToDiffTime 0)

cookie :: Cookie
cookie = Cookie { cookie_name = "key"
                , cookie_value = "value"
                , cookie_expiry_time = future
                , cookie_domain = "127.0.0.1"
                , cookie_path = "/dump_cookies"
                , cookie_creation_time = past
                , cookie_last_access_time = past
                , cookie_persistent = False
                , cookie_host_only = False
                , cookie_secure_only = False
                , cookie_http_only = False
                }

cookie_jar :: CookieJar
cookie_jar = createCookieJar [cookie]

app :: Application
app req =
    case pathInfo req of
        [] ->
            if maybe False ("example.com:" `S.isPrefixOf`) $ lookup "host" $ Wai.requestHeaders req
                then return $ responseLBS status200 [] "homepage for example.com"
                else return $ responseLBS status200 [] "homepage"
        ["cookies"] -> return $ responseLBS status200 [tastyCookie] "cookies"
        ["cookie_redir1"] -> return $ responseLBS status303 [tastyCookie, (hLocation, "/checkcookie")] ""
        ["checkcookie"] -> return $ case lookup hCookie $ Wai.requestHeaders req of
                                Just "flavor=chocolate-chip" -> responseLBS status200 [] "nom-nom-nom"
                                _ -> responseLBS status412 [] "Baaaw where's my chocolate?"
        ["infredir", i'] ->
            let i = read $ T.unpack i' :: Int
            in return $ responseLBS status303
                    [(hLocation, S.append "/infredir/" $ S8.pack $ show $ i+1)]
                    (L8.pack $ show i)
        ["dump_cookies"] -> return $ responseLBS status200 [] $ L.fromChunks $ return $ maybe "" id $ lookup hCookie $ Wai.requestHeaders req
        ["delayed"] -> return $ ResponseSource status200 [("foo", "bar")] $ do
            yield Flush
            liftIO $ threadDelay 30000000
            yield $ Chunk $ fromByteString "Hello World!"
        _ -> return $ responseLBS status404 [] "not found"

    where tastyCookie = (mk (fromString "Set-Cookie"), fromString "flavor=chocolate-chip;")

nextPort :: I.IORef Int
nextPort = unsafePerformIO $ I.newIORef 15452

getPort :: IO Int
getPort = do
    port <- I.atomicModifyIORef nextPort $ \p -> (p + 1, p)
    esocket <- try $ bindPort port HostIPv4
    case esocket of
        Left (_ :: IOException) -> getPort
        Right socket -> do
            sClose socket
            return port

withApp :: Application -> (Int -> IO ()) -> IO ()
withApp app' f = withApp' (const app') f

withApp' :: (Int -> Application) -> (Int -> IO ()) -> IO ()
withApp' app' f = do
    port <- getPort
    baton <- newEmptyMVar
    bracket
        (forkIO $ runSettings defaultSettings
            { settingsPort = port
            , settingsBeforeMainLoop = putMVar baton ()
            } (app' port) `onException` putMVar baton ())
        killThread
        (const $ takeMVar baton >> f port)

main :: IO ()
main = withSocketsDo $ do
  mapM_ (`hSetBuffering` LineBuffering) [stdout, stderr]
  hspec $ do
    cookieTest
    describe "simpleHttp" $ do
        it "gets homepage" $ withApp app $ \port -> do
            lbs <- simpleHttp $ "http://127.0.0.1:" ++ show port
            lbs @?= "homepage"
        it "throws exception on 404" $ withApp app $ \port -> do
            elbs <- try $ simpleHttp $ concat ["http://127.0.0.1:", show port, "/404"]
            case elbs of
                Left (_ :: SomeException) -> return ()
                Right _ -> error "Expected an exception"
    describe "httpLbs" $ do
        it "preserves 'set-cookie' headers" $ withApp app $ \port -> do
            request <- parseUrl $ concat ["http://127.0.0.1:", show port, "/cookies"]
            withManager $ \manager -> do
                response <- httpLbs request manager
                let setCookie = mk (fromString "Set-Cookie")
                    (setCookieHeaders, _) = partition ((== setCookie) . fst) (responseHeaders response)
                liftIO $ assertBool "response contains a 'set-cookie' header" $ length setCookieHeaders > 0
        it "redirects set cookies" $ withApp app $ \port -> do
            request <- parseUrl $ concat ["http://127.0.0.1:", show port, "/cookie_redir1"]
            withManager $ \manager -> do
                response <- httpLbs request manager
                liftIO $ (responseBody response) @?= "nom-nom-nom"
        it "user-defined cookie jar works" $ withApp app $ \port -> do
            request <- parseUrl $ concat ["http://127.0.0.1:", show port, "/dump_cookies"]
            withManager $ \manager -> do
                response <- httpLbs (request {redirectCount = 1, cookieJar = Just cookie_jar}) manager
                liftIO $ (responseBody response) @?= "key=value"
        it "user-defined cookie jar is not ignored when redirection is disabled" $ withApp app $ \port -> do
            request <- parseUrl $ concat ["http://127.0.0.1:", show port, "/dump_cookies"]
            withManager $ \manager -> do
                response <- httpLbs (request {redirectCount = 0, cookieJar = Just cookie_jar}) manager
                liftIO $ (responseBody response) @?= "key=value"
        it "cookie jar is available in response" $ withApp app $ \port -> do
            request <- parseUrl $ concat ["http://127.0.0.1:", show port, "/cookies"]
            withManager $ \manager -> do
                response <- httpLbs (request {cookieJar = Just def}) manager
                liftIO $ (length $ destroyCookieJar $ responseCookieJar response) @?= 1
        it "Cookie header isn't touched when no cookie jar supplied" $ withApp app $ \port -> do
            request <- parseUrl $ concat ["http://127.0.0.1:", show port, "/dump_cookies"]
            withManager $ \manager -> do
                let request_headers = (mk "Cookie", "key2=value2") : filter ((/= mk "Cookie") . fst) (NHC.requestHeaders request)
                response <- httpLbs (request {NHC.requestHeaders = request_headers, cookieJar = Nothing}) manager
                liftIO $ (responseBody response) @?= "key2=value2"
        it "Response cookie jar is nothing when request cookie jar is nothing" $ withApp app $ \port -> do
            request <- parseUrl $ concat ["http://127.0.0.1:", show port, "/cookies"]
            withManager $ \manager -> do
                response <- httpLbs (request {cookieJar = Nothing}) manager
                liftIO $ (responseCookieJar response) @?= def
    describe "manager" $ do
        it "closes all connections" $ withApp app $ \port1 -> withApp app $ \port2 -> do
            clearSocketsList
            withManager $ \manager -> do
                let Just req1 = parseUrl $ "http://127.0.0.1:" ++ show port1
                let Just req2 = parseUrl $ "http://127.0.0.1:" ++ show port2
                _res1a <- http req1 manager
                _res1b <- http req1 manager
                _res2 <- http req2 manager
                return ()
            requireAllSocketsClosed
    describe "DOS protection" $ do
        it "overlong headers" $ overLongHeaders $ \port -> do
            withManager $ \manager -> do
                let Just req1 = parseUrl $ "http://127.0.0.1:" ++ show port
                res1 <- try $ http req1 manager
                case res1 of
                    Left e -> liftIO $ show (e :: SomeException) @?= show OverlongHeaders
                    _ -> error "Shouldn't have worked"
        it "not overlong headers" $ notOverLongHeaders $ \port -> do
            withManager $ \manager -> do
                let Just req1 = parseUrl $ "http://127.0.0.1:" ++ show port
                _ <- httpLbs req1 manager
                return ()
    describe "redirects" $ do
        it "doesn't double escape" $ redir $ \port -> do
            withManager $ \manager -> do
                let go (encoded, final) = do
                        let Just req1 = parseUrl $ concat ["http://127.0.0.1:", show port, "/redir/", encoded]
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
        it "TooManyRedirects: redirect request body is preserved" $ withApp app $ \port -> do
            let Just req = parseUrl $ concat ["http://127.0.0.1:", show port, "/infredir/0"]
            let go (res, i) = liftIO $ responseBody res @?= (L8.pack $ show i)
            E.catch (withManager $ \manager -> do
                void $ http req{redirectCount=5} manager) $
                \(TooManyRedirects redirs) -> mapM_ go (zip redirs [5,4..0 :: Int])
    describe "chunked request body" $ do
        it "works" $ echo $ \port -> do
            withManager $ \manager -> do
                let go bss = do
                        let Just req1 = parseUrl $ "http://127.0.0.1:" ++ show port
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
        it "works" $ noStatusMessage $ \port -> do
            req <- parseUrl $ "http://127.0.0.1:" ++ show port
            withManager $ \manager -> do
                res <- httpLbs req manager
                liftIO $ do
                    Network.HTTP.Conduit.responseStatus res `shouldBe` status200
                    responseBody res `shouldBe` "foo"

    describe "redirect" $ do
        it "ignores large response bodies" $ do
            let app' port req =
                    case pathInfo req of
                        ["foo"] -> return $ responseLBS status200 [] "Hello World!"
                        _ -> return $ ResponseSource status301 [("location", S8.pack $ "http://127.0.0.1:" ++ show port ++ "/foo")] $ forever $ yield $ Chunk $ fromByteString "hello\n"
            withApp' app' $ \port -> withManager $ \manager -> do
                req <- parseUrl $ "http://127.0.0.1:" ++ show port
                res <- httpLbs req manager
                liftIO $ do
                    Network.HTTP.Conduit.responseStatus res `shouldBe` status200
                    responseBody res `shouldBe` "Hello World!"
    describe "multipart/form-data" $ do
        it "formats correctly" $ do
            let bd = "---------------------------190723902820679116301912680260"
            (RequestBodySource _ src) <- renderParts bd
                [partBS "email" ""
                ,partBS "parent_id" "70488"
                ,partBS "captcha" ""
                ,partBS "homeboard" "0chan.hk"
                ,partBS "text" $ TE.encodeUtf8 ">>72127\r\nМы работаем над этим."
                ,partFileSource "upload" "nyan.gif"
                ]
            mfd <- fmap (toByteString . mconcat) $ runResourceT $ src $$ CL.consume
            exam <- S.readFile "multipart-example.bin"
            mfd @?= exam

    describe "HTTP/1.0" $ do
        it "BaseHTTP" $ do
            let baseHTTP app' = do
                    _ <- appSource app' $$ await
                    yield "HTTP/1.0 200 OK\r\n\r\nThis is it!" $$ appSink app'
            withCApp baseHTTP $ \port -> withManager $ \manager -> do
                req <- parseUrl $ "http://127.0.0.1:" ++ show port
                res1 <- httpLbs req manager
                res2 <- httpLbs req manager
                liftIO $ res1 @?= res2

    describe "hostAddress" $ do
        it "overrides host" $ withApp app $ \port -> do
            entry <- Network.BSD.getHostByName "127.0.0.1"
            req' <- parseUrl $ "http://example.com:" ++ show port
            let req = req' { hostAddress = Just $ Network.BSD.hostAddress entry }
            res <- withManager $ httpLbs req
            responseBody res @?= "homepage for example.com"

    describe "managerResponseTimeout" $ do
        it "works" $ withApp app $ \port -> do
            req1 <- parseUrl $ "http://localhost:" ++ show port
            let req2 = req1 { responseTimeout = Just 5000000 }
            withManagerSettings def { managerResponseTimeout = Just 1 } $ \man -> do
                eres1 <- try $ httpLbs req1 man
                case eres1 of
                    Left (FailedConnectionException _ _) -> return ()
                    _ -> error "Did not time out"
                _ <- httpLbs req2 man
                return ()

    describe "delayed body" $ do
        it "works" $ withApp app $ \port -> do
            req <- parseUrl $ "http://localhost:" ++ show port ++ "/delayed"
            withManager $ \man -> do
                res <- http req man
                error $ show $ fmap (const ()) res
                return ()

withCApp :: Data.Conduit.Network.Application IO -> (Int -> IO ()) -> IO ()
withCApp app' f = do
    port <- getPort
    baton <- newEmptyMVar
    let start = putMVar baton ()
        settings :: ServerSettings IO
        settings = (serverSettings port HostAny :: ServerSettings IO) { serverAfterBind = const start }
    bracket
        (forkIO $ runTCPServer settings app' `onException` start)
        killThread
        (const $ takeMVar baton >> f port)

overLongHeaders :: (Int -> IO ()) -> IO ()
overLongHeaders =
    withCApp $ \app' -> src $$ appSink app'
  where
    src = sourceList $ "HTTP/1.0 200 OK\r\nfoo: " : repeat "bar"

notOverLongHeaders :: (Int -> IO ()) -> IO ()
notOverLongHeaders = withCApp $ \app' -> do
    appSource app' $$ CL.drop 1
    src $$ appSink app'
  where
    src = sourceList $ [S.concat $ "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 16384\r\n\r\n" : ( take 16384 $ repeat "x")]

redir :: (Int -> IO ()) -> IO ()
redir =
    withApp' redirApp
  where
    redirApp port req =
        case pathInfo req of
            ["redir", foo] -> return $ responseLBS status301
                [ ("Location", S8.pack (concat ["http://127.0.0.1:", show port, "/content/"]) `S.append` escape foo)
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

echo :: (Int -> IO ()) -> IO ()
echo = withApp $ \req -> do
    bss <- Wai.requestBody req $$ CL.consume
    return $ responseLBS status200 [] $ L.fromChunks bss

noStatusMessage :: (Int -> IO ()) -> IO ()
noStatusMessage =
    withCApp $ \app' -> src $$ appSink app'
  where
    src = yield "HTTP/1.0 200\r\nContent-Length: 3\r\n\r\nfoo: barbazbin"
