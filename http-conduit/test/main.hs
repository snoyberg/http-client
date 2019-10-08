{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.Hspec
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Test.HUnit
import Network.Wai hiding (requestBody)
import Network.Wai.Conduit (responseSource, sourceRequestBody)
import Network.HTTP.Client (streamFile)
import System.IO.Temp (withSystemTempFile)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setBeforeMainLoop, Settings, setTimeout)
import Network.HTTP.Conduit hiding (port)
import qualified Network.HTTP.Conduit as NHC
import Network.HTTP.Client.MultipartFormData
import Control.Concurrent (forkIO, killThread, putMVar, takeMVar, newEmptyMVar, threadDelay)
import Network.HTTP.Types
import UnliftIO.Exception (try, SomeException, bracket, onException, IOException)
import qualified Data.IORef as I
import qualified Control.Exception as E (catch)
import qualified Network.Socket as NS
import CookieTest (cookieTest)
#if MIN_VERSION_conduit(1,1,0)
import Data.Conduit.Network (runTCPServer, serverSettings, appSink, appSource, ServerSettings)
import Data.Streaming.Network (bindPortTCP, setAfterBind)
#define bindPort bindPortTCP
#else
import Data.Conduit.Network (runTCPServer, serverSettings, HostPreference (..), appSink, appSource, bindPort, serverAfterBind, ServerSettings)
#endif
import qualified Data.Conduit.Network
import System.IO.Unsafe (unsafePerformIO)
import Data.Conduit ((.|), yield, Flush (Chunk, Flush), await, runConduit)
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
import Blaze.ByteString.Builder (fromByteString)
import System.IO
import Data.Time.Clock
import Data.Time.Calendar
import qualified Network.Wai.Handler.WarpTLS as WT
import Network.Connection (settingDisableCertificateValidation)
import Data.Default.Class (def)
import qualified Data.Aeson as A
import qualified Network.HTTP.Simple as Simple
import Data.Monoid (mempty)
import Control.Monad.Trans.Resource (runResourceT)

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

app :: Wai.Request -> IO Wai.Response
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
        ["delayed"] -> return $ responseSource status200 [("foo", "bar")] $ do
            yield Flush
            liftIO $ threadDelay 30000000
            yield $ Chunk $ fromByteString "Hello World!"
        _ -> return $ responseLBS status404 [] "not found"

    where tastyCookie = (mk (fromString "Set-Cookie"), fromString "flavor=chocolate-chip;")

nextPort :: I.IORef Int
nextPort = unsafePerformIO $ I.newIORef 15452
{-# NOINLINE nextPort #-}

getPort :: IO Int
getPort = do
    port <- I.atomicModifyIORef nextPort $ \p -> (p + 1, p + 1)
    esocket <- try $ bindPort port "*4"
    case esocket of
        Left (_ :: IOException) -> getPort
        Right socket -> do
            NS.close socket
            return port

withApp :: (Wai.Request -> IO Wai.Response) -> (Int -> IO ()) -> IO ()
withApp app' f = withApp' (const app') f

withApp' :: (Int -> Wai.Request -> IO Wai.Response) -> (Int -> IO ()) -> IO ()
withApp' = withAppSettings id

withAppSettings :: (Settings -> Settings)
                -> (Int -> Wai.Request -> IO Wai.Response)
                -> (Int -> IO ())
                -> IO ()
withAppSettings modSettings app' f = do
    port <- getPort
    baton <- newEmptyMVar
    bracket
        (forkIO $ runSettings (modSettings $
            setPort port
            $ setBeforeMainLoop (putMVar baton ())
              defaultSettings) (app'' port) `onException` putMVar baton ())
        killThread
        (const $ takeMVar baton >> f port)
  where
    app'' port req sendResponse = do
        res <- app' port req
        sendResponse res

withAppTls :: (Wai.Request -> IO Wai.Response) -> (Int -> IO ()) -> IO ()
withAppTls app' f = withAppTls' (const app') f

withAppTls' :: (Int -> Wai.Request -> IO Wai.Response) -> (Int -> IO ()) -> IO ()
withAppTls' app' f = do
    port <- getPort
    baton <- newEmptyMVar
    bracket
        (forkIO $ WT.runTLS WT.defaultTlsSettings (
            setPort port
            $ setBeforeMainLoop (putMVar baton ())
              defaultSettings)
            (app'' port) `onException` putMVar baton ())
        killThread
        (const $ takeMVar baton >> f port)
  where
    app'' port req sendResponse = do
        res <- app' port req
        sendResponse res

main :: IO ()
main = do
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
                Left (HttpExceptionRequest _ StatusCodeException {}) -> return ()
                _ -> error "Expected an exception"
    describe "httpLbs" $ do
        it "preserves 'set-cookie' headers" $ withApp app $ \port -> do
            request <- parseUrlThrow $ concat ["http://127.0.0.1:", show port, "/cookies"]
            manager <- newManager tlsManagerSettings
            response <- httpLbs request manager
            let setCookie = mk (fromString "Set-Cookie")
                (setCookieHeaders, _) = partition ((== setCookie) . fst) (NHC.responseHeaders response)
            assertBool "response contains a 'set-cookie' header" $ length setCookieHeaders > 0
        it "redirects set cookies" $ withApp app $ \port -> do
            request <- parseUrlThrow $ concat ["http://127.0.0.1:", show port, "/cookie_redir1"]
            manager <- newManager tlsManagerSettings
            response <- httpLbs request manager
            (responseBody response) @?= "nom-nom-nom"
        it "user-defined cookie jar works" $ withApp app $ \port -> do
            request <- parseUrlThrow $ concat ["http://127.0.0.1:", show port, "/dump_cookies"]
            manager <- newManager tlsManagerSettings
            response <- httpLbs (request {redirectCount = 1, cookieJar = Just cookie_jar}) manager
            (responseBody response) @?= "key=value"
        it "user-defined cookie jar is not ignored when redirection is disabled" $ withApp app $ \port -> do
            request <- parseUrlThrow $ concat ["http://127.0.0.1:", show port, "/dump_cookies"]
            manager <- newManager tlsManagerSettings
            response <- httpLbs (request {redirectCount = 0, cookieJar = Just cookie_jar}) manager
            (responseBody response) @?= "key=value"
        it "cookie jar is available in response" $ withApp app $ \port -> do
            request <- parseUrlThrow $ concat ["http://127.0.0.1:", show port, "/cookies"]
            manager <- newManager tlsManagerSettings
            response <- httpLbs (request {cookieJar = Just Data.Monoid.mempty}) manager
            (length $ destroyCookieJar $ responseCookieJar response) @?= 1
        it "Cookie header isn't touched when no cookie jar supplied" $ withApp app $ \port -> do
            request <- parseUrlThrow $ concat ["http://127.0.0.1:", show port, "/dump_cookies"]
            manager <- newManager tlsManagerSettings
            let request_headers = (mk "Cookie", "key2=value2") : filter ((/= mk "Cookie") . fst) (NHC.requestHeaders request)
            response <- httpLbs (request {NHC.requestHeaders = request_headers, cookieJar = Nothing}) manager
            (responseBody response) @?= "key2=value2"
        it "Response cookie jar is nothing when request cookie jar is nothing" $ withApp app $ \port -> do
            request <- parseUrlThrow $ concat ["http://127.0.0.1:", show port, "/cookies"]
            manager <- newManager tlsManagerSettings
            response <- httpLbs (request {cookieJar = Nothing}) manager
            (responseCookieJar response) @?= mempty
        it "TLS" $ withAppTls app $ \port -> do
            request <- parseUrlThrow $ "https://127.0.0.1:" ++ show port
            let set = mkManagerSettings
                    def
                        { settingDisableCertificateValidation = True
                        }
                    Nothing
            manager <- newManager set
            response <- httpLbs request manager
            responseBody response @?= "homepage"
    describe "manager" $ do
        it "closes all connections" $ withApp app $ \port1 -> withApp app $ \port2 -> do
            --FIXME clearSocketsList
            manager <- newManager tlsManagerSettings
            let Just req1 = parseUrlThrow $ "http://127.0.0.1:" ++ show port1
            let Just req2 = parseUrlThrow $ "http://127.0.0.1:" ++ show port2
            runResourceT $ do
                _res1a <- http req1 manager
                _res1b <- http req1 manager
                _res2 <- http req2 manager
                return ()
            --FIXME requireAllSocketsClosed
    describe "http" $ do
        it "response body" $ withApp app $ \port -> do
            manager <- newManager tlsManagerSettings
            req <- parseUrlThrow $ "http://127.0.0.1:" ++ show port
            runResourceT $ do
                res1 <- http req manager
                bss <- runConduit $ responseBody res1 .| CL.consume
                res2 <- httpLbs req manager
                liftIO $ L.fromChunks bss `shouldBe` responseBody res2
    describe "DOS protection" $ do
        it "overlong headers" $ overLongHeaders $ \port -> do
            manager <- newManager tlsManagerSettings
            let Just req1 = parseUrlThrow $ "http://127.0.0.1:" ++ show port
            res1 <- try $ runResourceT $ http req1 manager
            case res1 of
              Left e -> show (e :: SomeException) @?= show (HttpExceptionRequest req1 OverlongHeaders)
              _ -> error "Shouldn't have worked"
        it "not overlong headers" $ notOverLongHeaders $ \port -> do
            manager <- newManager tlsManagerSettings
            let Just req1 = parseUrlThrow $ "http://127.0.0.1:" ++ show port
            _ <- httpLbs req1 manager
            return ()
    describe "redirects" $ do
        it "doesn't double escape" $ redir $ \port -> do
            manager <- newManager tlsManagerSettings
            let go (encoded, final) = do
                    let Just req1 = parseUrlThrow $ concat ["http://127.0.0.1:", show port, "/redir/", encoded]
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
            let Just req = parseUrlThrow $ concat ["http://127.0.0.1:", show port, "/infredir/0"]
            let go (res, i) = liftIO $ responseBody res @?= (L8.pack $ show i)
            manager <- newManager tlsManagerSettings
            E.catch (void $ runResourceT $ http req{redirectCount=5} manager)
              $ \e ->
                    case e of
                        HttpExceptionRequest _ (TooManyRedirects redirs) ->
                            mapM_ go (zip redirs [5,4..0 :: Int])
                        _ -> error $ show e
    describe "chunked request body" $ do
        it "works" $ echo $ \port -> do
            manager <- newManager tlsManagerSettings
            let go bss = do
                    let Just req1 = parseUrlThrow $ "POST http://127.0.0.1:" ++ show port
                        src = sourceList bss
                        lbs = L.fromChunks bss
                    res <- httpLbs req1
                        { requestBody = requestBodySourceChunked src
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
            req <- parseUrlThrow $ "http://127.0.0.1:" ++ show port
            manager <- newManager tlsManagerSettings
            res <- httpLbs req manager
            liftIO $ do
                Network.HTTP.Conduit.responseStatus res `shouldBe` status200
                responseBody res `shouldBe` "foo"

    describe "response body too short" $ do
        it "throws an exception" $ wrongLength $ \port -> do
            req <- parseUrlThrow $ "http://127.0.0.1:" ++ show port
            manager <- newManager tlsManagerSettings
            eres <- try $ httpLbs req manager
            liftIO $ either (Left . (show :: HttpException -> String)) (Right . id) eres
             `shouldBe` Left (show $ HttpExceptionRequest req $ ResponseBodyTooShort 50 18)

    describe "chunked response body" $ do
        it "no chunk terminator" $ wrongLengthChunk1 $ \port -> do
            req <- parseUrlThrow $ "http://127.0.0.1:" ++ show port
            manager <- newManager tlsManagerSettings
            eres <- try $ httpLbs req manager
            liftIO $ either (Left . (show :: HttpException -> String)) (Right . id) eres
             `shouldBe` Left (show (HttpExceptionRequest req IncompleteHeaders))
        it "incomplete chunk" $ wrongLengthChunk2 $ \port -> do
            req <- parseUrlThrow $ "http://127.0.0.1:" ++ show port
            manager <- newManager tlsManagerSettings
            eres <- try $ httpLbs req manager
            liftIO $ either (Left . (show :: HttpException -> String)) (Right . id) eres
             `shouldBe` Left (show (HttpExceptionRequest req InvalidChunkHeaders))
        it "invalid chunk" $ invalidChunk $ \port -> do
            req <- parseUrlThrow $ "http://127.0.0.1:" ++ show port
            manager <- newManager tlsManagerSettings
            eres <- try $ httpLbs req manager
            liftIO $ either (Left . (show :: HttpException -> String)) (Right . id) eres
             `shouldBe` Left (show (HttpExceptionRequest req InvalidChunkHeaders))

        it "missing header" $ rawApp
          "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n4\r\nabcd\r\n\r\n\r\n"
          $ \port -> do
            req <- parseUrlThrow $ "http://127.0.0.1:" ++ show port
            manager <- newManager tlsManagerSettings
            eres <- try $ httpLbs req manager
            liftIO $ either (Left . (show :: HttpException -> String)) (Right . id) eres
             `shouldBe` Left (show (HttpExceptionRequest req InvalidChunkHeaders))

        it "junk header" $ rawApp
          "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n4\r\nabcd\r\njunk\r\n\r\n"
          $ \port -> do
            req <- parseUrlThrow $ "http://127.0.0.1:" ++ show port
            manager <- newManager tlsManagerSettings
            eres <- try $ httpLbs req manager
            liftIO $ either (Left . (show :: HttpException -> String)) (Right . id) eres
             `shouldBe` Left (show (HttpExceptionRequest req InvalidChunkHeaders))

    describe "redirect" $ do
        it "ignores large response bodies" $ do
            let app' port req =
                    case pathInfo req of
                        ["foo"] -> return $ responseLBS status200 [] "Hello World!"
                        _ -> return $ responseSource status301 [("location", S8.pack $ "http://127.0.0.1:" ++ show port ++ "/foo")] $ forever $ yield $ Chunk $ fromByteString "hello\n"
            manager <- newManager tlsManagerSettings
            withApp' app' $ \port -> do
                req <- liftIO $ parseUrlThrow $ "http://127.0.0.1:" ++ show port
                res <- httpLbs req manager
                liftIO $ do
                    Network.HTTP.Conduit.responseStatus res `shouldBe` status200
                    responseBody res `shouldBe` "Hello World!"
    describe "multipart/form-data" $ do
        it "formats correctly" $ do
            let bd = "---------------------------190723902820679116301912680260"
            (RequestBodyStream _ givesPopper) <- renderParts bd
                [partBS "email" ""
                ,partBS "parent_id" "70488"
                ,partBS "captcha" ""
                ,partBS "homeboard" "0chan.hk"
                ,partBS "text" $ TE.encodeUtf8 ">>72127\r\nМы работаем над этим."
                ,partFileSource "upload" "nyan.gif"
                ]
            ires <- I.newIORef S.empty
            let loop front popper = do
                    bs <- popper
                    if S.null bs
                        then I.writeIORef ires $ S.concat $ front []
                        else loop (front . (bs:)) popper
            givesPopper $ loop id
            mfd <- I.readIORef ires
            exam <- S.readFile "multipart-example.bin"
            mfd @?= exam

    describe "HTTP/1.0" $ do
        it "BaseHTTP" $ do
            let baseHTTP app' = do
                    _ <- runConduit $ appSource app' .| await
                    runConduit $ yield "HTTP/1.0 200 OK\r\n\r\nThis is it!" .| appSink app'
            manager <- newManager tlsManagerSettings
            withCApp baseHTTP $ \port -> do
                req <- liftIO $ parseUrlThrow $ "http://127.0.0.1:" ++ show port
                res1 <- httpLbs req manager
                res2 <- httpLbs req manager
                liftIO $ res1 @?= res2

    describe "hostAddress" $ do
        it "overrides host" $ withApp app $ \port -> do
            req' <- parseUrlThrow $ "http://example.com:" ++ show port
            let req = req' { hostAddress = Just 0x0100007f } -- 127.0.0.1
            manager <- newManager tlsManagerSettings
            res <- httpLbs req manager
            responseBody res @?= "homepage for example.com"

    describe "managerResponseTimeout" $ do
        it "works" $ withApp app $ \port -> do
            req1 <- parseUrlThrow $ "http://localhost:" ++ show port
            let req2 = req1 { responseTimeout = responseTimeoutMicro 5000000 }
            man <- newManager tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro 1 }
            eres1 <- try $ httpLbs req1 { NHC.path = "/delayed" } man
            case eres1 of
                Left (HttpExceptionRequest _ ConnectionTimeout{}) -> return ()
                _ -> error "Did not time out"
            _ <- httpLbs req2 man
            return ()

    describe "delayed body" $ do
        it "works" $ withApp app $ \port -> do
            req <- parseUrlThrow $ "http://localhost:" ++ show port ++ "/delayed"
            man <- newManager tlsManagerSettings
            _ <- runResourceT $ http req man
            return ()

    it "reuse/connection close tries again" $ do
        withAppSettings (setTimeout 1) (const app) $ \port -> do
            req <- parseUrlThrow $ "http://localhost:" ++ show port
            man <- newManager tlsManagerSettings
            res1 <- httpLbs req man
            threadDelay 3000000
            res2 <- httpLbs req man
            let f res = res
                    { NHC.responseHeaders = filter (not . isDate) (NHC.responseHeaders res)
                    }
                isDate ("date", _) = True
                isDate _ = False
            f res2 `shouldBe` f res1

    it "setQueryString" $ do
        ref <- I.newIORef undefined
        let app' req = do
                I.writeIORef ref $ Wai.queryString req
                return $ responseLBS status200 [] ""
        withApp app' $ \port -> do
            let qs =
                    [ ("foo", Just "bar")
                    , (TE.encodeUtf8 "שלום", Just "hola")
                    , ("noval", Nothing)
                    ]
            man <- newManager tlsManagerSettings
            req <- parseUrlThrow $ "http://localhost:" ++ show port
            _ <- httpLbs (setQueryString qs req) man
            res <- I.readIORef ref
            res `shouldBe` qs

    describe "Simple.JSON" $ do
        it "normal" $ jsonApp $ \port -> do
            req <- parseUrlThrow $ "http://localhost:" ++ show port
            value <- Simple.httpJSON req
            responseBody value `shouldBe` jsonValue
        it "trailing whitespace" $ jsonApp $ \port -> do
            req <- parseUrlThrow $ "http://localhost:" ++ show port ++ "/trailing"
            value <- Simple.httpJSON req
            responseBody value `shouldBe` jsonValue

    it "RequestBodyIO" $ echo $ \port -> do
        manager <- newManager tlsManagerSettings
        let go bss = withSystemTempFile "request-body-io" $ \tmpfp tmph -> do
                liftIO $ do
                    mapM_ (S.hPutStr tmph) bss
                    hClose tmph

                let Just req1 = parseUrlThrow $ "POST http://127.0.0.1:" ++ show port
                    lbs = L.fromChunks bss
                res <- httpLbs req1
                    { requestBody = RequestBodyIO (streamFile tmpfp)
                    } manager
                liftIO $ Network.HTTP.Conduit.responseStatus res @?= status200
                let ts = S.concat . L.toChunks
                liftIO $ ts (responseBody res) @?= ts lbs
        mapM_ go
            [ ["hello", "world"]
            , replicate 500 "foo\003\n\r"
            ]

withCApp :: (Data.Conduit.Network.AppData -> IO ()) -> (Int -> IO ()) -> IO ()
withCApp app' f = do
    port <- getPort
    baton <- newEmptyMVar
    let start = putMVar baton ()
#if MIN_VERSION_conduit(1,1,0)
        settings :: ServerSettings
        settings = setAfterBind (const start) (serverSettings port "*")
#else
        settings :: ServerSettings IO
        settings = (serverSettings port "*" :: ServerSettings IO) { serverAfterBind = const start }
#endif
    bracket
        (forkIO $ runTCPServer settings app' `onException` start)
        killThread
        (const $ takeMVar baton >> f port)

overLongHeaders :: (Int -> IO ()) -> IO ()
overLongHeaders =
    withCApp $ \app' -> runConduit $ src .| appSink app'
  where
    src = sourceList $ "HTTP/1.0 200 OK\r\nfoo: " : repeat "bar"

notOverLongHeaders :: (Int -> IO ()) -> IO ()
notOverLongHeaders = withCApp $ \app' -> do
    runConduit $ appSource app' .| CL.drop 1
    runConduit $ src .| appSink app'
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
    bss <- runConduit $ sourceRequestBody req .| CL.consume
    return $ responseLBS status200 [] $ L.fromChunks bss

noStatusMessage :: (Int -> IO ()) -> IO ()
noStatusMessage =
    withCApp $ \app' -> runConduit $ src .| appSink app'
  where
    src = yield "HTTP/1.0 200\r\nContent-Length: 3\r\n\r\nfoo: barbazbin"

wrongLength :: (Int -> IO ()) -> IO ()
wrongLength =
    withCApp $ \app' -> do
        _ <- runConduit $ appSource app' .| await
        runConduit $ src .| appSink app'
  where
    src = do
        yield "HTTP/1.0 200 OK\r\nContent-Length: 50\r\n\r\n"
        yield "Not quite 50 bytes"

wrongLengthChunk1 :: (Int -> IO ()) -> IO ()
wrongLengthChunk1 =
    withCApp $ \app' -> do
        _ <- runConduit $ appSource app' .| await
        runConduit $ src .| appSink app'
  where
    src = yield "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n4\r\nWiki\r\n"

wrongLengthChunk2 :: (Int -> IO ()) -> IO ()
wrongLengthChunk2 =
    withCApp $ \app' -> do
        _ <- runConduit $ appSource app' .| await
        runConduit $ src .| appSink app'
  where
    src = yield "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n4\r\nWiki\r\n5\r\npedia\r\nE\r\nin\r\n\r\nch\r\n"

invalidChunk :: (Int -> IO ()) -> IO ()
invalidChunk =
    withCApp $ \app' -> do
        _ <- runConduit $ appSource app' .| await
        runConduit $ src .| appSink app'
  where
    src = yield "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n4\r\nabcd\r\ngarbage\r\nef\r\n0\r\n\r\n"

rawApp :: S8.ByteString -> (Int -> IO ()) -> IO ()
rawApp bs =
    withCApp $ \app' -> do
        _ <- runConduit $ appSource app' .| await
        runConduit $ src .| appSink app'
  where
    src = yield bs

jsonApp :: (Int -> IO ()) -> IO ()
jsonApp = withApp $ \req -> return $ responseLBS
    status200
    [ ("Content-Type", "application/json")
    ] $
    case pathInfo req of
      [] -> A.encode jsonValue
      ["trailing"] -> L.append (A.encode jsonValue) "   \n\r\n\t  "
      x -> error $ "unsupported: " ++ show x

jsonValue :: A.Value
jsonValue = A.object
    [ "name" A..= ("Alice" :: String)
    , "age" A..= (35 :: Int)
    ]
