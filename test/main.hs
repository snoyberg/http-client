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
import Control.Concurrent (forkIO, killThread, putMVar, takeMVar, newEmptyMVar)
import Network.HTTP.Types
import Control.Exception.Lifted (try, SomeException, bracket, onException, IOException)
import qualified Data.IORef as I
import qualified Control.Exception as E (catch)
import Network.HTTP.Conduit.ConnInfo
import Network (withSocketsDo)
import Network.Socket (sClose)
import CookieTest (cookieTest)
import Data.Conduit.Network (runTCPServer, serverSettings, HostPreference (..), appSink, appSource, bindPort, serverAfterBind, ServerSettings)
import qualified Data.Conduit.Network
import System.IO.Unsafe (unsafePerformIO)
import Data.Conduit (($$), yield, Flush (Chunk))
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

app :: Application
app req =
    case pathInfo req of
        [] -> return $ responseLBS status200 [] "homepage"
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
        ["infredirrepeat", i'] ->
            let i = read $ T.unpack i' :: Int
            in return $ responseLBS status303
                    [(hLocation, S.append "/infredirrepeat/" $ S8.pack $ show $ i + 1)
                    ,(hContentLength, "2048")]
                    (L8.pack $ take 2048 $ unwords $ repeat $ show i)
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
main = withSocketsDo $ hspec $ do
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
                Response _ _ headers _ <- httpLbs request manager
                let setCookie = mk (fromString "Set-Cookie")
                    (setCookieHeaders, _) = partition ((== setCookie) . fst) headers
                liftIO $ assertBool "response contains a 'set-cookie' header" $ length setCookieHeaders > 0
        it "redirects set cookies" $ withApp app $ \port -> do
            request <- parseUrl $ concat ["http://127.0.0.1:", show port, "/cookie_redir1"]
            withManager $ \manager -> do
                Response _ _ _ body <- httpLbs request manager
                liftIO $ body @?= "nom-nom-nom"
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
                        _ -> return $ ResponseSource status301 [("location", S8.pack $ "http://localhost:" ++ show port ++ "/foo")] $ forever $ yield $ Chunk $ fromByteString "hello\n"
            withApp' app' $ \port -> withManager $ \manager -> do
                req <- parseUrl $ "http://127.0.0.1:" ++ show port
                res <- httpLbs req manager
                liftIO $ do
                    Network.HTTP.Conduit.responseStatus res `shouldBe` status200
                    responseBody res `shouldBe` "Hello World!"

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
