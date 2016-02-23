{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.RequestSpec where

import Blaze.ByteString.Builder (fromByteString)
import Control.Applicative ((<$>))
import Control.Monad (join, forM_, (<=<))
import Data.IORef
import Data.Maybe (isJust, fromMaybe, fromJust)
import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Client (parseUrl, requestHeaders, applyBasicProxyAuth)
import Network.HTTP.Client.Internal
import Network.URI (URI(..), URIAuth(..), parseURI) 
import Test.Hspec

spec :: Spec
spec = do
    describe "case insensitive scheme" $ do
        forM_ ["http://example.com", "httP://example.com", "HttP://example.com", "HttPs://example.com"] $ \url ->
            it url $ case parseUrl url of
                Nothing -> error "failed"
                Just _ -> return () :: IO ()
        forM_ ["ftp://example.com"] $ \url ->
            it url $ case parseUrl url of
                Nothing -> return () :: IO ()
                Just req -> error $ show req

    describe "authentication in url" $ do
      it "passes validation" $ do
        case parseUrl "http://agent:topsecret@example.com" of
          Nothing -> error "failed"
          Just _ -> return () :: IO ()

      it "add username/password to headers section" $ do
        let request = parseUrl "http://user:pass@example.com"
            field = join $ lookup "Authorization" . requestHeaders <$> request
            requestHostnameWithoutAuth = "example.com"
        (uriRegName $ fromJust $ uriAuthority $ getUri $ fromJust request) `shouldBe` requestHostnameWithoutAuth
        field `shouldSatisfy` isJust
        field `shouldBe` Just "Basic dXNlcjpwYXNz"

    describe "applyBasicProxyAuth" $ do
        let request = applyBasicProxyAuth "user" "pass" <$> parseUrl "http://example.org"
            field   = join $ lookup "Proxy-Authorization" . requestHeaders <$> request
        it "Should add a proxy-authorization header" $ do
            field `shouldSatisfy` isJust
        it "Should add a proxy-authorization header with the specified username and password." $ do
            field `shouldBe` Just "Basic dXNlcjpwYXNz"

    describe "extract credentials from a URI" $ do
        let username = return . fst <=< extractBasicAuthInfo <=< parseURI
            password = return . snd <=< extractBasicAuthInfo <=< parseURI
        it "fetches non-empty username before the first ':'" $ do
            username "http://agent:secret@example.com" `shouldBe` Just "agent"

        it "after ':' is considered password" $ do
            password "http://agent007:shakenNotStirred@example.com" `shouldBe` Just "shakenNotStirred"

        it "decodes username special characters per RFC3986" $ do
            username "http://%2F%3F%23%5B%5D%21%24%26%27%28%29%2A%2B%2C%3B%3D:therealpassword@example.com" `shouldBe` Just "/?#[]!$&'()*+,;="

        it "decodes password special characters per RFC3986" $ do
            password "http://therealusername:%3F%23%5B%5D%21%24%26%27%28%29%2A%2B%2C%3B%3D%2F@example.com" `shouldBe` Just "?#[]!$&'()*+,;=/"

        it "no auth is empty" $ do
            username "http://example.com" `shouldBe` Nothing
            password "http://example.com" `shouldBe` Nothing

    describe "requestBuilder" $ do
        it "sends the full request, combining headers and body in the non-streaming case" $ do
            let Just req  = parseUrl "http://localhost"
            let      req' = req { method = "PUT", path = "foo" }
            (conn, out, _) <- dummyConnection []
            forM_ (bodies `zip` out1) $ \(b, o) -> do
                cont <- requestBuilder (req' { requestBody = b } ) conn
                (const "<IO ()>" <$> cont) `shouldBe` Nothing
                out >>= (`shouldBe` o)

        it "sends only headers and returns an action for the body on 'Expect: 100-continue'" $ do
            let Just req  = parseUrl "http://localhost"
            let      req' = req { requestHeaders = [("Expect", "100-continue")]
                                , method = "PUT"
                                , path = "foo"
                                }
            (conn, out, _) <- dummyConnection []
            forM_ (bodies `zip` out2) $ \(b, (h, o)) -> do
                cont <- requestBuilder (req' { requestBody = b } ) conn
                out >>= (`shouldBe` [h, ""])
                fromMaybe (return ()) cont
                out >>= (`shouldBe` o)
      where
        bodies = [ RequestBodyBS "data"
                 , RequestBodyLBS "data"
                 , RequestBodyBuilder 4 (fromByteString "data")
                 , RequestBodyStream 4 (popper ["data"] >>=)
                 , RequestBodyStreamChunked (popper ["data"] >>=)
                 ]

        out1 = [ [nonChunked <> "\r\ndata"]
               , [nonChunked <> "\r\ndata"]
               , [nonChunked <> "\r\ndata"]
               , [nonChunked <> "\r\n", "", "data"]
               , [chunked <> "\r\n", "", "4\r\ndata\r\n","0\r\n\r\n"]
               ]

        out2 = [ (nonChunked <> "Expect: 100-continue\r\n\r\n", ["data"])
               , (nonChunked <> "Expect: 100-continue\r\n\r\n", ["data"])
               , (nonChunked <> "Expect: 100-continue\r\n\r\n", ["data"])
               , (nonChunked <> "Expect: 100-continue\r\n\r\n", ["data"])
               , (chunked    <> "Expect: 100-continue\r\n\r\n", ["4\r\ndata\r\n","0\r\n\r\n"])
               ]

        nonChunked = "PUT /foo HTTP/1.1\r\nHost: localhost\r\nAccept-Encoding: gzip\r\nContent-Length: 4\r\n"
        chunked    = "PUT /foo HTTP/1.1\r\nHost: localhost\r\nAccept-Encoding: gzip\r\nTransfer-Encoding: chunked\r\n"

        popper dat = do
            r <- newIORef dat
            return . atomicModifyIORef' r $ \xs ->
                case xs of
                    (x:xs') -> (xs', x)
                    [] -> ([], "")
 
