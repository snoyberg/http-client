{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.ClientSpec where

import           Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8        as BS
import           Network.HTTP.Client
import           Network.HTTP.Client.Internal
import           Network.HTTP.Types        (status200, found302, status405)
import           Network.HTTP.Types.Status
import qualified Network.Socket               as NS
import           Test.Hspec
import           Control.Applicative       ((<$>))
import           Data.ByteString.Lazy.Char8 () -- orphan instance
import           System.Mem (performGC)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Client" $ do
    it "works" $ do
        req <- parseUrlThrow "http://httpbin.org/"
        man <- newManager defaultManagerSettings
        res <- httpLbs req man
        responseStatus res `shouldBe` status200

    -- Test the failure condition described in https://github.com/snoyberg/http-client/issues/489
    it "keeps connection alive long enough" $ do
        req <- parseUrlThrow "http://httpbin.org/"
        man <- newManager defaultManagerSettings
        res <- responseOpen req man
        responseStatus res `shouldBe` status200
        let
            getChunk = responseBody res
            drainAll = do
                chunk <- getChunk
                if BS.null chunk then pure () else drainAll

        -- The returned `BodyReader` used to not contain a reference to the `Managed Connection`,
        -- only to the extracted connection and to the release action. Therefore, triggering a GC
        -- would close the connection even though we were not done reading.
        performGC
        -- Not ideal, but weak finalizers run on a separate thread, so it's racing with our drain
        -- call
        threadDelay 500000

        drainAll
        -- Calling `responseClose res` here prevents the early collection from happening in this
        -- test, but in a larger production application that did involve a `responseClose`, it still
        -- occurred.

    describe "method in URL" $ do
        it "success" $ do
            req <- parseUrlThrow "POST http://httpbin.org/post"
            man <- newManager defaultManagerSettings
            res <- httpLbs req man
            responseStatus res `shouldBe` status200

        it "failure" $ do
            req <- parseRequest "PUT http://httpbin.org/post"
            man <- newManager defaultManagerSettings
            res <- httpLbs req man
            responseStatus res `shouldBe` status405
    describe "bearer auth" $ do
        it "success" $ do
            initialReq <- parseUrlThrow "http://httpbin.org/bearer"
            let finalReq = applyBearerAuth "token" initialReq
            man <- newManager defaultManagerSettings
            res <- httpLbs finalReq man
            responseStatus res `shouldBe` status200
        it "failure" $ do
            req <- parseRequest "http://httpbin.org/bearer"
            man <- newManager defaultManagerSettings
            res <- httpLbs req man
            responseStatus res `shouldBe` status401

    describe "redirects" $ do
        xit "follows redirects" $ do
            req <- parseRequest "http://httpbin.org/redirect-to?url=http://httpbin.org"
            man <- newManager defaultManagerSettings
            res <- httpLbs req man
            responseStatus res `shouldBe` status200

        xit "allows to disable redirect following" $ do
            req <- (\ r -> r{ redirectCount = 0 }) <$>
              parseRequest "http://httpbin.org/redirect-to?url=http://httpbin.org"
            man <- newManager defaultManagerSettings
            res <- httpLbs req man
            responseStatus res `shouldBe` found302

    context "managerModifyResponse" $ do
      it "allows to modify the response status code" $ do
        let modify :: Response BodyReader -> IO (Response BodyReader)
            modify res = do
              return res {
                responseStatus = (responseStatus res) {
                  statusCode = 201
                }
              }
            settings = defaultManagerSettings { managerModifyResponse = modify }
        man <- newManager settings
        res <- httpLbs "http://httpbin.org" man
        (statusCode.responseStatus) res `shouldBe` 201

      it "modifies the response body" $ do
        let modify :: Response BodyReader -> IO (Response BodyReader)
            modify res = do
              reader <- constBodyReader [BS.pack "modified response body"]
              return res {
                responseBody = reader
              }
            settings = defaultManagerSettings { managerModifyResponse = modify }
        man <- newManager settings
        res <- httpLbs "http://httpbin.org" man
        responseBody res `shouldBe` "modified response body"

    context "managerModifyRequest" $ do
        it "port" $ do
            let modify req = return req { port = 80 }
                settings = defaultManagerSettings { managerModifyRequest = modify }
            man <- newManager settings
            res <- httpLbs "http://httpbin.org:1234" man
            responseStatus res `shouldBe` status200

        it "checkResponse" $ do
            let modify req = return req { checkResponse = \_ _ -> error "some exception" }
                settings = defaultManagerSettings { managerModifyRequest = modify }
            man <- newManager settings
            httpLbs "http://httpbin.org" man `shouldThrow` anyException

        xit "redirectCount" $ do
            let modify req = return req { redirectCount = 0 }
                settings = defaultManagerSettings { managerModifyRequest = modify }
            man <- newManager settings
            response <- httpLbs "http://httpbin.org/redirect-to?url=foo" man
            responseStatus response `shouldBe` found302

    -- skipped because CI doesn't have working IPv6
    xdescribe "raw IPV6 address as hostname" $ do
        it "works" $ do
            -- We rely on example.com serving a web page over IPv6.
            -- The request (currently) actually ends up as 404 due to
            -- virtual hosting, but we just care that the networking
            -- side works.
            (addr:_) <- NS.getAddrInfo
                (Just NS.defaultHints { NS.addrFamily = NS.AF_INET6 })
                (Just "example.com")
                (Just "http")
            -- ipv6Port will be of the form [::1]:80, which is good enough
            -- for our purposes; ideally we'd easily get just the ::1.
            let ipv6Port = show $ NS.addrAddress addr
            ipv6Port `shouldStartWith` "["
            req <- parseUrlThrow $ "http://" ++ ipv6Port
            man <- newManager defaultManagerSettings
            _ <- httpLbs (setRequestIgnoreStatus req) man
            return ()
