{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.ClientSpec where

import qualified Data.ByteString.Char8        as BS
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Text as T
import           Network.HTTP.Client
import           Network.HTTP.Client.Internal
import           Network.HTTP.Types        (status200, found302, status405)
import           Network.HTTP.Types.Status
import           Test.Hspec
import           Control.Applicative       ((<$>))
import           Data.ByteString.Lazy.Char8 () -- orphan instance
import           Data.CaseInsensitive      (mk)
import           Data.Aeson                (FromJSON, Object, eitherDecode)
import           GHC.Generics              (Generic)

main :: IO ()
main = hspec spec

newtype HeadersResponse = HeadersResponse {
  headers :: Object
} deriving (Show, Generic)

instance FromJSON HeadersResponse

spec :: Spec
spec = describe "Client" $ do
    it "works" $ do
        req <- parseUrlThrow "http://httpbin.org/"
        man <- newManager defaultManagerSettings
        res <- httpLbs req man
        responseStatus res `shouldBe` status200

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

    describe "redirects" $ do
        it "follows redirects" $ do
            req <- parseRequest "http://httpbin.org/redirect-to?url=http://httpbin.org"
            man <- newManager defaultManagerSettings
            res <- httpLbs req man
            responseStatus res `shouldBe` status200

        it "allows to disable redirect following" $ do
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

        it "redirectCount" $ do
            let modify req = return req { redirectCount = 0 }
                settings = defaultManagerSettings { managerModifyRequest = modify }
            man <- newManager settings
            response <- httpLbs "http://httpbin.org/redirect-to?url=foo" man
            responseStatus response `shouldBe` found302

        it "isn't applied n+1 times" $ do
            let
              modify req = do
                let headerKey = mk . BS.pack $ "X-Test-" ++ show (redirectCount req)
                return req {
                  requestHeaders = (headerKey, "test") : requestHeaders req,
                  redirectCount = redirectCount req + 1
                }
              settings = defaultManagerSettings { managerModifyRequest = modify }
            man <- newManager settings
            response <- httpLbs "http://httpbin.org/headers" man
            case eitherDecode (responseBody response) :: Either String HeadersResponse of
              Left _ -> fail $ "Decoding failed. Response: " ++ show response
              Right (HeadersResponse headers) -> do
                let
                  headerKey :: Int -> T.Text
                  headerKey = T.pack . (++) "X-Test-" . show
                  defRedirectCount = redirectCount defaultRequest
                HashMap.lookup (headerKey defRedirectCount) headers `shouldBe` Just "test"
                HashMap.lookup (headerKey (defRedirectCount + 1)) headers `shouldBe` Nothing
