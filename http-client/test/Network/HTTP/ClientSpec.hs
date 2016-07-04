{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.ClientSpec where

import           Network                   (withSocketsDo)
import           Network.HTTP.Client
import           Network.HTTP.Types        (status200, status405)
import           Test.Hspec
import           Data.ByteString.Lazy.Char8 () -- orphan instance

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Client" $ do
    it "works" $ withSocketsDo $ do
        req <- parseUrlThrow "http://httpbin.org/"
        man <- newManager defaultManagerSettings
        res <- httpLbs req man
        responseStatus res `shouldBe` status200

    describe "method in URL" $ do
        it "success" $ withSocketsDo $ do
            req <- parseUrlThrow "POST http://httpbin.org/post"
            man <- newManager defaultManagerSettings
            res <- httpLbs req man
            responseStatus res `shouldBe` status200

        it "failure" $ withSocketsDo $ do
            req <- parseRequest "PUT http://httpbin.org/post"
            man <- newManager defaultManagerSettings
            res <- httpLbs req man
            responseStatus res `shouldBe` status405

    it "managerModifyRequest" $ do
        let modify req = return req { port = 80 }
            settings = defaultManagerSettings { managerModifyRequest = modify }
        man <- newManager settings
        res <- httpLbs "http://httpbin.org:1234" man
        responseStatus res `shouldBe` status200

    it "managerModifyRequestCheckStatus" $ do
        let modify req = return req { checkResponse = \_ _ -> error "some exception" }
            settings = defaultManagerSettings { managerModifyRequest = modify }
        man <- newManager settings
        httpLbs "http://httpbin.org" man `shouldThrow` anyException
