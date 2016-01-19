{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.ClientSpec where

import           Control.Exception         (toException)
import           Network                   (withSocketsDo)
import           Network.HTTP.Client
import           Network.HTTP.Types        (status200)
import           Test.Hspec
import           Data.ByteString.Lazy.Char8 () -- orphan instance

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Client" $ do
    it "works" $ withSocketsDo $ do
        req <- parseUrl "http://httpbin.org/"
        man <- newManager defaultManagerSettings
        res <- httpLbs req man
        responseStatus res `shouldBe` status200

    it "managerModifyRequest" $ do
        let modify req = return req { port = 80 }
            settings = defaultManagerSettings { managerModifyRequest = modify }
        withManager settings $ \man -> do
            res <- httpLbs "http://httpbin.org:1234" man
            responseStatus res `shouldBe` status200

    it "managerModifyRequestCheckStatus" $ do
        let modify req = return req { checkStatus = \s hs cj -> Just $ toException $ StatusCodeException s hs cj }
            settings = defaultManagerSettings { managerModifyRequest = modify }
        withManager settings $ \man ->
            httpLbs "http://httpbin.org" man `shouldThrow` anyException
