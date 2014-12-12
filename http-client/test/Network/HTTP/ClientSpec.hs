{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.ClientSpec where

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
        req <- parseUrl "http://www.yesodweb.com/"
        man <- newManager defaultManagerSettings
        res <- httpLbs req man
        responseStatus res `shouldBe` status200

    it "managerModifyRequest" $ do
        let modify req = return req { port = 80 }
            settings = defaultManagerSettings { managerModifyRequest = modify }
        withManager settings $ \man -> do
            res <- httpLbs "http://httpbin.org:1234" man
            responseStatus res `shouldBe` status200
