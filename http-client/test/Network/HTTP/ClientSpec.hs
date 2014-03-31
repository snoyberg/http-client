{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.ClientSpec where

import           Network.HTTP.Client
import           Network.HTTP.Types (status200)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Client" $ do
    it "works" $ do
        req <- parseUrl "http://www.yesodweb.com/"
        man <- newManager defaultManagerSettings
        res <- httpLbs req man
        responseStatus res `shouldBe` status200
    describe "fails on empty hostnames #40" $ do
        let test url = it url $ do
                req <- parseUrl url
                man <- newManager defaultManagerSettings
                _ <- httpLbs req man `shouldThrow` \e ->
                    case e of
                        InvalidDestinationHost "" -> True
                        _ -> False
                return ()
        mapM_ test ["http://", "https://", "http://:8000", "https://:8001"]
