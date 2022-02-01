module Network.HTTP.Client.ConnectionSpec where

import Network.HTTP.Client (strippedHostName)
import Test.Hspec

spec :: Spec
spec = do
    describe "strippedHostName" $ do
        it "passes along a normal domain name" $ do
            strippedHostName "example.com" `shouldBe` "example.com"
        it "passes along an IPv4 address" $ do
            strippedHostName "127.0.0.1" `shouldBe` "127.0.0.1"
        it "strips brackets of an IPv4 address" $ do
            strippedHostName "[::1]" `shouldBe` "::1"
            strippedHostName "[::127.0.0.1]" `shouldBe` "::127.0.0.1"

        describe "pathological cases" $ do
            -- just need to handle these gracefully, it's unclear
            -- what the result should be
            it "doesn't touch future ip address formats" $ do
                strippedHostName "[v2.huh]" `shouldBe` "[v2.huh]"
            it "doesn't strip trailing stuff" $ do
                strippedHostName "[::1]foo" `shouldBe` "[::1]foo"
