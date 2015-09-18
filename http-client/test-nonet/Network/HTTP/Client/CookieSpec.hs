{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.CookieSpec where

import           Data.Time.Clock
import           Network.HTTP.Client.Internal
import           Network.HTTP.Types
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "CookieSpec" $ do
    it "cookie equality - case insensitive Eq" $ do
      now <- getCurrentTime
      let cookie1 = Cookie "test" "value" now "doMain.Org" "/" now now False False False False
          cookie2 = Cookie "test" "value" now "DOMAIn.ORg" "/" now now False False False False
      cookie1 `shouldBe` cookie2

    it "domainMatches - case insensitive" $ do
      domainMatches "www.org" "www.org" `shouldBe` True
      domainMatches "wWw.OrG" "Www.oRG" `shouldBe` True
      domainMatches "wxw.OrG" "Www.oRG" `shouldBe` False

    it "domainMatches - case insensitive, partial" $ do
      domainMatches "www.org" "xxx.www.org" `shouldBe` False
      domainMatches "xxx.www.org" "WWW.ORG" `shouldBe` True
