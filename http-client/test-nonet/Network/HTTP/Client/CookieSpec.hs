{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.CookieSpec where

import           Control.Monad (when)
import           Data.Monoid
import           Data.Time.Clock
import           Network.HTTP.Client.Internal
import           Test.Hspec
import qualified Data.Time                 as DT
import qualified Web.Cookie                as WC

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "CookieSpec" $ do
    it "cookie equality - case insensitive Eq" $ do
      now <- getCurrentTime
      let cookie1 = Cookie "test" "value" now "doMain.Org" "/" now now False False False False
          cookie2 = Cookie "test" "value" now "DOMAIn.ORg" "/" now now False False False False
      cookie1 `shouldSatisfy` (equivCookie cookie2)

    it "domainMatches - case insensitive" $ do
      domainMatches "www.org" "www.org" `shouldBe` True
      domainMatches "wWw.OrG" "Www.oRG" `shouldBe` True
      domainMatches "wxw.OrG" "Www.oRG" `shouldBe` False

    it "domainMatches - case insensitive, partial" $ do
      domainMatches "www.org" "xxx.www.org" `shouldBe` False
      domainMatches "xxx.www.org" "WWW.ORG" `shouldBe` True

    describe "equalCookie vs. equivCookie" $ do
      let make :: IO Cookie
          make = do
            now <- DT.getCurrentTime
            req <- parseRequest "http://www.example.com/path"
            let Just cky = generateCookie (WC.parseSetCookie raw) req now True
                raw = "somename=somevalue.v=1.k=1.d=1590419679.t=u.l=s.u=8b2734ae-9dd1-11ea-bd7f-3bcf5b8d5d2a.r=795e71b5; " <>
                      "Path=/access; Domain=example.com; HttpOnly; Secure"
            return cky

          modifications :: [(String, Cookie -> Cookie, Bool)]
          modifications
              = [ ("cookie_name", \cky -> cky { cookie_name = "othername" }, True)
                , ("cookie_value", \cky -> cky { cookie_value = "othervalue" }, False)
                , ("cookie_expiry_time", \cky -> cky { cookie_expiry_time = DT.addUTCTime 60 $ cookie_expiry_time cky }, False)
                , ("cookie_domain", \cky -> cky { cookie_domain = cookie_domain cky <> ".com" }, True)
                , ("cookie_path", \cky -> cky { cookie_path = cookie_path cky <> "/sub" }, True)
                , ("cookie_creation_time", \cky -> cky { cookie_creation_time = DT.addUTCTime 60 $ cookie_creation_time cky }, False)
                , ("cookie_last_access_time", \cky -> cky { cookie_last_access_time = DT.addUTCTime 60 $ cookie_last_access_time cky }, False)
                , ("cookie_persistent", \cky -> cky { cookie_persistent = not $ cookie_persistent cky }, False)
                , ("cookie_host_only", \cky -> cky { cookie_host_only = not $ cookie_host_only cky }, False)
                , ("cookie_secure_only", \cky -> cky { cookie_secure_only = not $ cookie_secure_only cky }, False)
                , ("cookie_http_only", \cky -> cky { cookie_http_only = not $ cookie_http_only cky }, False)
                ]

          check :: (String, Cookie -> Cookie, Bool) -> Spec
          check (msg, f, countsForEquiv) = it msg $ do
            cky <- make
            cky `equalCookie` f cky `shouldBe` False
            when countsForEquiv $ cky `equivCookie` f cky `shouldBe` False

      check `mapM_` modifications
