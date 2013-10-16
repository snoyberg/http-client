{-# LANGUAGE RecordWildCards #-}
module Network.HTTP.ClientSpec where

import           Data.Default
import           Network.HTTP.Client
import           Network.HTTP.Client.Manager
import           Network.HTTP.Client.Request
import           Network.HTTP.Client.Types
import           Network.HTTP.Types
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Client" $ do
    it "works" $ do
        req <- parseUrl "http://www.yesodweb.com/"
        man <- newManager defaultManagerSettings
        Response {..} <- httpLbs req man
        responseStatus `shouldBe` status200
