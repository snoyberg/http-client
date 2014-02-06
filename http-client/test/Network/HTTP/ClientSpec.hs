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
