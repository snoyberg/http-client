{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
import Test.Hspec
import Network.HTTP.Client
import Network.HTTP.Client.OpenSSL
import Network.HTTP.Types
import qualified OpenSSL.Session       as SSL

main :: IO ()
main = withOpenSSL $ hspec $ do
    it "make a TLS connection" $ do
        manager <- newManager $ opensslManagerSettings SSL.context
        withResponse (parseRequest_ "HEAD https://s3.amazonaws.com/hackage.fpcomplete.com/01-index.tar.gz") manager $ \res -> do
            responseStatus res `shouldBe` status200
            lookup "content-type" (responseHeaders res) `shouldBe` Just "application/x-gzip"
#ifdef USE_PROXY
    it "make a TLS connection with proxy" $ do
        manager <- newManager $ opensslManagerSettings SSL.context
        let req = addProxy "localhost" 8080 $
                  parseRequest_ "HEAD https://s3.amazonaws.com/hackage.fpcomplete.com/01-index.tar.gz"
        withResponse req manager $ \res -> do
            responseStatus res `shouldBe` status200
            lookup "content-type" (responseHeaders res) `shouldBe` Just "application/x-gzip"
    it "compare responses without and with proxy" $ do
        manager <- newManager $ opensslManagerSettings SSL.context
        let req = parseRequest_ "GET https://raw.githubusercontent.com/snoyberg/http-client/master/README.md"
        v_org <- withResponse req manager $ \res -> do
          lbsResponse res
        v <- withResponse (addProxy "localhost" 8080 req) manager $ \res -> do
          lbsResponse res
        (responseBody v) `shouldBe` (responseBody v_org)
#endif
