{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Network.HTTP.Client
import Network.HTTP.Client.OpenSSL
import Network.HTTP.Client.Internal
import Network.HTTP.Types
import qualified OpenSSL.Session       as SSL

main :: IO ()
main = withOpenSSL $ hspec $ do
    it "make a TLS connection" $ do
        manager <- newManager $ opensslManagerSettings SSL.context
        withResponse (parseRequest_ "https://httpbin.org/status/418") manager $ \res ->
            responseStatus res `shouldBe` status418

    it "BadSSL: expired" $ do
        manager <- newManager $ opensslManagerSettings SSL.context
        let action = withResponse "https://expired.badssl.com/"
                { checkStatus = \_ _ _ -> Nothing
                } manager (const (pure ()))
        action `shouldThrow` anyException

    it "BadSSL: self-signed" $ do
        manager <- newManager $ opensslManagerSettings SSL.context
        let action = withResponse "https://self-signed.badssl.com/"
                { checkStatus = \_ _ _ -> Nothing
                } manager (const (pure ()))
        action `shouldThrow` anyException

    it "BadSSL: wrong.host" $ do
        manager <- newManager $ opensslManagerSettings SSL.context
        let action = withResponse "https://wrong.host.badssl.com/"
                { checkStatus = \_ _ _ -> Nothing
                } manager (const (pure ()))
        action `shouldThrow` anyException

    it "BadSSL: we do have case-insensitivity though" $ do
        manager <- newManager $ opensslManagerSettings SSL.context
        withResponse "https://BADSSL.COM"
            { checkStatus = \_ _ _ -> Nothing
            } manager $ \res -> do
            responseStatus res `shouldBe` status200