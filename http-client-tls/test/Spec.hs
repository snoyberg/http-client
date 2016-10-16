{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Control.Monad (join)

main :: IO ()
main = hspec $ do
    it "make a TLS connection" $ do
        manager <- newManager tlsManagerSettings
        withResponse "https://httpbin.org/status/418" manager $ \res ->
            responseStatus res `shouldBe` status418

    it "digest authentication" $ do
        man <- newManager defaultManagerSettings
        req <- join $ applyDigestAuth
            "user"
            "passwd"
            "http://httpbin.org/digest-auth/qop/user/passwd"
            man
        response <- httpNoBody req man
        responseStatus response `shouldBe` status200

    it "incorrect digest authentication" $ do
        man <- newManager defaultManagerSettings
        join (applyDigestAuth "user" "passwd" "http://httpbin.org/" man)
            `shouldThrow` \(DigestAuthException _ _ det) ->
                det == UnexpectedStatusCode

    it "BadSSL: expired" $ do
        manager <- newManager tlsManagerSettings
        let action = withResponse "https://expired.badssl.com/" manager (const (return ()))
        action `shouldThrow` anyException

    it "BadSSL: self-signed" $ do
        manager <- newManager tlsManagerSettings
        let action = withResponse "https://self-signed.badssl.com/" manager (const (return ()))
        action `shouldThrow` anyException

    it "BadSSL: wrong.host" $ do
        manager <- newManager tlsManagerSettings
        let action = withResponse "https://wrong.host.badssl.com/" manager (const (return ()))
        action `shouldThrow` anyException

    it "BadSSL: we do have case-insensitivity though" $ do
        manager <- newManager $ tlsManagerSettings
        withResponse "https://BADSSL.COM" manager $ \res ->
            responseStatus res `shouldBe` status200