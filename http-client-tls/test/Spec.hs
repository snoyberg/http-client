{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Network.Connection
import Network.HTTP.Client
import Network.HTTP.Client.TLS hiding (tlsManagerSettings)
import Network.HTTP.Types
import Control.Monad (join)
import Data.Default
import qualified Network.TLS as TLS

main :: IO ()
main = hspec $ do
    let tlsSettings = def
    -- Since the release of v2.0.0 of the `tls` package , the default value of
    -- the `supportedExtendedMainSecret` parameter `is `RequireEMS`, this means
    -- that all the connections to a server not supporting TLS1.2+EMS will fail.
    -- The badssl.com service does not yet support TLS1.2+EMS connections, so
    -- let's switch to the value `AllowEMS`, ie: TLS1.2 conenctions without EMS.
#if MIN_VERSION_crypton_connection(0,4,0)
            {settingClientSupported = def {TLS.supportedExtendedMainSecret = TLS.AllowEMS}}
#endif

    let tlsManagerSettings = mkManagerSettings tlsSettings Nothing

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

    -- https://github.com/snoyberg/http-client/issues/289
    it "accepts TLS settings" $ do
        let
          tlsSettings' = tlsSettings
            { settingDisableCertificateValidation = True
            , settingDisableSession = False
            , settingUseServerName = False
            }
          socketSettings = Nothing
          managerSettings = mkManagerSettings tlsSettings' socketSettings
        manager <- newTlsManagerWith managerSettings
        let url = "https://wrong.host.badssl.com"
        request <- parseRequest url
        response <- httpNoBody request manager
        responseStatus response `shouldBe` status200

    it "global supports TLS" $ do
        manager <- getGlobalManager
        request <- parseRequest "https://httpbin.org"
        response <- httpNoBody request manager
        responseStatus response `shouldBe` status200
