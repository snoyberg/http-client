{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.RequestSpec where

import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.Maybe (isJust)
import Test.Hspec
import Network.HTTP.Client (parseUrl, requestHeaders, applyBasicProxyAuth)
import Control.Monad (forM_)

spec :: Spec
spec = do
  describe "case insensitive scheme" $ do
    forM_ ["http://example.com", "httP://example.com", "HttP://example.com", "HttPs://example.com"] $ \url ->
      it url $ case parseUrl url of
        Nothing -> error "failed"
        Just _ -> return () :: IO ()
    forM_ ["ftp://example.com"] $ \url ->
      it url $ case parseUrl url of
        Nothing -> return () :: IO ()
        Just req -> error $ show req
  describe "applyBasicProxyAuth" $ do
    let request = applyBasicProxyAuth "user" "pass" <$> parseUrl "http://example.org"
        field   = join $ lookup "Proxy-Authorization" . requestHeaders <$> request
    it "Should add a proxy-authorization header" $ do
      field `shouldSatisfy` isJust
    it "Should add a proxy-authorization header with the specified username and password." $ do
      field `shouldBe` Just "Basic dXNlcjpwYXNz"
