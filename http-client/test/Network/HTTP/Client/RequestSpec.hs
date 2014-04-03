{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.RequestSpec where

import Test.Hspec
import Network.HTTP.Client (parseUrl)

spec :: Spec
spec = describe "case insensitive scheme" $ do
    let succeed s = it s $
            case parseUrl s of
                Nothing -> error "failed"
                Just _ -> return () :: IO ()
        failure s = it s $
            case parseUrl s of
                Nothing -> return () :: IO ()
                Just req -> error $ show req
    succeed "http://example.com"
    succeed "httP://example.com"
    succeed "HttP://example.com"
    succeed "HttPs://example.com"
    failure "ftp://example.com"
