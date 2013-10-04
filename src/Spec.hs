module Spec where

import Test.Hspec
import qualified Network.HTTP.Client.ResponseBodySpec as ResponseBodySpec
import qualified Network.HTTP.Client.ResponseParserSpec as ResponseParserSpec

main :: IO ()
main = hspec $ do
    ResponseBodySpec.spec
    ResponseParserSpec.spec
