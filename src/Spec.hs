module Spec where

import Test.Hspec
import qualified Network.HTTP.Client.BodySpec as BodySpec
import qualified Network.HTTP.Client.HeadersSpec as HeadersSpec
import qualified Network.HTTP.Client.ResponseSpec as ResponseSpec

main :: IO ()
main = hspec $ do
    BodySpec.spec
    HeadersSpec.spec
    ResponseSpec.spec
