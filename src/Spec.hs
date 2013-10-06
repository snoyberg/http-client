module Spec where

import Test.Hspec
import qualified Network.HTTP.Client.BodySpec as BodySpec
import qualified Network.HTTP.Client.HeadersSpec as HeadersSpec

main :: IO ()
main = hspec $ do
    BodySpec.spec
    HeadersSpec.spec
