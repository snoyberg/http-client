import Test.Hspec
import qualified Network.HTTP.Client.BodySpec as BodySpec
import qualified Network.HTTP.Client.HeadersSpec as HeadersSpec
import qualified Network.HTTP.Client.ResponseSpec as ResponseSpec
import qualified Network.HTTP.ClientSpec as ClientSpec

main :: IO ()
main = hspec $ do
    BodySpec.spec
    HeadersSpec.spec
    ResponseSpec.spec
    ClientSpec.spec
