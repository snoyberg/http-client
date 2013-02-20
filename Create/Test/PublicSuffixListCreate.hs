import Network.PublicSuffixList.DataStructure
import Network.PublicSuffixList.Internal.Internal
import Test.HUnit
import           Data.Serialize.Get hiding (getTreeOf)
import           Data.Serialize.Put

testSerializationRoundTrip = TestCase $ assertEqual "Round Trip" dataStructure ds
  where Right ds = runGet getDataStructure serializedDataStructure
        serializedDataStructure = runPut $ putDataStructure dataStructure

main = runTestTT testSerializationRoundTrip
