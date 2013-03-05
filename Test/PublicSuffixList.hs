{-# LANGUAGE OverloadedStrings #-}

import           Data.Char
import           Data.Maybe
import           Data.Serialize.Get hiding (getTreeOf)
import           Data.Serialize.Put
import qualified Data.Text                              as T
import           Debug.Trace
import           Network.PublicSuffixList.DataStructure
import qualified Network.PublicSuffixList.Lookup        as L
import           Network.PublicSuffixList.Serialize
import           System.Exit
import           Test.HUnit
import           Text.IDNA


isSuffix' :: T.Text -> Bool
isSuffix' = L.isSuffix . T.intercalate "." . map (fromJust . toASCII False True . T.map toLower) . T.split (== '.')

-- Derived from http://mxr.mozilla.org/mozilla-central/source/netwerk/test/unit/data/test_psl.txt
-- on October 21, 2012
hunittests :: Test
hunittests = TestList [
  --TestCase $ assertEqual " 1" True  $ isSuffix' "",
  TestCase $ assertEqual " 2" True  $ isSuffix' "COM",
  TestCase $ assertEqual " 3" False $ isSuffix' "example.COM",
  TestCase $ assertEqual " 4" False $ isSuffix' "WwW.example.COM",
  --TestCase $ assertEqual " 5" True  $ isSuffix' ".com",
  --TestCase $ assertEqual " 6" True  $ isSuffix' ".example",
  --TestCase $ assertEqual " 7" True  $ isSuffix' ".example.com",
  --TestCase $ assertEqual " 8" True  $ isSuffix' ".example.example",
  TestCase $ assertEqual " 9" True  $ isSuffix' "example",
  TestCase $ assertEqual "10" False $ isSuffix' "example.example",
  TestCase $ assertEqual "11" False $ isSuffix' "b.example.example",
  TestCase $ assertEqual "12" False $ isSuffix' "a.b.example.example",
  TestCase $ assertEqual "13" True  $ isSuffix' "biz",
  TestCase $ assertEqual "14" False $ isSuffix' "domain.biz",
  TestCase $ assertEqual "15" False $ isSuffix' "b.domain.biz",
  TestCase $ assertEqual "16" False $ isSuffix' "a.b.domain.biz",
  TestCase $ assertEqual "17" True  $ isSuffix' "com",
  TestCase $ assertEqual "18" False $ isSuffix' "example.com",
  TestCase $ assertEqual "19" False $ isSuffix' "b.example.com",
  TestCase $ assertEqual "20" False $ isSuffix' "a.b.example.com",
  TestCase $ assertEqual "21" True  $ isSuffix' "uk.com",
  TestCase $ assertEqual "22" False $ isSuffix' "example.uk.com",
  TestCase $ assertEqual "23" False $ isSuffix' "b.example.uk.com",
  TestCase $ assertEqual "24" False $ isSuffix' "a.b.example.uk.com",
  TestCase $ assertEqual "25" False $ isSuffix' "test.ac",
  TestCase $ assertEqual "26" True  $ isSuffix' "cy",
  TestCase $ assertEqual "27" True  $ isSuffix' "c.cy",
  TestCase $ assertEqual "28" False $ isSuffix' "b.c.cy",
  TestCase $ assertEqual "29" False $ isSuffix' "a.b.c.cy",
  TestCase $ assertEqual "30" True  $ isSuffix' "jp",
  TestCase $ assertEqual "31" False $ isSuffix' "test.jp",
  TestCase $ assertEqual "32" False $ isSuffix' "www.test.jp",
  TestCase $ assertEqual "33" True  $ isSuffix' "ac.jp",
  TestCase $ assertEqual "34" False $ isSuffix' "test.ac.jp",
  TestCase $ assertEqual "35" False $ isSuffix' "www.test.ac.jp",
  TestCase $ assertEqual "36" True  $ isSuffix' "kyoto.jp",
  TestCase $ assertEqual "37" False $ isSuffix' "test.kyoto.jp",
  TestCase $ assertEqual "38" True  $ isSuffix' "ide.kyoto.jp",
  TestCase $ assertEqual "39" False $ isSuffix' "b.ide.kyoto.jp",
  TestCase $ assertEqual "30" False $ isSuffix' "a.b.ide.kyoto.jp",
  TestCase $ assertEqual "41" True  $ isSuffix' "c.kobe.jp",
  TestCase $ assertEqual "42" False $ isSuffix' "b.c.kobe.jp",
  TestCase $ assertEqual "43" False $ isSuffix' "a.b.c.kobe.jp",
  TestCase $ assertEqual "44" False $ isSuffix' "city.kobe.jp",
  TestCase $ assertEqual "45" False $ isSuffix' "www.city.kobe.jp",
  TestCase $ assertEqual "46" True  $ isSuffix' "om",
  TestCase $ assertEqual "47" True  $ isSuffix' "test.om",
  TestCase $ assertEqual "48" False $ isSuffix' "b.test.om",
  TestCase $ assertEqual "49" False $ isSuffix' "a.b.test.om",
  TestCase $ assertEqual "40" False $ isSuffix' "songfest.om",
  TestCase $ assertEqual "51" False $ isSuffix' "www.songfest.om",
  TestCase $ assertEqual "52" True  $ isSuffix' "us",
  TestCase $ assertEqual "53" False $ isSuffix' "test.us",
  TestCase $ assertEqual "54" False $ isSuffix' "www.test.us",
  TestCase $ assertEqual "55" True  $ isSuffix' "ak.us",
  TestCase $ assertEqual "56" False $ isSuffix' "test.ak.us",
  TestCase $ assertEqual "57" False $ isSuffix' "www.test.ak.us",
  TestCase $ assertEqual "58" True  $ isSuffix' "k12.ak.us",
  TestCase $ assertEqual "59" False $ isSuffix' "test.k12.ak.us",
  TestCase $ assertEqual "60" False $ isSuffix' "www.test.k12.ak.us"
  ]

testSerializationRoundTrip = TestCase $ assertEqual "Round Trip" dataStructure ds
  where Right ds = runGet getDataStructure serializedDataStructure
        serializedDataStructure = runPut $ putDataStructure dataStructure

main = do
  counts <- runTestTT $ TestList [TestLabel "Mozilla Tests" hunittests, TestLabel "Round Trip" testSerializationRoundTrip]
  if errors counts == 0 && failures counts == 0
    then exitSuccess
    else exitFailure
