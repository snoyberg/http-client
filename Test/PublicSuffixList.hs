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
-- DO NOT MODIFY! This file has been automatically generated from the CreateTest.hs script at 2013-03-07 09:22:38.613413 UTC
hunittests :: Test
hunittests = TestList [
  TestCase $ assertEqual "0" True $ isSuffix' "COM",
  TestCase $ assertEqual "1" False $ isSuffix' "example.COM",
  TestCase $ assertEqual "2" False $ isSuffix' "WwW.example.COM",
  TestCase $ assertEqual "3" True $ isSuffix' "example",
  TestCase $ assertEqual "4" False $ isSuffix' "example.example",
  TestCase $ assertEqual "5" False $ isSuffix' "b.example.example",
  TestCase $ assertEqual "6" False $ isSuffix' "a.b.example.example",
  TestCase $ assertEqual "7" True $ isSuffix' "biz",
  TestCase $ assertEqual "8" False $ isSuffix' "domain.biz",
  TestCase $ assertEqual "9" False $ isSuffix' "b.domain.biz",
  TestCase $ assertEqual "10" False $ isSuffix' "a.b.domain.biz",
  TestCase $ assertEqual "11" True $ isSuffix' "com",
  TestCase $ assertEqual "12" False $ isSuffix' "example.com",
  TestCase $ assertEqual "13" False $ isSuffix' "b.example.com",
  TestCase $ assertEqual "14" False $ isSuffix' "a.b.example.com",
  TestCase $ assertEqual "15" True $ isSuffix' "uk.com",
  TestCase $ assertEqual "16" False $ isSuffix' "example.uk.com",
  TestCase $ assertEqual "17" False $ isSuffix' "b.example.uk.com",
  TestCase $ assertEqual "18" False $ isSuffix' "a.b.example.uk.com",
  TestCase $ assertEqual "19" False $ isSuffix' "test.ac",
  TestCase $ assertEqual "20" True $ isSuffix' "cy",
  TestCase $ assertEqual "21" True $ isSuffix' "c.cy",
  TestCase $ assertEqual "22" False $ isSuffix' "b.c.cy",
  TestCase $ assertEqual "23" False $ isSuffix' "a.b.c.cy",
  TestCase $ assertEqual "24" True $ isSuffix' "jp",
  TestCase $ assertEqual "25" False $ isSuffix' "test.jp",
  TestCase $ assertEqual "26" False $ isSuffix' "www.test.jp",
  TestCase $ assertEqual "27" True $ isSuffix' "ac.jp",
  TestCase $ assertEqual "28" False $ isSuffix' "test.ac.jp",
  TestCase $ assertEqual "29" False $ isSuffix' "www.test.ac.jp",
  TestCase $ assertEqual "30" True $ isSuffix' "kyoto.jp",
  TestCase $ assertEqual "31" False $ isSuffix' "test.kyoto.jp",
  TestCase $ assertEqual "32" True $ isSuffix' "ide.kyoto.jp",
  TestCase $ assertEqual "33" False $ isSuffix' "b.ide.kyoto.jp",
  TestCase $ assertEqual "34" False $ isSuffix' "a.b.ide.kyoto.jp",
  TestCase $ assertEqual "35" True $ isSuffix' "c.kobe.jp",
  TestCase $ assertEqual "36" False $ isSuffix' "b.c.kobe.jp",
  TestCase $ assertEqual "37" False $ isSuffix' "a.b.c.kobe.jp",
  TestCase $ assertEqual "38" False $ isSuffix' "city.kobe.jp",
  TestCase $ assertEqual "39" False $ isSuffix' "www.city.kobe.jp",
  TestCase $ assertEqual "40" True $ isSuffix' "om",
  TestCase $ assertEqual "41" True $ isSuffix' "test.om",
  TestCase $ assertEqual "42" False $ isSuffix' "b.test.om",
  TestCase $ assertEqual "43" False $ isSuffix' "a.b.test.om",
  TestCase $ assertEqual "44" False $ isSuffix' "songfest.om",
  TestCase $ assertEqual "45" False $ isSuffix' "www.songfest.om",
  TestCase $ assertEqual "46" True $ isSuffix' "us",
  TestCase $ assertEqual "47" False $ isSuffix' "test.us",
  TestCase $ assertEqual "48" False $ isSuffix' "www.test.us",
  TestCase $ assertEqual "49" True $ isSuffix' "ak.us",
  TestCase $ assertEqual "50" False $ isSuffix' "test.ak.us",
  TestCase $ assertEqual "51" False $ isSuffix' "www.test.ak.us",
  TestCase $ assertEqual "52" True $ isSuffix' "k12.ak.us",
  TestCase $ assertEqual "53" False $ isSuffix' "test.k12.ak.us",
  TestCase $ assertEqual "54" False $ isSuffix' "www.test.k12.ak.us"  ]

testSerializationRoundTrip = TestCase $ assertEqual "Round Trip" dataStructure ds
  where Right ds = runGet getDataStructure serializedDataStructure
        serializedDataStructure = runPut $ putDataStructure dataStructure

main = do
  counts <- runTestTT $ TestList [TestLabel "Mozilla Tests" hunittests, TestLabel "Round Trip" testSerializationRoundTrip]
  if errors counts == 0 && failures counts == 0
    then exitSuccess
    else exitFailure
