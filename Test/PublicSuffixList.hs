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


effectiveTLDPlusOne' :: T.Text -> Maybe T.Text
effectiveTLDPlusOne' = L.effectiveTLDPlusOne . T.intercalate "." . map (fromJust . toASCII False True . T.map toLower) . T.split (== '.')
-- DO NOT MODIFY! This file has been automatically generated from the CreateTest.hs script at 2013-04-11 06:46:12.885338 UTC
hunittests :: Test
hunittests = TestList [
  TestCase $ assertEqual "0" (Nothing) $ effectiveTLDPlusOne' "COM",
  TestCase $ assertEqual "1" (Just "example.com") $ effectiveTLDPlusOne' "example.COM",
  TestCase $ assertEqual "2" (Just "example.com") $ effectiveTLDPlusOne' "WwW.example.COM",
  TestCase $ assertEqual "3" (Nothing) $ effectiveTLDPlusOne' "example",
  TestCase $ assertEqual "4" (Just "example.example") $ effectiveTLDPlusOne' "example.example",
  TestCase $ assertEqual "5" (Just "example.example") $ effectiveTLDPlusOne' "b.example.example",
  TestCase $ assertEqual "6" (Just "example.example") $ effectiveTLDPlusOne' "a.b.example.example",
  TestCase $ assertEqual "7" (Nothing) $ effectiveTLDPlusOne' "biz",
  TestCase $ assertEqual "8" (Just "domain.biz") $ effectiveTLDPlusOne' "domain.biz",
  TestCase $ assertEqual "9" (Just "domain.biz") $ effectiveTLDPlusOne' "b.domain.biz",
  TestCase $ assertEqual "10" (Just "domain.biz") $ effectiveTLDPlusOne' "a.b.domain.biz",
  TestCase $ assertEqual "11" (Nothing) $ effectiveTLDPlusOne' "com",
  TestCase $ assertEqual "12" (Just "example.com") $ effectiveTLDPlusOne' "example.com",
  TestCase $ assertEqual "13" (Just "example.com") $ effectiveTLDPlusOne' "b.example.com",
  TestCase $ assertEqual "14" (Just "example.com") $ effectiveTLDPlusOne' "a.b.example.com",
  TestCase $ assertEqual "15" (Nothing) $ effectiveTLDPlusOne' "uk.com",
  TestCase $ assertEqual "16" (Just "example.uk.com") $ effectiveTLDPlusOne' "example.uk.com",
  TestCase $ assertEqual "17" (Just "example.uk.com") $ effectiveTLDPlusOne' "b.example.uk.com",
  TestCase $ assertEqual "18" (Just "example.uk.com") $ effectiveTLDPlusOne' "a.b.example.uk.com",
  TestCase $ assertEqual "19" (Just "test.ac") $ effectiveTLDPlusOne' "test.ac",
  TestCase $ assertEqual "20" (Nothing) $ effectiveTLDPlusOne' "cy",
  TestCase $ assertEqual "21" (Nothing) $ effectiveTLDPlusOne' "c.cy",
  TestCase $ assertEqual "22" (Just "b.c.cy") $ effectiveTLDPlusOne' "b.c.cy",
  TestCase $ assertEqual "23" (Just "b.c.cy") $ effectiveTLDPlusOne' "a.b.c.cy",
  TestCase $ assertEqual "24" (Nothing) $ effectiveTLDPlusOne' "jp",
  TestCase $ assertEqual "25" (Just "test.jp") $ effectiveTLDPlusOne' "test.jp",
  TestCase $ assertEqual "26" (Just "test.jp") $ effectiveTLDPlusOne' "www.test.jp",
  TestCase $ assertEqual "27" (Nothing) $ effectiveTLDPlusOne' "ac.jp",
  TestCase $ assertEqual "28" (Just "test.ac.jp") $ effectiveTLDPlusOne' "test.ac.jp",
  TestCase $ assertEqual "29" (Just "test.ac.jp") $ effectiveTLDPlusOne' "www.test.ac.jp",
  TestCase $ assertEqual "30" (Nothing) $ effectiveTLDPlusOne' "kyoto.jp",
  TestCase $ assertEqual "31" (Just "test.kyoto.jp") $ effectiveTLDPlusOne' "test.kyoto.jp",
  TestCase $ assertEqual "32" (Nothing) $ effectiveTLDPlusOne' "ide.kyoto.jp",
  TestCase $ assertEqual "33" (Just "b.ide.kyoto.jp") $ effectiveTLDPlusOne' "b.ide.kyoto.jp",
  TestCase $ assertEqual "34" (Just "b.ide.kyoto.jp") $ effectiveTLDPlusOne' "a.b.ide.kyoto.jp",
  TestCase $ assertEqual "35" (Nothing) $ effectiveTLDPlusOne' "c.kobe.jp",
  TestCase $ assertEqual "36" (Just "b.c.kobe.jp") $ effectiveTLDPlusOne' "b.c.kobe.jp",
  TestCase $ assertEqual "37" (Just "b.c.kobe.jp") $ effectiveTLDPlusOne' "a.b.c.kobe.jp",
  TestCase $ assertEqual "38" (Just "city.kobe.jp") $ effectiveTLDPlusOne' "city.kobe.jp",
  TestCase $ assertEqual "39" (Just "city.kobe.jp") $ effectiveTLDPlusOne' "www.city.kobe.jp",
  TestCase $ assertEqual "40" (Nothing) $ effectiveTLDPlusOne' "om",
  TestCase $ assertEqual "41" (Nothing) $ effectiveTLDPlusOne' "test.om",
  TestCase $ assertEqual "42" (Just "b.test.om") $ effectiveTLDPlusOne' "b.test.om",
  TestCase $ assertEqual "43" (Just "b.test.om") $ effectiveTLDPlusOne' "a.b.test.om",
  TestCase $ assertEqual "44" (Just "songfest.om") $ effectiveTLDPlusOne' "songfest.om",
  TestCase $ assertEqual "45" (Just "songfest.om") $ effectiveTLDPlusOne' "www.songfest.om",
  TestCase $ assertEqual "46" (Nothing) $ effectiveTLDPlusOne' "us",
  TestCase $ assertEqual "47" (Just "test.us") $ effectiveTLDPlusOne' "test.us",
  TestCase $ assertEqual "48" (Just "test.us") $ effectiveTLDPlusOne' "www.test.us",
  TestCase $ assertEqual "49" (Nothing) $ effectiveTLDPlusOne' "ak.us",
  TestCase $ assertEqual "50" (Just "test.ak.us") $ effectiveTLDPlusOne' "test.ak.us",
  TestCase $ assertEqual "51" (Just "test.ak.us") $ effectiveTLDPlusOne' "www.test.ak.us",
  TestCase $ assertEqual "52" (Nothing) $ effectiveTLDPlusOne' "k12.ak.us",
  TestCase $ assertEqual "53" (Just "test.k12.ak.us") $ effectiveTLDPlusOne' "test.k12.ak.us",
  TestCase $ assertEqual "54" (Just "test.k12.ak.us") $ effectiveTLDPlusOne' "www.test.k12.ak.us"  ]

testSerializationRoundTrip = TestCase $ assertEqual "Round Trip" dataStructure ds
  where Right ds = runGet getDataStructure serializedDataStructure
        serializedDataStructure = runPut $ putDataStructure dataStructure

main = do
  counts <- runTestTT $ TestList [TestLabel "Mozilla Tests" hunittests, TestLabel "Round Trip" testSerializationRoundTrip]
  if errors counts == 0 && failures counts == 0
    then exitSuccess
    else exitFailure
