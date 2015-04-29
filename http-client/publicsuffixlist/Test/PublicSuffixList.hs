{-# LANGUAGE OverloadedStrings #-}

import           Data.Char
import           Data.Maybe
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
-- DO NOT MODIFY! This file has been automatically generated from the CreateTest.hs script at 2015-04-29 05:00:07.582598 UTC
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
  TestCase $ assertEqual "40" (Nothing) $ effectiveTLDPlusOne' "ck",
  TestCase $ assertEqual "41" (Nothing) $ effectiveTLDPlusOne' "test.ck",
  TestCase $ assertEqual "42" (Just "b.test.ck") $ effectiveTLDPlusOne' "b.test.ck",
  TestCase $ assertEqual "43" (Just "b.test.ck") $ effectiveTLDPlusOne' "a.b.test.ck",
  TestCase $ assertEqual "44" (Just "www.ck") $ effectiveTLDPlusOne' "www.ck",
  TestCase $ assertEqual "45" (Just "www.ck") $ effectiveTLDPlusOne' "www.www.ck",
  TestCase $ assertEqual "46" (Nothing) $ effectiveTLDPlusOne' "us",
  TestCase $ assertEqual "47" (Just "test.us") $ effectiveTLDPlusOne' "test.us",
  TestCase $ assertEqual "48" (Just "test.us") $ effectiveTLDPlusOne' "www.test.us",
  TestCase $ assertEqual "49" (Nothing) $ effectiveTLDPlusOne' "ak.us",
  TestCase $ assertEqual "50" (Just "test.ak.us") $ effectiveTLDPlusOne' "test.ak.us",
  TestCase $ assertEqual "51" (Just "test.ak.us") $ effectiveTLDPlusOne' "www.test.ak.us",
  TestCase $ assertEqual "52" (Nothing) $ effectiveTLDPlusOne' "k12.ak.us",
  TestCase $ assertEqual "53" (Just "test.k12.ak.us") $ effectiveTLDPlusOne' "test.k12.ak.us",
  TestCase $ assertEqual "54" (Just "test.k12.ak.us") $ effectiveTLDPlusOne' "www.test.k12.ak.us",
  TestCase $ assertEqual "55" (Just "\39135\29422.com.cn") $ effectiveTLDPlusOne' "食狮.com.cn",
  TestCase $ assertEqual "56" (Just "\39135\29422.\20844\21496.cn") $ effectiveTLDPlusOne' "食狮.公司.cn",
  TestCase $ assertEqual "57" (Just "\39135\29422.\20844\21496.cn") $ effectiveTLDPlusOne' "www.食狮.公司.cn",
  TestCase $ assertEqual "58" (Just "shishi.\20844\21496.cn") $ effectiveTLDPlusOne' "shishi.公司.cn",
  TestCase $ assertEqual "59" (Nothing) $ effectiveTLDPlusOne' "公司.cn",
  TestCase $ assertEqual "60" (Just "\39135\29422.\20013\22269") $ effectiveTLDPlusOne' "食狮.中国",
  TestCase $ assertEqual "61" (Just "\39135\29422.\20013\22269") $ effectiveTLDPlusOne' "www.食狮.中国",
  TestCase $ assertEqual "62" (Just "shishi.\20013\22269") $ effectiveTLDPlusOne' "shishi.中国",
  TestCase $ assertEqual "63" (Nothing) $ effectiveTLDPlusOne' "中国",
  TestCase $ assertEqual "64" (Just "xn--85x722f.com.cn") $ effectiveTLDPlusOne' "xn--85x722f.com.cn",
  TestCase $ assertEqual "65" (Just "xn--85x722f.xn--55qx5d.cn") $ effectiveTLDPlusOne' "xn--85x722f.xn--55qx5d.cn",
  TestCase $ assertEqual "66" (Just "xn--85x722f.xn--55qx5d.cn") $ effectiveTLDPlusOne' "www.xn--85x722f.xn--55qx5d.cn",
  TestCase $ assertEqual "67" (Just "shishi.xn--55qx5d.cn") $ effectiveTLDPlusOne' "shishi.xn--55qx5d.cn",
  TestCase $ assertEqual "68" (Nothing) $ effectiveTLDPlusOne' "xn--55qx5d.cn",
  TestCase $ assertEqual "69" (Just "xn--85x722f.xn--fiqs8s") $ effectiveTLDPlusOne' "xn--85x722f.xn--fiqs8s",
  TestCase $ assertEqual "70" (Just "xn--85x722f.xn--fiqs8s") $ effectiveTLDPlusOne' "www.xn--85x722f.xn--fiqs8s",
  TestCase $ assertEqual "71" (Just "shishi.xn--fiqs8s") $ effectiveTLDPlusOne' "shishi.xn--fiqs8s",
  TestCase $ assertEqual "72" (Nothing) $ effectiveTLDPlusOne' "xn--fiqs8s"  ]

testSerializationRoundTrip = TestCase $ assertEqual "Round Trip" dataStructure ds
  where ds = getDataStructure serializedDataStructure
        serializedDataStructure = putDataStructure dataStructure

main = do
  counts <- runTestTT $ TestList [TestLabel "Mozilla Tests" hunittests, TestLabel "Round Trip" testSerializationRoundTrip]
  if errors counts == 0 && failures counts == 0
    then exitSuccess
    else exitFailure
