module Test.PublicSuffixList (tests) where

import Network.PublicSuffixList.Lookup
import System.Exit
import Test.HUnit
import Test.Framework.Providers.HUnit

hunittests :: Test
hunittests = TestList [
  TestCase $ assertEqual "" True  $ matches "com",
  TestCase $ assertEqual "" False $ matches "example.com",
  TestCase $ assertEqual "" False $ matches "www.example.com",
  TestCase $ assertEqual "" True  $ matches "example",
  TestCase $ assertEqual "" True  $ matches "example.example",
  TestCase $ assertEqual "" True  $ matches "b.example.example",
  TestCase $ assertEqual "" True  $ matches "a.b.example.example",
  TestCase $ assertEqual "" True  $ matches "biz",
  TestCase $ assertEqual "" False $ matches "domain.biz",
  TestCase $ assertEqual "" False $ matches "b.domain.biz",
  TestCase $ assertEqual "" False $ matches "a.b.domain.biz",
  TestCase $ assertEqual "" True  $ matches "com",
  TestCase $ assertEqual "" False $ matches "example.com",
  TestCase $ assertEqual "" False $ matches "b.example.com",
  TestCase $ assertEqual "" False $ matches "a.b.example.com",
  TestCase $ assertEqual "" True  $ matches "uk.com",
  TestCase $ assertEqual "" False $ matches "example.uk.com",
  TestCase $ assertEqual "" False $ matches "b.example.uk.com",
  TestCase $ assertEqual "" False $ matches "a.b.example.uk.com",
  TestCase $ assertEqual "" False $ matches "test.ac",
  TestCase $ assertEqual "" True  $ matches "cy",
  TestCase $ assertEqual "" True  $ matches "c.cy",
  TestCase $ assertEqual "" False $ matches "b.c.cy",
  TestCase $ assertEqual "" False $ matches "a.b.c.cy",
  TestCase $ assertEqual "" True  $ matches "jp",
  TestCase $ assertEqual "" False $ matches "test.jp",
  TestCase $ assertEqual "" False $ matches "www.test.jp",
  TestCase $ assertEqual "" True  $ matches "ac.jp",
  TestCase $ assertEqual "" False $ matches "test.ac.jp",
  TestCase $ assertEqual "" False $ matches "www.test.ac.jp",
  TestCase $ assertEqual "" True  $ matches "kyoto.jp",
  TestCase $ assertEqual "" True  $ matches "c.kyoto.jp",
  TestCase $ assertEqual "" False $ matches "b.c.kyoto.jp",
  TestCase $ assertEqual "" False $ matches "a.b.c.kyoto.jp",
  TestCase $ assertEqual "" False $ matches "pref.kyoto.jp",
  TestCase $ assertEqual "" False $ matches "www.pref.kyoto.jp",
  TestCase $ assertEqual "" False $ matches "city.kyoto.jp",
  TestCase $ assertEqual "" False $ matches "www.city.kyoto.jp",
  TestCase $ assertEqual "" True  $ matches "om",
  TestCase $ assertEqual "" True  $ matches "test.om",
  TestCase $ assertEqual "" False $ matches "b.test.om",
  TestCase $ assertEqual "" False $ matches "a.b.test.om",
  TestCase $ assertEqual "" False $ matches "songfest.om",
  TestCase $ assertEqual "" False $ matches "www.songfest.om",
  TestCase $ assertEqual "" True  $ matches "us",
  TestCase $ assertEqual "" False $ matches "test.us",
  TestCase $ assertEqual "" False $ matches "www.test.us",
  TestCase $ assertEqual "" True  $ matches "ak.us",
  TestCase $ assertEqual "" False $ matches "test.ak.us",
  TestCase $ assertEqual "" False $ matches "www.test.ak.us",
  TestCase $ assertEqual "" True  $ matches "k12.ak.us",
  TestCase $ assertEqual "" False $ matches "test.k12.ak.us",
  TestCase $ assertEqual "" False $ matches "www.test.k12.ak.us"
  ]

tests = hUnitTestToTests hunittests

main = do
  counts <- runTestTT hunittests
  if errors counts == 0 && failures counts == 0
    then exitSuccess
    else exitFailure