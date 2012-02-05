module Main where
{-
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import qualified Test.Framework.Providers.API as TF
-}
import System.Exit (exitFailure, exitSuccess)
import qualified Data.ByteString as BS
import Test.HUnit hiding (path)
import Network.HTTP.Conduit.Cookies.Internal
import qualified Network.HTTP.Conduit as HC
import Data.ByteString.UTF8
import Data.Maybe
import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.Set as S
import qualified Data.CaseInsensitive as CI
import Web.Cookie

default_request :: HC.Request m
default_request = fromJust $ HC.parseUrl "http://www.google.com/"

default_cookie :: Cookie
default_cookie = Cookie { name = fromString "name"
                        , value = fromString "value"
                        , expiry_time = default_time
                        , domain = fromString "www.google.com"
                        , path = fromString "/"
                        , creation_time = default_time
                        , last_access_time = default_time
                        , persistent = False
                        , host_only = False
                        , secure_only = False
                        , http_only = False
                        }

default_time :: UTCTime
default_time = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

default_diff_time :: DiffTime
default_diff_time = secondsToDiffTime 0

default_set_cookie :: SetCookie
default_set_cookie = SetCookie { setCookieName = fromString "name"
                               , setCookieValue = fromString "value"
                               , setCookiePath = Just $ fromString "/"
                               , setCookieExpires = Just default_time
                               , setCookieMaxAge = Just default_diff_time
                               , setCookieDomain = Just $ fromString "www.google.com"
                               , setCookieHttpOnly = False
                               , setCookieSecure = False
                               }

testValidIp = TestCase $ assertBool "Couldn't parse valid IP address" $
  isIpAddress $ fromString "1.2.3.4"

testIpNumTooHigh = TestCase $ assertBool "One of the digits in the IP address is too large" $
  not $ isIpAddress $ fromString "501.2.3.4"

testTooManySegmentsInIp = TestCase $ assertBool "Too many segments in the ip address" $
  not $ isIpAddress $ fromString "1.2.3.4.5"

testCharsInIp = TestCase $ assertBool "Chars are not allowed in IP addresses" $
  not $ isIpAddress $ fromString "1.2a3.4.5"

testDomainMatchesSuccess = TestCase $ assertBool "Domains should match" $
  domainMatches (fromString "www.google.com") (fromString "google.com")

testSameDomain = TestCase $ assertBool "Same domain should match" $
  domainMatches domain domain
  where domain = fromString "www.google.com"

testSiblingDomain = TestCase $ assertBool "Sibling domain should not match" $
  not $ domainMatches (fromString "www.google.com") (fromString "secure.google.com")

testParentDomain = TestCase $ assertBool "Parent domain should fail" $
  not $ domainMatches (fromString "google.com") (fromString "www.google.com")

testNaiveSuffixDomain = TestCase $ assertBool "Naively checking for suffix for domain matching should fail" $
  not $ domainMatches (fromString "agoogle.com") (fromString "google.com")

testDefaultPath = TestCase $ assertEqual "Getting default path from a request"
  (fromString "/") (defaultPath default_request)

testShortDefaultPath = TestCase $ assertEqual "Getting default path from a short path"
  (fromString "/") (defaultPath $ default_request {HC.path = fromString "/search"})

testPopulatedDefaultPath = TestCase $ assertEqual "Getting default path from a request with a path"
  (fromString "/search") (defaultPath $ default_request {HC.path = fromString "/search/term"})

testParamsDefaultPath = TestCase $ assertEqual "Getting default path from a request with a path and GET params"
  (fromString "/search") (defaultPath $ default_request {HC.path = fromString "/search/term?var=val"})

testDefaultPathEndingInSlash = TestCase $ assertEqual "Getting default path that ends in a slash"
  (fromString "/search/term") (defaultPath $ default_request {HC.path = fromString "/search/term/"})

testSamePathsMatch = TestCase $ assertBool "The same path should match" $
  pathMatches path path
  where path = fromString "/a/path"

testPathSlashAtEnd = TestCase $ assertBool "Putting the slash at the end should still match paths" $
  pathMatches (fromString "/a/path/to/here") (fromString "/a/path/")

testPathNoSlashAtEnd = TestCase $ assertBool "Not putting the slash at the end should still match paths" $
  pathMatches (fromString "/a/path/to/here") (fromString "/a/path")

testDivergingPaths = TestCase $ assertBool "Diverging paths don't match" $
  not $ pathMatches (fromString "/a/path/to/here") (fromString "/a/different/path")

testCookieEqualitySuccess = TestCase $ assertEqual "The same cookies should be equal"
  cookie cookie
  where cookie = default_cookie

testCookieEqualityResiliance = TestCase $ assertEqual "Cookies should still be equal if extra options are changed"
  (default_cookie {persistent = True}) (default_cookie {host_only = True})

testDomainChangesEquality = TestCase $ assertBool "Changing the domain should make cookies not equal" $
  default_cookie /= (default_cookie {domain = fromString "/search"})

testRemoveCookie = TestCase $ assertEqual "Removing a cookie works"
  (Just default_cookie, []) (removeExistingCookieFromCookieJar default_cookie [default_cookie])

testRemoveNonexistantCookie = TestCase $ assertEqual "Removing a nonexistant cookie doesn't work"
  (Nothing, [default_cookie]) (removeExistingCookieFromCookieJar (default_cookie {name = fromString "key2"}) [default_cookie])

testRemoveCorrectCookie = TestCase $ assertEqual "Removing only the correct cookie"
  (Just search_for, [red_herring]) (removeExistingCookieFromCookieJar search_for [red_herring, search_for])
  where search_for = default_cookie {name = fromString "name1"}
        red_herring = default_cookie {name = fromString "name2"}

testEvictExpiredCookies = TestCase $ assertEqual "Evicting expired cookies works"
  [a, c] (evictExpiredCookies [a, b, c, d] middle)
  where a = default_cookie {name = fromString "a", expiry_time = UTCTime (ModifiedJulianDay 3) (secondsToDiffTime 0)}
        b = default_cookie {name = fromString "b", expiry_time = UTCTime (ModifiedJulianDay 1) (secondsToDiffTime 0)}
        c = default_cookie {name = fromString "c", expiry_time = UTCTime (ModifiedJulianDay 3) (secondsToDiffTime 0)}
        d = default_cookie {name = fromString "d", expiry_time = UTCTime (ModifiedJulianDay 1) (secondsToDiffTime 0)}
        middle = UTCTime (ModifiedJulianDay 2) (secondsToDiffTime 0)

testEvictNoCookies = TestCase $ assertEqual "Evicting empty cookie jar"
  [] (evictExpiredCookies [] middle)
  where middle = UTCTime (ModifiedJulianDay 2) (secondsToDiffTime 0)

testComputeCookieStringUpdateLastAccessTime = TestCase $ assertEqual "Updates last access time upon using cookies"
  (fromString "name=value", out_cookie_jar) (computeCookieString request cookie_jar now True)
  where request = default_request
        cookie_jar = [default_cookie]
        now = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 1)
        out_cookie_jar = [default_cookie {last_access_time = now}]

testComputeCookieStringHostOnly = TestCase $ assertEqual "Host only cookies should match host exactly"
  (fromString "name=value", cookie_jar) (computeCookieString request cookie_jar default_time True)
  where request = default_request
        cookie_jar = [default_cookie {host_only = True}]

testComputeCookieStringHostOnlyFilter = TestCase $ assertEqual "Host only cookies shouldn't match subdomain"
  (fromString "", cookie_jar) (computeCookieString request cookie_jar default_time True)
  where request = default_request {HC.host = fromString "sub1.sub2.google.com"}
        cookie_jar = [default_cookie {host_only = True, domain = fromString "sub2.google.com"}]

testComputeCookieStringDomainMatching = TestCase $ assertEqual "Domain matching works for new requests"
  (fromString "name=value", cookie_jar) (computeCookieString request cookie_jar default_time True)
  where request = default_request {HC.host = fromString "sub1.sub2.google.com"}
        cookie_jar = [default_cookie {domain = fromString "sub2.google.com"}]

testComputeCookieStringPathMatching = TestCase $ assertEqual "Path matching works for new requests"
  (fromString "name=value", cookie_jar) (computeCookieString request cookie_jar default_time True)
  where request = default_request {HC.path = fromString "/a/path/to/nowhere"}
        cookie_jar = [default_cookie {path = fromString "/a/path"}]

testComputeCookieStringPathMatchingFails = TestCase $ assertEqual "Path matching fails when it should"
  (fromString "", cookie_jar) (computeCookieString request cookie_jar default_time True)
  where request = default_request {HC.path = fromString "/a/different/path/to/nowhere"}
        cookie_jar = [default_cookie {path = fromString "/a/path"}]

testComputeCookieStringPathMatchingWithParms = TestCase $ assertEqual "Path matching succeeds when request has GET params"
  (fromString "name=value", cookie_jar) (computeCookieString request cookie_jar default_time True)
  where request = default_request {HC.path = fromString "/a/path/to/nowhere?var=val"}
        cookie_jar = [default_cookie {path = fromString "/a/path"}]

testComputeCookieStringSecure = TestCase $ assertEqual "Secure flag filters properly"
  (fromString "", cookie_jar) (computeCookieString default_request cookie_jar default_time True)
  where cookie_jar = [default_cookie {secure_only = True}]

testComputeCookieStringHttpOnly = TestCase $ assertEqual "http-only flag filters properly"
  (fromString "", cookie_jar) (computeCookieString default_request cookie_jar default_time False)
  where cookie_jar = [default_cookie {http_only = True}]

testComputeCookieStringSort = TestCase $ assertEqual "Sorting works correctly"
  (fromString "c1=v1;c3=v3;c4=v4;c2=v2", S.fromList cookie_jar_out) format_output
  where now = UTCTime (ModifiedJulianDay 10) (secondsToDiffTime 11)
        cookie_jar = [ default_cookie { name = fromString "c1"
                                      , value = fromString "v1"
                                      , path = fromString "/all/encompassing/request"
                                      }
                     , default_cookie { name = fromString "c2"
                                      , value = fromString "v2"
                                      , path = fromString "/all"
                                      }
                     , default_cookie { name = fromString "c3"
                                      , value = fromString "v3"
                                      , path = fromString "/all/encompassing"
                                      , creation_time = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 1)
                                      }
                     , default_cookie { name = fromString "c4"
                                      , value = fromString "v4"
                                      , path = fromString "/all/encompassing"
                                      , creation_time = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 2)
                                      }
                     ]
        cookie_jar_out = [ default_cookie { name = fromString "c1"
                                          , value = fromString "v1"
                                          , path = fromString "/all/encompassing/request"
                                          , last_access_time = now
                                          }
                         , default_cookie { name = fromString "c2"
                                          , value = fromString "v2"
                                          , path = fromString "/all"
                                          , last_access_time = now
                                          }
                         , default_cookie { name = fromString "c3"
                                          , value = fromString "v3"
                                          , path = fromString "/all/encompassing"
                                          , creation_time = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 1)
                                          , last_access_time = now
                                          }
                         , default_cookie { name = fromString "c4"
                                          , value = fromString "v4"
                                          , path = fromString "/all/encompassing"
                                          , creation_time = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 2)
                                          , last_access_time = now
                                          }
                         ]
        request = default_request {HC.path = fromString "/all/encompassing/request/path"}
        format_output = helper $ computeCookieString request cookie_jar default_time False
        helper (a, b) = (a, S.fromList b)

testInsertCookiesIntoRequestWorks = TestCase $ assertEqual "Inserting cookies works"
  [(CI.mk $ fromString "Cookie", fromString "key=val")] out_headers
  where out_headers = HC.requestHeaders req
        (req, _) = insertCookiesIntoRequest req' cookie_jar default_time
        cookie_jar = [default_cookie { name = fromString "key"
                                     , value = fromString "val"
                                     }]
        req' = default_request {HC.requestHeaders = [(CI.mk $ fromString "Cookie",
                                                      fromString "otherkey=otherval")]}

testReceiveSetCookie = TestCase $ assertEqual "Receiving a Set-Cookie"
  [default_cookie] (receiveSetCookie default_set_cookie default_request default_time True [])

testReceiveSetCookieTrailingDot = TestCase $ assertEqual "Receiving a Set-Cookie with a trailing domain dot"
  [] (receiveSetCookie set_cookie default_request default_time True [])
  where set_cookie = default_set_cookie {setCookieDomain = Just $ fromString "www.google.com."}

testReceiveSetCookieLeadingDot = TestCase $ assertEqual "Receiving a Set-Cookie with a leading domain dot"
  [default_cookie] (receiveSetCookie set_cookie default_request default_time True [])
  where set_cookie = default_set_cookie {setCookieDomain = Just $ fromString ".www.google.com"}

testReceiveSetCookieNoDomain = TestCase $ assertEqual "Receiving cookie without domain"
  [default_cookie] (receiveSetCookie set_cookie default_request default_time True [])
  where set_cookie = default_set_cookie {setCookieDomain = Nothing}

testReceiveSetCookieEmptyDomain = TestCase $ assertEqual "Receiving cookie with empty domain"
  [default_cookie] (receiveSetCookie set_cookie default_request default_time True [])
  where set_cookie = default_set_cookie {setCookieDomain = Just BS.empty}

-- Can't test public suffixes until that module is written

testReceiveSetCookieNonMatchingDomain = TestCase $ assertEqual "Receiving cookie with non-matching domain"
  [] (receiveSetCookie set_cookie default_request default_time True [])
  where set_cookie = default_set_cookie {setCookieDomain = Just $ fromString "www.wikipedia.org"}

testReceiveSetCookieHostOnly = TestCase $ assertBool "Checking host-only flag gets set" $
  host_only $ head $ receiveSetCookie set_cookie default_request default_time True []
  where set_cookie = default_set_cookie {setCookieDomain = Nothing}

testReceiveSetCookieHostOnlyNotSet = TestCase $ assertBool "Checking host-only flag doesn't get set" $
  not $ host_only $ head $ receiveSetCookie set_cookie default_request default_time True []
  where set_cookie = default_set_cookie {setCookieDomain = Just $ fromString "google.com"}

testReceiveSetCookieHttpOnly = TestCase $ assertBool "Checking http-only flag gets set" $
  http_only $ head $ receiveSetCookie set_cookie default_request default_time True []
  where set_cookie = default_set_cookie {setCookieHttpOnly = True}

testReceiveSetCookieHttpOnlyNotSet = TestCase $ assertBool "Checking http-only flag doesn't get set" $
  not $ http_only $ head $ receiveSetCookie set_cookie default_request default_time True []
  where set_cookie = default_set_cookie {setCookieHttpOnly = False}

testReceiveSetCookieHttpOnlyDrop = TestCase $ assertEqual "Checking non http request gets dropped"
  [] (receiveSetCookie set_cookie default_request default_time False [])
  where set_cookie = default_set_cookie {setCookieHttpOnly = True}

testReceiveSetCookieName = TestCase $ assertEqual "Name gets set correctly"
  (fromString "name") (name $ head $ receiveSetCookie default_set_cookie default_request default_time True [])

testReceiveSetCookieValue = TestCase $ assertEqual "Value gets set correctly"
  (fromString "value") (value $ head $ receiveSetCookie default_set_cookie default_request default_time True [])

testReceiveSetCookieExpiry = TestCase $ assertEqual "Expiry gets set correctly"
  default_time (expiry_time $ head $ receiveSetCookie default_set_cookie default_request default_time True [])

testReceiveSetCookiePath = TestCase $ assertEqual "Path gets set correctly"
  (fromString "/a/path") (path $ head $ receiveSetCookie set_cookie default_request default_time True [])
  where set_cookie = default_set_cookie {setCookiePath = Just $ fromString "/a/path"}

testReceiveSetCookieNoPath = TestCase $ assertEqual "Path gets set correctly when nonexistant"
  (fromString "/a/path/to") (path $ head $ receiveSetCookie set_cookie request default_time True [])
  where set_cookie = default_set_cookie {setCookiePath = Nothing}
        request = default_request {HC.path = fromString "/a/path/to/nowhere"}

testReceiveSetCookieCreationTime = TestCase $ assertEqual "Creation time gets set correctly"
  now (creation_time $ head $ receiveSetCookie default_set_cookie default_request now True [])
  where now = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 1)

testReceiveSetCookieAccessTime = TestCase $ assertEqual "Last access time gets set correctly"
  now (last_access_time $ head $ receiveSetCookie default_set_cookie default_request now True [])
  where now = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 1)

testReceiveSetCookiePersistent = TestCase $ assertBool "Persistent flag gets set correctly" $
  persistent $ head $ receiveSetCookie set_cookie default_request default_time True []
  where set_cookie = default_set_cookie {setCookieExpires = Just default_time}

testReceiveSetCookieSecure = TestCase $ assertBool "Secure flag gets set correctly" $
  secure_only $ head $ receiveSetCookie set_cookie default_request default_time True []
  where set_cookie = default_set_cookie {setCookieSecure = True}

testReceiveSetCookieMaxAge = TestCase $ assertEqual "Max-Age gets set correctly"
  total (expiry_time $ head $ receiveSetCookie set_cookie default_request now True [])
  where set_cookie = default_set_cookie { setCookieExpires = Nothing
                                        , setCookieMaxAge = Just $ secondsToDiffTime 10
                                        }
        now = UTCTime (ModifiedJulianDay 10) (secondsToDiffTime 12)
        total = UTCTime (ModifiedJulianDay 10) (secondsToDiffTime 22)

testReceiveSetCookiePreferMaxAge = TestCase $ assertEqual "Max-Age is preferred over Expires"
  total (expiry_time $ head $ receiveSetCookie set_cookie default_request now True [])
  where set_cookie = default_set_cookie { setCookieExpires = Just exp
                                        , setCookieMaxAge = Just $ secondsToDiffTime 10
                                        }
        exp = UTCTime (ModifiedJulianDay 11) (secondsToDiffTime 5)
        now = UTCTime (ModifiedJulianDay 10) (secondsToDiffTime 12)
        total = UTCTime (ModifiedJulianDay 10) (secondsToDiffTime 22)

testReceiveSetCookieExisting = TestCase $ assertEqual "Existing cookie gets updated"
  t (expiry_time $ head $ receiveSetCookie set_cookie default_request default_time True [default_cookie])
  where set_cookie = default_set_cookie { setCookieExpires = Just t
                                        , setCookieMaxAge = Nothing
                                        }
        t = UTCTime (ModifiedJulianDay 10) (secondsToDiffTime 12)

testReceiveSetCookieExistingCreation = TestCase $ assertEqual "Creation time gets updated in existing cookie"
  default_time (creation_time $ head $ receiveSetCookie default_set_cookie default_request now True [default_cookie])
  where now = UTCTime (ModifiedJulianDay 10) (secondsToDiffTime 12)

testReceiveSetCookieExistingHttpOnly = TestCase $ assertEqual "Existing http-only cookie gets dropped"
  default_time (expiry_time $ head $ receiveSetCookie default_set_cookie default_request default_time False [existing_cookie])
  where existing_cookie = default_cookie {http_only = True}
        set_cookie = default_set_cookie {setCookieExpires = Just t}
        t = UTCTime (ModifiedJulianDay 10) (secondsToDiffTime 12)

ipParseTests = TestList [ TestLabel "Valid IP" testValidIp
                        , TestLabel "Digit Too High" testIpNumTooHigh
                        , TestLabel "Too Many Segments" testTooManySegmentsInIp
                        , TestLabel "Chars in IP" testCharsInIp
                        ]

domainMatchingTests = TestList [ TestLabel "Should Match" testDomainMatchesSuccess
                               , TestLabel "Same Domain" testSameDomain
                               , TestLabel "Sibling Domain" testSiblingDomain
                               , TestLabel "Parent Domain" testParentDomain
                               , TestLabel "Checking for Naive suffix-check" testNaiveSuffixDomain
                               ]

defaultPathTests = TestList [TestLabel "Basic default path test" testDefaultPath
                            , TestLabel "Basic populated default path" testPopulatedDefaultPath
                            , TestLabel "Default path from request with GET params works" testParamsDefaultPath
                            , TestLabel "Getting a default path that ends in a slash" testDefaultPathEndingInSlash
                            , TestLabel "Getting a short default path" testShortDefaultPath
                            ]

pathMatchingTests = TestList [ TestLabel "Same paths match" testSamePathsMatch
                             , TestLabel "Putting slash at end" testPathSlashAtEnd
                             , TestLabel "Not putting slash at end" testPathNoSlashAtEnd
                             , TestLabel "Diverging paths don't match" testDivergingPaths
                             ]

equalityTests = TestList [ TestLabel "The same cookie should be equal to itself" testCookieEqualitySuccess
                         , TestLabel "Changing extra options shouldn't change equality" testCookieEqualityResiliance
                         , TestLabel "Changing a cookie's domain should change its equality" testDomainChangesEquality
                         ]

removeTests = TestList [ TestLabel "Removing a cookie works" testRemoveCookie
                       , TestLabel "Removing a nonexistant cookie doesn't work" testRemoveNonexistantCookie
                       , TestLabel "Removing the correct cookie" testRemoveCorrectCookie
                       ]

evictionTests = TestList [ TestLabel "Testing eviction" testEvictExpiredCookies
                         , TestLabel "Evicting from empty cookie jar" testEvictNoCookies
                         ]

sendingTests = TestList [ TestLabel "Updates last access time upon using cookies" testComputeCookieStringUpdateLastAccessTime
                        , TestLabel "Host-only flag matches exact host" testComputeCookieStringHostOnly
                        , TestLabel "Host-only flag doesn't match subdomain" testComputeCookieStringHostOnlyFilter
                        , TestLabel "Domain matching works properly" testComputeCookieStringDomainMatching
                        , TestLabel "Path matching works" testComputeCookieStringPathMatching
                        , TestLabel "Path matching fails when it should" testComputeCookieStringPathMatchingFails
                        , TestLabel "Path matching succeeds when request has GET params" testComputeCookieStringPathMatchingWithParms
                        , TestLabel "Secure flag filters correctly" testComputeCookieStringSecure
                        , TestLabel "Http-only flag filters correctly" testComputeCookieStringHttpOnly
                        , TestLabel "Sorting works correctly" testComputeCookieStringSort
                        , TestLabel "Inserting cookie header works" testInsertCookiesIntoRequestWorks
                        ]

receivingTests = TestList [ TestLabel "Can receive set-cookie" testReceiveSetCookie
                          , TestLabel "Receiving a Set-Cookie with a trailing dot on the domain" testReceiveSetCookieTrailingDot
                          , TestLabel "Receiving a Set-Cookie with a leading dot on the domain" testReceiveSetCookieLeadingDot
                          , TestLabel "Set-Cookie with no domain" testReceiveSetCookieNoDomain
                          , TestLabel "Set-Cookie with empty domain" testReceiveSetCookieEmptyDomain
                          , TestLabel "Set-Cookie with non-matching domain" testReceiveSetCookieNonMatchingDomain
                          , TestLabel "Host-only flag gets set" testReceiveSetCookieHostOnly
                          , TestLabel "Host-only flag doesn't get set" testReceiveSetCookieHostOnlyNotSet
                          , TestLabel "Http-only flag gets set" testReceiveSetCookieHttpOnly
                          , TestLabel "Http-only flag doesn't get set" testReceiveSetCookieHttpOnlyNotSet
                          , TestLabel "Checking non http request gets dropped" testReceiveSetCookieHttpOnlyDrop
                          , TestLabel "Name gets set correctly" testReceiveSetCookieName
                          , TestLabel "Value gets set correctly" testReceiveSetCookieValue
                          , TestLabel "Expiry gets set correctly" testReceiveSetCookieExpiry
                          , TestLabel "Path gets set correctly when nonexistant" testReceiveSetCookieNoPath
                          , TestLabel "Path gets set correctly" testReceiveSetCookiePath
                          , TestLabel "Creation time gets set correctly" testReceiveSetCookieCreationTime
                          , TestLabel "Last access time gets set correctly" testReceiveSetCookieAccessTime
                          , TestLabel "Persistent flag gets set correctly" testReceiveSetCookiePersistent
                          , TestLabel "Existing cookie gets updated" testReceiveSetCookieExisting
                          , TestLabel "Creation time gets updated in existing cookie" testReceiveSetCookieExistingCreation
                          , TestLabel "Existing http-only cookie gets dropped" testReceiveSetCookieExistingHttpOnly
                          , TestLabel "Secure flag gets set correctly" testReceiveSetCookieSecure
                          , TestLabel "Max-Age flag gets set correctly" testReceiveSetCookieMaxAge
                          , TestLabel "Max-Age is preferred over Expires" testReceiveSetCookiePreferMaxAge
                          ]

allTests = TestList [ ipParseTests
                    , domainMatchingTests
                    , defaultPathTests
                    , pathMatchingTests
                    , equalityTests
                    , removeTests
                    , evictionTests
                    , sendingTests
                    , receivingTests
                    ]

{-
tests :: [TF.Test]
tests = hUnitTestToTests allTests
-}

main :: IO ()
main = do
  counts <- runTestTT allTests
  if errors counts > 0 || failures counts > 0 then exitFailure else exitSuccess
