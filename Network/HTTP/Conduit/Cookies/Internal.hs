-- | This module implements the algorithms described in RFC 6265 for the Network.HTTP.Conduit library.
module Network.HTTP.Conduit.Cookies.Internal where

import qualified Network.HTTP.Types as W
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as U
import Text.Regex
import Data.Maybe
import qualified Data.List as L
import Data.Time.Clock
import Data.Time.Calendar
import Web.Cookie
import qualified Data.CaseInsensitive as CI
import Blaze.ByteString.Builder

import qualified Network.HTTP.Conduit.Request as Req
import qualified Network.HTTP.Conduit.Response as Res

slash :: Integral a => a
slash = 47 -- '/'

isIpAddress :: W.Ascii -> Bool
isIpAddress a = case strs of
  Just strs' -> helper strs'
  Nothing -> False
  where s = U.toString a
        regex = mkRegex "^([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})$"
        strs = matchRegex regex s
        helper l = length l == 4 && all helper2 l
        helper2 v = (read v :: Int) >= 0 && (read v :: Int) < 256

-- | This corresponds to the subcomponent algorithm entitled "Domain Matching" detailed
-- in section 5.1.3
domainMatches :: W.Ascii -> W.Ascii -> Bool
domainMatches string domainString
  | string == domainString = True
  | BS.length string < BS.length domainString + 1 = False
  | domainString `BS.isSuffixOf` string && BS.singleton (BS.last difference) == U.fromString "." && not (isIpAddress string) = True
  | otherwise = False
  where difference = BS.take (BS.length string - BS.length domainString) string

-- | This corresponds to the subcomponent algorithm entitled "Paths" detailed
-- in section 5.1.4
defaultPath :: Req.Request m -> W.Ascii
defaultPath req
  | BS.null uri_path = U.fromString "/"
  | BS.singleton (BS.head uri_path) /= U.fromString "/" = U.fromString "/"
  | BS.count slash uri_path <= 1 = U.fromString "/"
  | otherwise = BS.reverse $ BS.tail $ BS.dropWhile (/= slash) $ BS.reverse uri_path
  where uri_path = Req.path req

-- | This corresponds to the subcomponent algorithm entitled "Path-Match" detailed
-- in section 5.1.4
pathMatches :: W.Ascii -> W.Ascii -> Bool
pathMatches requestPath cookiePath
  | cookiePath == requestPath = True
  | cookiePath `BS.isPrefixOf` requestPath && BS.singleton (BS.last cookiePath) == U.fromString "/" = True
  | cookiePath `BS.isPrefixOf` requestPath && BS.singleton (BS.head remainder) == U.fromString "/" = True
  | otherwise = False
  where remainder = BS.drop (BS.length cookiePath) requestPath

-- This corresponds to the description of a cookie detailed in Section 5.3 "Storage Model"
data Cookie = Cookie
  { name :: W.Ascii
  , value :: W.Ascii
  , expiry_time :: UTCTime
  , domain :: W.Ascii
  , path :: W.Ascii
  , creation_time :: UTCTime
  , last_access_time :: UTCTime
  , persistent :: Bool
  , host_only :: Bool
  , secure_only :: Bool
  , http_only :: Bool
  }
  deriving (Show)
-- This corresponds to step 11 of the algorithm described in Section 5.3 "Storage Model"
instance Eq Cookie where
  (==) a b = name_matches && domain_matches && path_matches
    where name_matches = name a == name b
          domain_matches = domain a == domain b
          path_matches = path a == path b
instance Ord Cookie where
  compare c1 c2
    | BS.length (path c1) > BS.length (path c2) = LT
    | BS.length (path c1) < BS.length (path c2) = GT
    | creation_time c1 > creation_time c2 = GT
    | otherwise = LT

type CookieJar = [Cookie]

insertIntoCookieJar :: Cookie -> CookieJar -> CookieJar
insertIntoCookieJar = (:)

removeExistingCookieFromCookieJar :: Cookie -> CookieJar -> (Maybe Cookie, CookieJar)
removeExistingCookieFromCookieJar _ [] = (Nothing, [])
removeExistingCookieFromCookieJar c (c' : cs)
  | c == c' = (Just c', cs)
  | otherwise = (cookie', c' : cookie_jar')
  where (cookie', cookie_jar') = removeExistingCookieFromCookieJar c cs

-- | Are we configured to reject cookies for domains such as "com"?
rejectPublicSuffixes :: Bool
rejectPublicSuffixes = True

isPublicSuffix :: W.Ascii -> Bool
isPublicSuffix _ = False

-- | This corresponds to the eviction algorithm described in Section 5.3 "Storage Model"
evictExpiredCookies :: CookieJar -> UTCTime -> CookieJar
evictExpiredCookies cookie_jar now = filter (\ cookie -> expiry_time cookie >= now) cookie_jar

-- | This applies the computeCookieString to a given Request
insertCookiesIntoRequest :: Req.Request m -> CookieJar -> UTCTime -> (Req.Request m, CookieJar)
insertCookiesIntoRequest request cookie_jar now = (request {Req.requestHeaders = cookie_header : purgedHeaders}, cookie_jar')
  where purgedHeaders = L.deleteBy (\ (a, _) (b, _) -> a == b) (CI.mk $ U.fromString "Cookie", BS.empty) $ Req.requestHeaders request
        (cookie_string, cookie_jar') = computeCookieString request cookie_jar now True
        cookie_header = (CI.mk $ U.fromString "Cookie", cookie_string)

-- | This corresponds to the algorithm described in Section 5.4 "The Cookie Header"
computeCookieString :: Req.Request m -> CookieJar -> UTCTime -> Bool -> (W.Ascii, CookieJar)
computeCookieString request cookie_jar now is_http_api = (output_line, cookie_jar')
  where matching_cookie cookie = condition1 && condition2 && condition3 && condition4
          where condition1
                  | host_only cookie = Req.host request == domain cookie
                  | otherwise = domainMatches (Req.host request) (domain cookie)
                condition2 = pathMatches (Req.path request) (path cookie)
                condition3
                  | not (secure_only cookie) = True
                  | otherwise = Req.secure request
                condition4
                  | not (http_only cookie) = True
                  | otherwise = is_http_api
        matching_cookies = filter matching_cookie cookie_jar
        output_cookies =  map (\ c -> (name c, value c)) $ L.sort matching_cookies
        output_line = toByteString $ renderCookies $ output_cookies
        folding_function cookie_jar'' cookie = case removeExistingCookieFromCookieJar cookie cookie_jar'' of
          (Just c, cookie_jar''') -> insertIntoCookieJar (c {last_access_time = now}) cookie_jar'''
          (Nothing, cookie_jar''') -> cookie_jar'''
        cookie_jar' = foldl folding_function cookie_jar matching_cookies

-- | This applies the receiveSetCookie to a given Response
updateCookieJar :: Res.Response a -> Req.Request m -> UTCTime -> CookieJar -> (CookieJar, Res.Response a)
updateCookieJar response request now cookie_jar = (cookie_jar', response {Res.responseHeaders = other_headers})
  where (set_cookie_headers, other_headers) = L.partition ((== (CI.mk $ U.fromString "Set-Cookie")) . fst) $ Res.responseHeaders response
        set_cookie_data = map snd set_cookie_headers
        set_cookies = map parseSetCookie set_cookie_data
        cookie_jar' = foldl (\ cj sc -> receiveSetCookie sc request now True cj) cookie_jar set_cookies

-- | This corresponds to the algorithm described in Section 5.3 "Storage Model"
receiveSetCookie :: SetCookie -> Req.Request m -> UTCTime -> Bool -> CookieJar -> CookieJar
receiveSetCookie set_cookie request now is_http_api cookie_jar = case result of
  Nothing -> cookie_jar
  Just cookie_jar' -> cookie_jar'
  where result :: Maybe CookieJar
        result = do
          cookie <- generateCookie
          (cookie_jar', cookie') <- existanceTest cookie cookie_jar
          return $ insertIntoCookieJar cookie' cookie_jar'
        generateCookie :: Maybe Cookie
        generateCookie = do
          domain_sanitized <- sanitizeDomain $ step4 (setCookieDomain set_cookie)
          domain_intermediate <- step5 domain_sanitized
          (domain_final, host_only') <- step6 domain_intermediate
          http_only' <- step10
          return $ Cookie { name = setCookieName set_cookie
                          , value = setCookieValue set_cookie
                          , expiry_time = getExpiryTime (setCookieExpires set_cookie) (setCookieMaxAge set_cookie)
                          , domain = domain_final
                          , path = getPath $ setCookiePath set_cookie
                          , creation_time = now
                          , last_access_time = now
                          , persistent = getPersistent
                          , host_only = host_only'
                          , secure_only = setCookieSecure set_cookie
                          , http_only = http_only'
                          }
        sanitizeDomain domain'
          | has_a_character && BS.singleton (BS.last domain') == U.fromString "." = Nothing
          | has_a_character && BS.singleton (BS.head domain') == U.fromString "." = Just $ BS.tail domain'
          | otherwise = Just $ domain'
          where has_a_character = not (BS.null domain')
        step4 (Just set_cookie_domain) = set_cookie_domain
        step4 Nothing = BS.empty
        step5 domain'
          | firstCondition && domain' == (Req.host request) = return BS.empty
          | firstCondition = Nothing
          | otherwise = return domain'
          where firstCondition = rejectPublicSuffixes && isPublicSuffix domain'
        step6 domain'
          | firstCondition && not (domainMatches (Req.host request) domain') = Nothing
          | firstCondition = return (domain', False)
          | otherwise = return (Req.host request, True)
          where firstCondition = not $ BS.null domain'
        step10
          | not is_http_api && setCookieHttpOnly set_cookie = Nothing
          | otherwise = return $ setCookieHttpOnly set_cookie
        getExpiryTime :: Maybe UTCTime -> Maybe DiffTime -> UTCTime
        getExpiryTime _ (Just t) = (fromRational $ toRational t) `addUTCTime` now
        getExpiryTime (Just t) Nothing = t
        getExpiryTime Nothing Nothing= UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
        getPath (Just p) = p
        getPath Nothing = defaultPath request
        getPersistent = isJust (setCookieExpires set_cookie) || isJust (setCookieMaxAge set_cookie)
        existanceTest cookie cookie_jar' = existanceTestHelper cookie $ removeExistingCookieFromCookieJar cookie cookie_jar'
        existanceTestHelper new_cookie (Just old_cookie, cookie_jar')
          | not is_http_api && http_only old_cookie = Nothing
          | otherwise = return (cookie_jar', new_cookie {creation_time = creation_time old_cookie})
        existanceTestHelper new_cookie (Nothing, cookie_jar') = return (cookie_jar', new_cookie)
