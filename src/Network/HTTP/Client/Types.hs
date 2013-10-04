{-# LANGUAGE DeriveDataTypeable #-}
module Network.HTTP.Client.Types where

import Data.Typeable
import Network.HTTP.Types
import Control.Exception (Exception, IOException, SomeException)
import Data.Word (Word64)
import qualified Data.ByteString as S
import Data.Default
import Data.Monoid
import Data.Time (UTCTime)
import qualified Data.List as DL

data StatusHeaders = StatusHeaders !Status !HttpVersion !RequestHeaders
    deriving (Show, Eq, Ord)

data HttpException = StatusCodeException Status ResponseHeaders CookieJar
                   | InvalidUrlException String String
                   -- FIXME | TooManyRedirects [Response L.ByteString]  -- ^ List of encountered responses containing redirects in reverse chronological order; including last redirect, which triggered the exception and was not followed.
                   -- FIXME | UnparseableRedirect (Response L.ByteString) -- ^ Response containing unparseable redirect.
                   | TooManyRetries
                   | HttpParserException String
                   | HandshakeFailed
                   | OverlongHeaders
                   | ResponseTimeout
                   | FailedConnectionException String Int -- ^ host/port
                   | ExpectedBlankAfter100Continue
                   | InvalidStatusLine S.ByteString
                   | InvalidHeader S.ByteString
                   | InternalIOException IOException
                   | ProxyConnectException S.ByteString Int (Either S.ByteString HttpException) -- ^ host/port
                   | NoResponseDataReceived
                   | TlsException SomeException
                   | ResponseBodyTooShort Word64 Word64
                   -- ^ Expected size/actual size.
                   --
                   -- Since 1.9.4
                   | InvalidChunkHeaders
                   -- ^
                   --
                   -- Since 1.9.4
                   | IncompleteHeaders
    deriving (Show, Typeable)
instance Exception HttpException


-- This corresponds to the description of a cookie detailed in Section 5.3 \"Storage Model\"
data Cookie = Cookie
  { cookie_name :: S.ByteString
  , cookie_value :: S.ByteString
  , cookie_expiry_time :: UTCTime
  , cookie_domain :: S.ByteString
  , cookie_path :: S.ByteString
  , cookie_creation_time :: UTCTime
  , cookie_last_access_time :: UTCTime
  , cookie_persistent :: Bool
  , cookie_host_only :: Bool
  , cookie_secure_only :: Bool
  , cookie_http_only :: Bool
  }
  deriving (Read, Show)

newtype CookieJar = CJ { expose :: [Cookie] }
  deriving (Read, Show)

-- This corresponds to step 11 of the algorithm described in Section 5.3 \"Storage Model\"
instance Eq Cookie where
  (==) a b = name_matches && domain_matches && path_matches
    where name_matches = cookie_name a == cookie_name b
          domain_matches = cookie_domain a == cookie_domain b
          path_matches = cookie_path a == cookie_path b

instance Ord Cookie where
  compare c1 c2
    | S.length (cookie_path c1) > S.length (cookie_path c2) = LT
    | S.length (cookie_path c1) < S.length (cookie_path c2) = GT
    | cookie_creation_time c1 > cookie_creation_time c2 = GT
    | otherwise = LT

instance Default CookieJar where
  def = CJ []

instance Eq CookieJar where
  (==) cj1 cj2 = (DL.sort $ expose cj1) == (DL.sort $ expose cj2)

-- | Since 1.9
instance Monoid CookieJar where
  mempty = def
  (CJ a) `mappend` (CJ b) = CJ (DL.nub $ DL.sortBy compare' $ a `mappend` b)
    where compare' c1 c2 =
            -- inverse so that recent cookies are kept by nub over older
            if cookie_creation_time c1 > cookie_creation_time c2
                then LT
                else GT
