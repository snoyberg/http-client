-- | This module implements the algorithms described in RFC 6265 for the Network.HTTP.Conduit library.
module Network.HTTP.Conduit.Cookies
  ( Cookie(..)
  , CookieJar
  , createCookieJar
  , destroyCookieJar
  , updateCookieJar
  , receiveSetCookie
  , insertCookiesIntoRequest
  , computeCookieString
  , evictExpiredCookies
  ) where

import Network.HTTP.Conduit.Cookies.Internal
