module Network.HTTP.Client
    ( -- * Cookies
      module Network.HTTP.Client.Cookies
    , module Network.HTTP.Client.Manager -- FIXME
      -- * Request creation and modification
    , module Network.HTTP.Client.Request
      -- * Performing requests
    , withResponse
    , httpLbs
    , responseOpen
      -- * Types
    , HttpException (..)
    , Cookie (..)
    , CookieJar
    , Proxy (..)
      -- ** Request body
    , RequestBody (..)
    , Popper
    , NeedsPopper
    , GivesPopper
      -- ** Request
    , Request
    , method
    , secure
    , host
    , port
    , path
    , queryString
    , requestHeaders
    , requestBody
    , proxy
    , decompress
    , redirectCount
    , checkStatus
    , responseTimeout
    , cookieJar
      -- ** Response
    , Response
    , responseStatus
    , responseVersion
    , responseHeaders
    , responseBody
    , responseCookieJar
    , responseClose
    ) where

import Network.HTTP.Client.Cookies
import Network.HTTP.Client.Core
import Network.HTTP.Client.Manager
import Network.HTTP.Client.Request
import Network.HTTP.Client.Response
import Network.HTTP.Client.Types
