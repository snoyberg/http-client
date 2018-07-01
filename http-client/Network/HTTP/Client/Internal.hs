{-# OPTIONS_HADDOCK not-home #-}
-- | Note that this is essentially the \"kitchen sink\" export module,
-- including many functions intended only to be used internally by this
-- package. No API stability is guaranteed for this module. If you see
-- functions here which you believe should be promoted to a stable API, please
-- contact the author.
module Network.HTTP.Client.Internal
    ( -- * Low-level response body handling
      module Network.HTTP.Client.Body
      -- * Raw connection handling
    , module Network.HTTP.Client.Connection
      -- * Cookies
    , module Network.HTTP.Client.Cookies
      -- * Performing requests
    , module Network.HTTP.Client.Core
      -- * Parse response headers
    , module Network.HTTP.Client.Headers
      -- * Request helper functions
    , module Network.HTTP.Client.Request
      -- * Low-level response body handling
    , module Network.HTTP.Client.Response
      -- * Manager
    , module Network.HTTP.Client.Manager
      -- * All types
    , module Network.HTTP.Client.Types
      -- * Various utilities
    , module Network.HTTP.Client.Util
    , dummyManaged
    ) where

import Network.HTTP.Client.Body
import Network.HTTP.Client.Connection
import Network.HTTP.Client.Cookies
import Network.HTTP.Client.Core
import Network.HTTP.Client.Headers
import Network.HTTP.Client.Manager
import Network.HTTP.Client.Request
import Network.HTTP.Client.Response
import Network.HTTP.Client.Types
import Network.HTTP.Client.Util
import Data.KeyedPool (dummyManaged)
