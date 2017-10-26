module Network.HTTP.Client.Socket
  ( module Network.Socket
  , module Network.Socket.ByteString
  ) where

import Network.Socket hiding (recv, recvFrom, send, sendTo)
import Network.Socket.ByteString
