module Network.HTTP.Client.Socket
  ( module Network.HTTP.Client.Socket
  , module System.Socket
  , module System.Socket.Family.Inet
  , module System.Socket.Protocol.TCP
  , module System.Socket.Type.Stream
  ) where

import Data.Word
import System.Socket
import System.Socket.Family.Inet
import System.Socket.Protocol.TCP
import System.Socket.Type.Stream

type AddrInfo = AddressInfo Inet Stream TCP

type HostAddress = (Word8, Word8, Word8, Word8)

type Socket' = Socket Inet Stream TCP
