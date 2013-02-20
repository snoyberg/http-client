module Network.PublicSuffixList.Internal.Internal (getDataStructure, putDataStructure) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as U8
import           Data.Functor
import           Data.Serialize.Get hiding (getTreeOf)
import           Data.Serialize.Put
import qualified Data.Text            as T

import Network.PublicSuffixList.Internal.Types

getTreeOf :: Ord k => Get k -> Get (Tree k)
getTreeOf p = Node <$> getMapOf p (getTreeOf p)

getText :: Get T.Text
getText = (T.pack . U8.toString . BS.pack) <$> getListOf getWord8

getDataStructure :: Get DataStructure
getDataStructure = getTwoOf (getTreeOf getText) (getTreeOf getText)

putTree :: Ord k => Putter k -> Putter (Tree k)
putTree p = putMapOf p (putTree p) . children

putText :: Putter T.Text
putText = putListOf putWord8 . BS.unpack . U8.fromString . T.unpack

putDataStructure :: Putter DataStructure
putDataStructure = putTwoOf (putTree putText) (putTree putText)

