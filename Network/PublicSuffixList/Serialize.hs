module Network.PublicSuffixList.Serialize (getDataStructure, putDataStructure) where

import qualified Data.ByteString          as BS
import           Data.Functor
import           Data.Serialize.Get       hiding (getTreeOf)
import           Data.Serialize.Put
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Encoding.Error as TEE

import Network.PublicSuffixList.Types

getTreeOf :: Ord k => Get k -> Get (Tree k)
getTreeOf p = Node <$> getMapOf p (getTreeOf p)

getText :: Get T.Text
getText = (TE.decodeUtf8With TEE.lenientDecode . BS.pack) <$> getListOf getWord8

getDataStructure :: Get DataStructure
getDataStructure = getTwoOf (getTreeOf getText) (getTreeOf getText)

putTree :: Ord k => Putter k -> Putter (Tree k)
putTree p = putMapOf p (putTree p) . children

putText :: Putter T.Text
putText = putListOf putWord8 . BS.unpack . TE.encodeUtf8

putDataStructure :: Putter DataStructure
putDataStructure = putTwoOf (putTree putText) (putTree putText)

