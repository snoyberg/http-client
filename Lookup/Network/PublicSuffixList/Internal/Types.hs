{-|
This module is only exported for the use of the 'publicsuffixlistcreate' package.
Every one else should consider everything in this file to be opaque.
-}

module Network.PublicSuffixList.Internal.Types where

import qualified Data.ByteString      as BS
import           Data.Default
import qualified Data.Map             as M
import qualified Data.Text            as T
import           Data.Serialize.Get hiding (getTreeOf)
import           Data.Serialize.Put
import qualified Data.ByteString.UTF8 as U8
import           Data.Functor

newtype Tree e = Node { children :: M.Map e (Tree e) }
  deriving (Show, Eq)

putTree :: Ord k => Putter k -> Putter (Tree k)
putTree p = putMapOf p (putTree p) . children

getTreeOf :: Ord k => Get k -> Get (Tree k)
getTreeOf p = Node <$> getMapOf p (getTreeOf p)

putText :: Putter T.Text
putText = putListOf putWord8 . BS.unpack . U8.fromString . T.unpack

getText :: Get T.Text
getText = (T.pack . U8.toString . BS.pack) <$> getListOf getWord8

putDataStructure :: Putter DataStructure
putDataStructure = putTwoOf (putTree putText) (putTree putText)

getDataStructure :: Get DataStructure
getDataStructure = getTwoOf (getTreeOf getText) (getTreeOf getText)

instance Ord e => Default (Tree e) where
  def = Node M.empty

type DataStructure = (Tree T.Text, Tree T.Text)
