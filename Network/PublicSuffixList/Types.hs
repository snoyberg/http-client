module Network.PublicSuffixList.Types where

import           Data.Default
import qualified Data.Map             as M

newtype Ord e => Tree e = Node { children :: M.Map e (Tree e) }
  deriving (Show)

instance Ord e => Default (Tree e) where
  def = Node M.empty

type DataStructure = (Tree String, Tree String)