{-|
This module is only exported for the use of the 'publicsuffixlistcreate' package.
Every one else should consider everything in this file to be opaque.
-}

module Network.PublicSuffixList.Internal.Types where

import           Data.Default
import qualified Data.Map             as M
import qualified Data.Text            as T

newtype Tree e = Node { children :: M.Map e (Tree e) }
  deriving (Show)

instance Ord e => Default (Tree e) where
  def = Node M.empty

type DataStructure = (Tree T.Text, Tree T.Text)
