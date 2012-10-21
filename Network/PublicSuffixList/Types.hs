{-# LANGUAGE DeriveDataTypeable #-}

module Network.PublicSuffixList.Types where

import           Control.Exception
import           Data.Default
import qualified Data.Map             as M
import qualified Data.Text            as T
import           Data.Typeable

data PublicSuffixListException = PublicSuffixListException
  deriving (Show, Typeable)

instance Exception PublicSuffixListException

newtype Tree e = Node { children :: M.Map e (Tree e) }
  deriving (Show)

instance Ord e => Default (Tree e) where
  def = Node M.empty

type DataStructure = (Tree T.Text, Tree T.Text)
