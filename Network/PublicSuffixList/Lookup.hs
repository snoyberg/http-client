module Network.PublicSuffixList.Lookup (matches) where

import qualified Data.Map          as M
import           Data.Maybe
import qualified Data.String.Utils as SU

import           Network.PublicSuffixList.DataStructure
import           Network.PublicSuffixList.Types

matches :: String -> Bool
matches s
  | recurse False (snd dataStructure) $ ps = False
  | isNothing $ getNext (fst dataStructure) $ head ps = True
  | otherwise = recurse True (fst dataStructure) $ ps
  where ps = reverse $ SU.split "." s
        getNext t s' = case M.lookup s' $ children t of
          Nothing -> M.lookup "*" $ children t
          j -> j
        recurse _ _ [] = True
        recurse d t [c] = case getNext t c of
          Nothing -> False
          Just c' -> case M.null $ children c' of
            True -> True
            False -> d
        recurse d t (c : cs) = case getNext t c of
          Nothing -> False
          Just t' -> recurse d t' cs