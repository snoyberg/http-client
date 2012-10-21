{-# LANGUAGE OverloadedStrings #-}
module Network.PublicSuffixList.Lookup (isSuffix) where

import qualified Data.Map          as M
import           Data.Maybe
import qualified Data.Text         as T

import           Network.PublicSuffixList.DataStructure
import           Network.PublicSuffixList.Types

data LookupResult = Inside | AtLeaf | OffEnd
  deriving (Eq)

{-|
This function returns whether or not this domain is owned by a
registrar or a regular person. True means that this is a registrar
domain; False means it's owned by a person. This is used to determine
if a cookie is allowed to bet set for a particular domain. For
example, you shouldn't be able to set a cookie for "com".

Note that this function expects lowercase ASCII strings. These strings
should be gotten from the toASCII algorithm as described in RFC 3490.
These strings should not start or end with the '.' character, and should
not have two '.' characters next to each other.
(The toASCII alrogithm is implemented in the 'idna' hackage package,
though that package doesn't always map strings to lowercase)
|-}
isSuffix :: T.Text -> Bool
isSuffix s
  -- Any TLD is a suffix
  | length ps == 1 = True
  -- Only match against the exception rules if we have a full match
  | exceptionResult == AtLeaf = False
  -- If we have a subdomain on an existing rule, we're not a suffix
  | rulesResult == OffEnd = False
  -- Otherwise, we're a suffix of a suffix, which is a suffix
  | otherwise = True
  where ps = reverse $ T.split (== '.') s
        exceptionResult = recurse ps $ snd dataStructure
        rulesResult = recurse ps $ fst dataStructure
        getNext :: Tree T.Text -> T.Text -> Maybe (Tree T.Text)
        getNext t s' = case M.lookup s' $ children t of
          Nothing -> M.lookup "*" $ children t
          j -> j
        recurse :: [T.Text] -> Tree T.Text -> LookupResult
        recurse [] t
          | M.null $ children t = AtLeaf
          | otherwise = Inside
        recurse (c : cs) t = case getNext t c of
          Nothing -> OffEnd
          Just t' -> recurse cs t'
