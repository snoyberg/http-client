{-# LANGUAGE OverloadedStrings #-}
module Network.PublicSuffixList.Lookup (effectiveTLDPlusOne, effectiveTLDPlusOne', isSuffix, isSuffix') where

import qualified Data.List         as L
import qualified Data.Map          as M
import           Data.Maybe (isNothing)
import qualified Data.Text         as T

import qualified Network.PublicSuffixList.DataStructure as DS
import           Network.PublicSuffixList.Types

data LookupResult = Inside | AtLeaf | OffEnd T.Text
  deriving (Eq)

{-|
This function returns whether or not this domain is owned by a
registrar or a regular person. 'Nothing' means that this is a registrar
domain; 'Just x' means it's owned by a person. This is used to determine
if a cookie is allowed to bet set for a particular domain. For
example, you shouldn't be able to set a cookie for \"com\".

If the value is 'Just x', then the x value is what is known as the
effective TLD plus one. This is one segment more than the suffix of the
domain. For example, the eTLD+1 for "this.is.a.subdom.com" is Just
"subdom.com"

Note that this function expects lowercase ASCII strings. These strings
should be gotten from the toASCII algorithm as described in RFC 3490.
These strings should not start or end with the \'.\' character, and should
not have two \'.\' characters next to each other.
(The toASCII algorithm is implemented in the \'idna\' hackage package,
though that package doesn't always map strings to lowercase)
-}
effectiveTLDPlusOne' :: DataStructure -> T.Text -> Maybe T.Text
effectiveTLDPlusOne' dataStructure s
  -- Any TLD is a suffix
  | length ps == 1 = Nothing
  -- Only match against the exception rules if we have a full match
  | exceptionResult == AtLeaf = Just s
  | otherwise = case rulesResult of
  -- If we have a subdomain on an existing rule, we're not a suffix
      OffEnd x -> Just x
  -- Otherwise, we're a suffix of a suffix, which is a suffix
      _ -> Nothing
  where ps = reverse $ T.split (== '.') s
        exceptionResult = recurse ps [] $ snd dataStructure
        rulesResult = recurse ps [] $ fst dataStructure
        getNext :: Tree T.Text -> T.Text -> Maybe (Tree T.Text)
        getNext t s' = case M.lookup s' $ children t of
          Nothing -> M.lookup "*" $ children t
          j -> j
        recurse :: [T.Text] -> [T.Text] -> Tree T.Text -> LookupResult
        recurse [] _ t
          | M.null $ children t = AtLeaf
          | otherwise = Inside
        recurse (c : cs) prev t = case getNext t c of
          Nothing -> OffEnd $ T.concat $ L.intersperse (T.pack ".") (c : prev)
          Just t' -> recurse cs (c : prev) t'

-- | >>> effectiveTLDPlusOne = effectiveTLDPlusOne' Network.PublicSuffixList.DataStructure.dataStructure
effectiveTLDPlusOne :: T.Text -> Maybe T.Text
effectiveTLDPlusOne = effectiveTLDPlusOne' DS.dataStructure

-- | >>> isSuffix' dataStructure = isNothing . effectiveTLDPlusOne' dataStructure
isSuffix' :: DataStructure -> T.Text -> Bool
isSuffix' dataStructure = isNothing . effectiveTLDPlusOne' dataStructure

-- | >>> isSuffix = isSuffix' Network.PublicSuffixList.DataStructure.dataStructure
isSuffix :: T.Text -> Bool
isSuffix = isNothing . effectiveTLDPlusOne
