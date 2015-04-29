{-# LANGUAGE OverloadedStrings #-}
module Network.PublicSuffixList.Lookup (effectiveTLDPlusOne, effectiveTLDPlusOne', isSuffix, isSuffix') where

import qualified Data.Map          as M
import           Data.Maybe (isNothing)
import qualified Data.Text         as T

import qualified Network.PublicSuffixList.DataStructure as DS
import           Network.PublicSuffixList.Types

{-|
OffEnd's Bool argument represents whether we fell off a
leaf or whether we fell off a non-leaf. True means that
we fell off a leaf. Its Text argument is the component
that pushed us off the end, along with all the components
to the right of that one, interspersed with "."s
-}
data LookupResult = Inside | AtLeaf | OffEnd Bool T.Text
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
  | length ss == 1 = Nothing
  | otherwise = output rulesResult exceptionResult
  where ss = T.splitOn "." s
        ps = reverse ss
        exceptionResult = recurse ps [] $ snd dataStructure
        rulesResult = recurse ps [] $ fst dataStructure
        -- If we fell off, did we do it at a leaf? Otherwise, what's the
        -- subtree that we're at
        getNext :: Tree T.Text -> T.Text -> Either Bool (Tree T.Text)
        getNext t s' = case M.lookup s' $ children t of
          Nothing -> Left (M.null $ children t)
          Just t' -> Right t'
        -- Look up the component we're looking for...
        getNextWithStar t s' = case getNext t s' of
          -- and if that fails, look up "*"
          Left _ -> getNext t "*"
          r -> r
        recurse :: [T.Text] -> [T.Text] -> Tree T.Text -> LookupResult
        recurse [] _ t
          | M.null $ children t = AtLeaf
          | otherwise = Inside
        recurse (c : cs) prev t = case getNextWithStar t c of
          Left b -> OffEnd b $ T.intercalate "." (c : prev)
          Right t' -> recurse cs (c : prev) t'
        -- Only match against the exception rules if we have a full match
        output _ AtLeaf = Just s
        output _ (OffEnd True x) = Just $ T.intercalate "." $ tail $ T.splitOn "." x
        -- If we have a subdomain on an existing rule, we're not a suffix
        output (OffEnd _ x) _
          -- A single level domain can never be a eTLD+1
          | isNothing $ T.find (== '.') x = Just $ T.intercalate "." $ drop (length ss - 2) ss
          | otherwise = Just x
        -- Otherwise, we're a suffix of a suffix, which is a suffix
        output _ _ = Nothing

-- | >>> effectiveTLDPlusOne = effectiveTLDPlusOne' Network.PublicSuffixList.DataStructure.dataStructure
effectiveTLDPlusOne :: T.Text -> Maybe T.Text
effectiveTLDPlusOne = effectiveTLDPlusOne' DS.dataStructure

-- | >>> isSuffix' dataStructure = isNothing . effectiveTLDPlusOne' dataStructure
isSuffix' :: DataStructure -> T.Text -> Bool
isSuffix' dataStructure = isNothing . effectiveTLDPlusOne' dataStructure

-- | >>> isSuffix = isSuffix' Network.PublicSuffixList.DataStructure.dataStructure
isSuffix :: T.Text -> Bool
isSuffix = isNothing . effectiveTLDPlusOne
