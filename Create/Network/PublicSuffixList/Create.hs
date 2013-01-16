{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-|
This script parses the public suffix list, and constructs a data structure which can
be used with the isSuffix function in Lookup.hs. It exports a GSink which produces
the opaque 'DataStructure' and can be fed any Source as input.

This makes an few assumption about the information in the public suffix list:
namely, that no rule is a suffix of another rule. For example, if there is a rule
abc.def.ghi
then then is no other rule
def.ghi
or
!def.ghi

The actual data structure involved here is a tree where the nodes have no value and
the edges are DNS labels. There are two trees: one to handle the exception rules,
and one to handle the regular rules.
-}

module Network.PublicSuffixList.Create (PublicSuffixListException, sink) where

import           Control.Exception
import qualified Data.ByteString      as BS
import qualified Data.Conduit         as C
import qualified Data.Conduit.List    as CL
import qualified Data.Conduit.Text    as CT
import           Data.Default
import qualified Data.Map             as M
import qualified Data.Text            as T
import           Data.Typeable
import           Text.IDNA

import           Network.PublicSuffixList.Internal.Types

data PublicSuffixListException = PublicSuffixListException
  deriving (Show, Typeable)

instance Exception PublicSuffixListException

insert :: (Ord e) => Tree e -> [e] -> Tree e
insert _ [] = def
insert t (p : ps) = case M.lookup p $ children t of
  Nothing -> t { children = M.insert p (insert def ps) $ children t }
  Just l -> t { children = M.insert p (insert l ps) $ children t }

foldingFunction :: DataStructure -> T.Text -> DataStructure
foldingFunction d@(rules, exceptions) s'
  | T.null s = d
  | T.take 2 s == "//" = d
  | T.head s == '!' = (rules, insert exceptions $ labelList $ T.tail s)
  | otherwise = (insert rules $ labelList s, exceptions)
  where ss = filter (not . T.null) $ T.words s'
        s
          | null ss = ""
          | otherwise = head ss
        labelList = reverse . map internationalize . T.split (== '.')
        internationalize str
          | str == "*" = str
          | otherwise = case toASCII False True $ T.toLower str of
                          Just x -> x
                          Nothing -> throw PublicSuffixListException

{-
Generate the opaque 'DataStructure'
-}
sink :: C.MonadThrow m => C.GSink BS.ByteString m DataStructure
sink = CT.decode CT.utf8 C.>+> CT.lines C.>+> CL.fold foldingFunction def
