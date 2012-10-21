{-# LANGUAGE OverloadedStrings #-}

{-|
This script downloads the public suffix list from mozilla's website, parses it, and
constructs a data structure which can be used with the isSuffix function in Lookup.hs.
It then generates a source file with the contents of this data structure so that
applications can link against this source file and get knowledget of public suffixes
without doing anything at runtime.

This scripts makes an few assumption about the information in the public suffix list:
namely, that no rule is a suffix of another rule. For example, if there is a rule
abc.def.ghi
then then is no other rule
def.ghi
or
!def.ghi

The actual data structure involved here is a tree where the nodes have no value and
the edges are DNS labels. There are two trees: one to handle the exception rules,
and one to handle the regular rules.
|-}

module Network.PublicSuffixList.Create where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as UTF8BS
import qualified Data.Conduit         as C
import qualified Data.Conduit.List    as CL
import qualified Data.Conduit.Text    as CT
import           Data.Default
import qualified Data.Map             as M
import qualified Data.String.Utils    as SU
import qualified Data.Text            as T
import           Data.Time.Clock
import qualified Network.HTTP.Conduit as HC
import           System.IO
import           Text.IDNA

import           Network.PublicSuffixList.Types

insert :: (Ord e) => Tree e -> [e] -> Tree e
insert t [] = def
insert t (p : ps) = case M.lookup p $ children t of
  Nothing -> t { children = M.insert p (insert def ps) $ children t }
  Just l -> t { children = M.insert p (insert l ps) $ children t }

getSubTree :: Ord e => Tree e -> e -> Maybe (Tree e)
getSubTree t k = M.lookup k $ children t

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
        internationalize s
          | s == "*" = s
          | otherwise = case toASCII False True $ T.toLower s of
                          Just x -> x
                          Nothing -> throw PublicSuffixListException

generateDataStructure :: String -> IO (DataStructure, UTCTime)
generateDataStructure url = do
  req <- HC.parseUrl url
  out <- HC.withManager $ \ manager -> do
    res <- HC.http req manager
    HC.responseBody res C.$$+- CT.decode CT.utf8 C.=$ CT.lines C.=$ CL.fold foldingFunction def
  current_time <- getCurrentTime
  putStrLn $ "Fetched Public Suffix List at " ++ show current_time
  return (out, current_time)

main :: IO ()
main = do
  (ds, current_time) <- generateDataStructure "http://mxr.mozilla.org/mozilla-central/source/netwerk/dns/effective_tld_names.dat?raw=1"
  withFile "Network/PublicSuffixList/DataStructure.hs" WriteMode $ \ h -> do
    hPutStrLn h "{-# LANGUAGE OverloadedStrings #-}"
    hPutStrLn h ""
    hPutStrLn h $ "-- DO NOT MODIFY! This file has been automatically generated from the Create.hs script at " ++ show current_time
    hPutStrLn h ""
    hPutStrLn h "module Network.PublicSuffixList.DataStructure where"
    hPutStrLn h ""
    hPutStrLn h "import Data.Map"
    hPutStrLn h ""
    hPutStrLn h "import Network.PublicSuffixList.Types"
    hPutStrLn h ""
    hPutStrLn h "dataStructure :: DataStructure"
    hPutStrLn h $ "dataStructure = " ++ (show ds)
