{-|
This script downloads the public suffix list from mozilla's website, and uses
Network.PublicSuffixList.Create.sink to construct an opaque data structure which can
be used with the isSuffix function in Network.PublicSuffixList.Lookup. It then
generates a source file with the contents of this data structure so that
applications can link against this source file and get knowledget of public suffixes
without doing anything at runtime.
-}

import qualified Data.Conduit         as C
import           Data.Time.Clock
import qualified Network.HTTP.Conduit as HC
import           System.IO

import           Network.PublicSuffixList.Create
import           Network.PublicSuffixList.Types


generateDataStructure :: String -> IO (DataStructure, UTCTime)
generateDataStructure url = do
  req <- HC.parseUrl url
  out <- HC.withManager $ \ manager -> do
    res <- HC.http req manager
    HC.responseBody res C.$$+- sink
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
    hPutStrLn h "{-|"
    hPutStrLn h $ "The opaque data structure that 'isSuffix' can query. This data structure was generated at " ++ show current_time
    hPutStrLn h "-}"
    hPutStrLn h "dataStructure :: DataStructure"
    hPutStrLn h $ "dataStructure = " ++ (show ds)
