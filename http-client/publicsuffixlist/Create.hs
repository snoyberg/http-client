{-|
This script downloads the public suffix list from mozilla's website, and uses
Network.PublicSuffixList.Create.sink to construct an opaque data structure which can
be used with the isSuffix function in Network.PublicSuffixList.Lookup. It then
generates a source file with the contents of this data structure so that
applications can link against this source file and get knowledget of public suffixes
without doing anything at runtime.
-}

import qualified Data.ByteString        as BS
import qualified Data.ByteString.UTF8   as U8
import qualified Data.Conduit           as C
import qualified Data.Text              as T
import           Data.Time.Clock
import qualified Network.HTTP.Conduit   as HC
import           Data.Conduit.Binary (conduitFile)
import           System.IO

import           Network.PublicSuffixList.Create
import           Network.PublicSuffixList.Types
import           Network.PublicSuffixList.Serialize


generateDataStructure :: String -> IO (DataStructure, UTCTime)
generateDataStructure url = do
  req <- HC.parseUrl url
  out <- HC.withManager $ \ manager -> do
    res <- HC.http req manager
    HC.responseBody res C.$$+- conduitFile "effective_tld_names.dat" C.=$ sink
  current_time <- getCurrentTime
  putStrLn $ "Fetched Public Suffix List at " ++ show current_time
  return (out, current_time)

main :: IO ()
main = do
  (ds, current_time) <- generateDataStructure "http://mxr.mozilla.org/mozilla-central/source/netwerk/dns/effective_tld_names.dat?raw=1"
  withFile "Network/PublicSuffixList/DataStructure.hs" WriteMode $ \ h -> do
    hPutStrLn h "{-# LANGUAGE OverloadedStrings #-}"
    hPutStrLn h "{-# LANGUAGE CPP #-}"
    hPutStrLn h ""
    hPutStrLn h $ "-- DO NOT MODIFY! This file has been automatically generated from the Create.hs script at " ++ show current_time
    hPutStrLn h ""
    hPutStrLn h "module Network.PublicSuffixList.DataStructure (dataStructure) where"
    hPutStrLn h ""
    hPutStrLn h "import           Data.ByteString.Char8 ()"
    hPutStrLn h ""
    hPutStrLn h "import Network.PublicSuffixList.Types"
    hPutStrLn h "#if !defined(RUNTIMELIST)"
    hPutStrLn h "import qualified Data.ByteString      as BS"
    hPutStrLn h "import Network.PublicSuffixList.Serialize"
    hPutStrLn h "#else"
    hPutStrLn h "import qualified Network.PublicSuffixList.Create as PSLC"
    hPutStrLn h "import qualified Data.Conduit as C"
    hPutStrLn h "import Data.Conduit.Binary (sourceFile)"
    hPutStrLn h "import System.IO.Unsafe (unsafePerformIO)"
    hPutStrLn h "#endif"
    hPutStrLn h ""
    hPutStrLn h "-- We could just put the raw data structure here, but if we do that, there will be lots of"
    hPutStrLn h "-- static string literals, which makes GHC really slow when compiling. Instead, we can manually"
    hPutStrLn h "-- serialize the datastructure ourself, so there's only one string literal."
    hPutStrLn h ""
    hPutStrLn h "{-|"
    hPutStrLn h $ "The opaque data structure that 'isSuffix' can query. This data structure was generated at " ++ show current_time
    hPutStrLn h "-}"
    hPutStrLn h "dataStructure :: DataStructure"
    hPutStrLn h "#if defined(RUNTIMELIST)"
    hPutStrLn h "{-# NOINLINE dataStructure #-}"
    hPutStrLn h "dataStructure = unsafePerformIO $ C.runResourceT $ sourceFile RUNTIMELIST C.$$ PSLC.sink"
    hPutStrLn h "#else"
    hPutStrLn h "dataStructure = getDataStructure serializedDataStructure"
    hPutStrLn h ""
    hPutStrLn h "serializedDataStructure :: BS.ByteString"
    hPutStrLn h $ "serializedDataStructure = " ++ (show $ putDataStructure ds)
    hPutStrLn h ""
    hPutStrLn h "#endif"
