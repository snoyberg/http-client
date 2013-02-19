{-|
This script downloads the public suffix list from mozilla's website, and uses
Network.PublicSuffixList.Create.sink to construct an opaque data structure which can
be used with the isSuffix function in Network.PublicSuffixList.Lookup. It then
generates a source file with the contents of this data structure so that
applications can link against this source file and get knowledget of public suffixes
without doing anything at runtime.
-}

import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as U8
import qualified Data.Conduit         as C
import           Data.Serialize.Put
import qualified Data.Text            as T
import           Data.Time.Clock
import qualified Network.HTTP.Conduit as HC
import           System.IO

import           Network.PublicSuffixList.Create
import           Network.PublicSuffixList.Internal.Types


generateDataStructure :: String -> IO (DataStructure, UTCTime)
generateDataStructure url = do
  req <- HC.parseUrl url
  out <- HC.withManager $ \ manager -> do
    res <- HC.http req manager
    HC.responseBody res C.$$+- sink
  current_time <- getCurrentTime
  putStrLn $ "Fetched Public Suffix List at " ++ show current_time
  return (out, current_time)

putTree :: Ord k => Putter k -> Putter (Tree k)
putTree p = putMapOf p (putTree p) . children

putText :: Putter T.Text
putText = putListOf putWord8 . BS.unpack . U8.fromString . T.unpack

putDataStructure :: Putter DataStructure
putDataStructure = putTwoOf (putTree putText) (putTree putText)

main :: IO ()
main = do
  (ds, current_time) <- generateDataStructure "http://mxr.mozilla.org/mozilla-central/source/netwerk/dns/effective_tld_names.dat?raw=1"
  withFile "../Lookup/Network/PublicSuffixList/DataStructure.hs" WriteMode $ \ h -> do
    hPutStrLn h "{-# LANGUAGE OverloadedStrings #-}"
    hPutStrLn h ""
    hPutStrLn h $ "-- DO NOT MODIFY! This file has been automatically generated from the Create.hs script at " ++ show current_time
    hPutStrLn h ""
    hPutStrLn h "module Network.PublicSuffixList.DataStructure (dataStructure) where"
    hPutStrLn h ""
    hPutStrLn h "import qualified Data.ByteString      as BS"
    hPutStrLn h "import           Data.ByteString.Char8 ()"
    hPutStrLn h "import qualified Data.ByteString.UTF8 as U8"
    hPutStrLn h "import           Data.Functor"
    hPutStrLn h "import           Data.Serialize.Get hiding (getTreeOf)"
    hPutStrLn h "import qualified Data.Text as T"
    hPutStrLn h ""
    hPutStrLn h "import Network.PublicSuffixList.Internal.Types"
    hPutStrLn h ""
    hPutStrLn h "getTreeOf :: Ord k => Get k -> Get (Tree k)"
    hPutStrLn h "getTreeOf p = Node <$> getMapOf p (getTreeOf p)"
    hPutStrLn h ""
    hPutStrLn h "getText :: Get T.Text"
    hPutStrLn h "getText = (T.pack . U8.toString . BS.pack) <$> getListOf getWord8"
    hPutStrLn h ""
    hPutStrLn h "getDataStructure :: Get DataStructure"
    hPutStrLn h "getDataStructure = getTwoOf (getTreeOf getText) (getTreeOf getText)"
    hPutStrLn h ""
    hPutStrLn h "{-|"
    hPutStrLn h $ "The opaque data structure that 'isSuffix' can query. This data structure was generated at " ++ show current_time
    hPutStrLn h "-}"
    hPutStrLn h "dataStructure :: DataStructure"
    hPutStrLn h "dataStructure = let Right ds = runGet getDataStructure serializedDataStructure in ds"
    hPutStrLn h ""
    hPutStrLn h "serializedDataStructure :: BS.ByteString"
    hPutStrLn h $ "serializedDataStructure = " ++ (show $ runPut $ putDataStructure ds)
