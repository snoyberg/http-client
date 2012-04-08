module Network.PublicSuffixList.Create where

import qualified Data.Conduit         as C
import qualified Data.Conduit.Text    as CT
import qualified Data.Conduit.List    as CL
import           Data.Default
import qualified Data.Map             as M
import qualified Data.String.Utils    as SU
import qualified Data.Text            as T
import qualified Network.HTTP.Conduit as HC
import           System.IO

import           Network.PublicSuffixList.Types

insert :: (Ord e) => Tree e -> [e] -> Tree e
insert t [p] = t { children = M.insert p def $ children t }
insert t (p : ps) = case M.lookup p $ children t of
  Nothing -> t { children = M.insert p (insert def ps) $ children t }
  Just l -> t { children = M.insert p (insert l ps) $ children t }

getSubTree :: Ord e => Tree e -> e -> Maybe (Tree e)
getSubTree t k = M.lookup k $ children t

linesConduit :: Monad m => C.Conduit T.Text m T.Text
linesConduit = C.NeedInput (f T.empty) (close T.empty)
  where f x s = g $ (T.append x $ head l) : (tail l)
          where l = T.lines s
        g [x] = C.NeedInput (f x) (close x)
        g (x : xs) = C.HaveOutput (g xs) (return ()) x
        close x = C.Done (if T.null x then Nothing else Just x) ()

textToStringConduit :: Monad m => C.Conduit T.Text m String
textToStringConduit = C.NeedInput (\ x -> C.HaveOutput textToStringConduit (return ()) $ T.unpack x) (C.Done Nothing ())

foldingFunction :: DataStructure -> String -> DataStructure
foldingFunction d@(rules, exceptions) s'
  | take 2 s' == "//" = d
  | null s = d
  | head s == '!' = (rules, insert exceptions $ reverse $ SU.split "." $ tail s)
  | otherwise = (insert rules $ reverse $ SU.split "." s, exceptions)
  where ss = SU.splitWs s'
        s
          | null ss = ""
          | otherwise = head ss

generateDataStructure :: String -> IO DataStructure
generateDataStructure url = do
  req <- HC.parseUrl url
  HC.withManager $ \ manager -> do
    res <- HC.http req manager
    HC.responseBody res C.$= CT.decode CT.utf8 C.$= linesConduit C.$= textToStringConduit C.$$ CL.fold foldingFunction def

main :: IO ()
main = do
  ds <- generateDataStructure "http://mxr.mozilla.org/mozilla-central/source/netwerk/dns/effective_tld_names.dat?raw=1"
  withFile "Network/PublicSuffixList/DataStructure.hs" WriteMode $ \ h -> do
    hPutStrLn h "module Network.PublicSuffixList.DataStructure where"
    hPutStrLn h ""
    hPutStrLn h "import Data.Map"
    hPutStrLn h ""
    hPutStrLn h "import Network.PublicSuffixList.Types"
    hPutStrLn h ""
    hPutStrLn h "dataStructure :: DataStructure"
    hPutStrLn h $ "dataStructure = " ++ (show ds)