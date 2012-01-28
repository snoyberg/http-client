{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
import Network.HTTP.Conduit
import Network
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import System.Environment.UTF8 (getArgs, getEnv)
import Data.CaseInsensitive (original)
import Data.Conduit
import Control.Monad.IO.Class (liftIO)
import Control.Exception (finally)
import Network.Socks5 (SocksConf(..), defaultSocksConf)

mproxify sockshost req
	| sockshost == "" = req
	| otherwise       = req { socksProxy = Just $ defaultSocksConf sockshost 1080 }

main :: IO ()
main = withSocketsDo $ do
    [url] <- getArgs
    proxy <- catch (getEnv "SOCKS_PROXY") (const $ return "")
    _req2 <- mproxify proxy `fmap` parseUrl url
    {-
    let req = urlEncodedBody
                [ ("foo", "bar")
                , ("baz%%38**.8fn", "bin")
                ] _req2
    -}
    flip finally printOpenSockets $ runResourceT $ do
        man <- newManager
        Response sc hs b <- httpLbs _req2 man
        liftIO $ do
            print sc
            mapM_ (\(x, y) -> do
                S.putStr $ original x
                putStr ": "
                S.putStr y
                putStrLn "") hs
            putStrLn ""
            L.putStr b
