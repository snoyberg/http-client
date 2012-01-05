{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
import Network.HTTP.Conduit
import Network
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import System.Environment.UTF8 (getArgs)
import Data.CaseInsensitive (original)
import Data.Conduit
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = withSocketsDo $ do
    [url] <- getArgs
    _req2 <- parseUrl url
    {-
    let req = urlEncodedBody
                [ ("foo", "bar")
                , ("baz%%38**.8fn", "bin")
                ] _req2
    -}
    runResourceT $ do
        man <- newManager
        Response sc hs b <- httpLbsRedirect _req2 man
#if DEBUG
        return ()
#else
        liftIO $ do
            print sc
            mapM_ (\(x, y) -> do
                S.putStr $ original x
                putStr ": "
                S.putStr y
                putStrLn "") hs
            putStrLn ""
            L.putStr b
#endif
    printOpenSockets
