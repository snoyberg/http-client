{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
import Network.HTTP.Enumerator
import Network
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import System.Environment.UTF8 (getArgs)
import Network.Wai (ciOriginal)
import qualified Data.Ascii as A

main :: IO ()
main = withSocketsDo $ do
    [urlS] <- getArgs
    urlA <- maybe (error "Invalid ASCII sequence") return $ A.fromChars urlS
    _req2 <- parseUrl urlA
    {-
    let req = urlEncodedBody
                [ ("foo", "bar")
                , ("baz%%38**.8fn", "bin")
                ] _req2
    -}
    Response sc hs b <- withManager $ httpLbsRedirect _req2
#if DEBUG
    return ()
#else
    print sc
    mapM_ (\(x, y) -> do
        S.putStr $ A.ciToByteString x
        putStr ": "
        S.putStr $ A.toByteString y
        putStrLn "") hs
    putStrLn ""
    L.putStr b
#endif
