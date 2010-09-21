{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Enumerator
import OpenSSL
import Network
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = withSocketsDo $ withOpenSSL $ do
    Response sc sm hs b <- http $ Request
        { host = "localhost"
        , port = 80
        , secure = False
        , requestHeaders = []
        , path = "/"
        , queryString = [("foo", "bar")]
        , requestBody = L8.pack "baz=bin"
        , method = "POST"
        }
    print (sc, sm)
    mapM_ (\(x, y) -> do
        S.putStr x
        putStr ": "
        S.putStr y
        putStrLn "") hs
    putStrLn ""
    L.putStr b
