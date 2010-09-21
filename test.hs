{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Enumerator
import OpenSSL
import Network
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = withSocketsDo $ withOpenSSL $ do
    (hs, b) <- http $ Request
        { host = "localhost"
        , port = 80
        , secure = False
        , headers = []
        , path = "/"
        , queryString = [("foo", "bar")]
        , body = L8.pack "baz=bin"
        , method = "POST"
        }
    mapM_ (\(x, y) -> do
        S.putStr x
        putStr ": "
        S.putStr y
        putStrLn "") hs
    putStrLn ""
    S.putStr b
