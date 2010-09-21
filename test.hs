{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Enumerator
import OpenSSL
import Network
import qualified Data.ByteString as S

main :: IO ()
main = withSocketsDo $ withOpenSSL $ do
    (hs, b) <- http $ Request "localhost" 80 False []
    mapM_ (\(x, y) -> do
        S.putStr x
        putStr ": "
        S.putStr y
        putStrLn "") hs
    putStrLn ""
    S.putStr b
