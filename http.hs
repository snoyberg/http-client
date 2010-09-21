{-# LANGUAGE OverloadedStrings #-}
import Http
import Data.Attoparsec
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString as S

main = do
    print $ parse parseHeaders
        "HTTP/1.1 100 CONTINUE\n\nHTTP/1.1 200 OK\r\nfoo: bar\nbaz: baz\r\nhelper: bin\n\r\ncontent body"
    let x@(Done _ y) = parse parseChunks sample
    print x
    print $ S.concat y == expected

sample = pack $ concatMap ntorn $ unlines
    [ "25"
    , "This is the data in the first chunk"
    , ""
    , "1C; ignore this please"
    , "and this is the second one"
    , ""
    , "3"
    , "con"
    , "8"
    , "sequence"
    , "0"
    ]

expected = pack $ concatMap ntorn $ init $ unlines
    [ "This is the data in the first chunk"
    , "and this is the second one"
    , "consequence"
    ]

ntorn '\n' = "\r\n"
ntorn c = [c]
