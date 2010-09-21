{-# LANGUAGE OverloadedStrings #-}
module Http where

import Prelude hiding (take)
import Data.Attoparsec
import Data.Attoparsec.Enumerator
import Data.Enumerator (Iteratee)
import qualified Data.ByteString as S
import Control.Applicative
import Data.Word (Word8)

type Header = (S.ByteString, S.ByteString)

parseHeader :: Parser Header
parseHeader = do
    k <- takeWhile1 notNewlineColon
    _ <- word8 58 -- colon
    skipWhile isSpace
    v <- takeWhile1 notNewline
    newline
    return (k, v)

notNewlineColon, isSpace, notNewline :: Word8 -> Bool

notNewlineColon 10 = False -- LF
notNewlineColon 13 = False -- CR
notNewlineColon 58 = False -- colon
notNewlineColon _  = True

isSpace 32 = True
isSpace _  = False

notNewline 10 = False
notNewline 13 = False
notNewline _  = True

newline :: Parser ()
newline =
    lf <|> (cr >> lf)
  where
    word8' x = word8 x >> return ()
    lf = word8' 10
    cr = word8' 13

parseHeaders :: Parser (Status, [Header])
parseHeaders = do
    s <- parseStatus
    h <- manyTill parseHeader newline
    return (s, h)

iterHeaders :: Monad m => Iteratee S.ByteString m (Status, [Header])
iterHeaders = iterParser parseHeaders

type Status = (S.ByteString, S.ByteString, S.ByteString)

parseStatus :: Parser Status
parseStatus = do
    _ <- string "HTTP/"
    ver <- takeWhile1 $ not . isSpace
    _ <- word8 32 -- space
    statCode <- takeWhile1 $ not . isSpace
    _ <- word8 32
    statMsg <- takeWhile1 $ notNewline
    newline
    if (statCode == "100")
        then newline >> parseStatus
        else return (ver, statCode, statMsg)

iterChunks :: Monad m => Iteratee S.ByteString m [S.ByteString]
iterChunks = iterParser parseChunks

parseChunks :: Parser [S.ByteString]
parseChunks = manyTill parseChunk zeroChunk

zeroChunk :: Parser ()
zeroChunk = word8 48 >> (newline <|> attribs) -- 0

parseChunk :: Parser S.ByteString
parseChunk = do
    len <- hexs
    newline <|> attribs
    bs <- take len
    newline
    return bs

attribs :: Parser ()
attribs = do
    _ <- word8 59 -- colon
    skipWhile notNewline
    newline

hexs :: Parser Int
hexs = do
    ws <- many1 hex
    return $ foldl1 (\a b -> a * 16 + b) $ map fromIntegral ws

hex :: Parser Word8
hex =
    (digit <|> upper <|> lower) <?> "Hexadecimal digit"
  where
    digit = do
        d <- satisfy $ \w -> (w >= 48 && w <= 57)
        return $ d - 48
    upper = do
        d <- satisfy $ \w -> (w >= 65 && w <= 70)
        return $ d - 55
    lower = do
        d <- satisfy $ \w -> (w >= 97 && w <= 102)
        return $ d - 87

{-
iterParserTill :: Monad m
               => Parser a
               -> Parser end
               -> E.Enumeratee a S.ByteString m b
iterParserTill p pend =
    E.continue $ step $ parse p
  where
    step parse (E.Chunks xs) = parseLoop parse xs
    step parse E.EOF = case parse S.empty of
        Done extra a -> E.yield a $ if S.null extra
            then E.Chunks []
            else E.Chunks [extra]
        Partial _ -> err [] "iterParser: divergent parser"
        Fail _ ctx msg -> err ctx msg

    parseLoop parse [] = E.continue (step parse)
    parseLoop parse (x:xs) = case parse x of
        Done extra a -> E.yield a $ if S.null extra
            then E.Chunks xs
            else E.Chunks (extra:xs)
        Partial parse' -> parseLoop parse' xs
        Fail _ ctx msg -> err ctx msg

    err ctx msg = E.throwError (ParseError ctx msg)
-}
