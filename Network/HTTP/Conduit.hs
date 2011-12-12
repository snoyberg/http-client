{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
-- | This module contains everything you need to initiate HTTP connections.  If
-- you want a simple interface based on URLs, you can use 'simpleHttp'. If you
-- want raw power, 'http' is the underlying workhorse of this package. Some
-- examples:
--
-- > -- Just download an HTML document and print it.
-- > import Network.HTTP.Enumerator
-- > import qualified Data.ByteString.Lazy as L
-- >
-- > main = simpleHttp "http://www.haskell.org/" >>= L.putStr
--
-- This example uses interleaved IO to write the response body to a file in
-- constant memory space. By using 'httpRedirect', it will automatically
-- follow 3xx redirects.
--
-- > import Data.Enumerator
-- > import Data.Enumerator.Binary
-- > import Network.HTTP.Enumerator
-- > import System.IO
-- >
-- > main :: IO ()
-- > main = withFile "google.html" WriteMode $ \handle -> do
-- >     request <- parseUrl "http://google.com/"
-- >     withManager $ \manager -> do
-- >         run_ $ httpRedirect request (\_ _ -> iterHandle handle) manager
--
-- The following headers are automatically set by this module, and should not
-- be added to 'requestHeaders':
--
-- * Content-Length
--
-- * Host
--
-- * Accept-Encoding (not currently set, but client usage of this variable /will/ cause breakage).
--
-- Any network code on Windows requires some initialization, and the network
-- library provides withSocketsDo to perform it. Therefore, proper usage of
-- this library will always involve calling that function at some point.  The
-- best approach is to simply call them at the beginning of your main function,
-- such as:
--
-- > import Network.HTTP.Enumerator
-- > import qualified Data.ByteString.Lazy as L
-- > import Network (withSocketsDo)
-- >
-- > main = withSocketsDo
-- >      $ simpleHttp "http://www.haskell.org/" >>= L.putStr
module Network.HTTP.Conduit
    ( -- * Perform a request
      simpleHttp
    , httpLbs
    , httpLbsRedirect
    , http
    , httpRedirect
    , redirectIter
      -- * Datatypes
    , Proxy (..)
    , RequestBody (..)
    , Response (..)
      -- ** Request
    , Request
    , def
    , method
    , secure
    , checkCerts
    , host
    , port
    , path
    , queryString
    , requestHeaders
    , requestBody
    , proxy
    , rawBody
    , decompress
      -- *** Defaults
    , defaultCheckCerts
      -- * Manager
    , Manager
    , newManager
      -- * Utility functions
    , parseUrl
    , applyBasicAuth
    , addProxy
    , semiParseUrl
    , lbsIter
      -- * Decompression predicates
    , alwaysDecompress
    , browserDecompress
      -- * Request bodies
    , urlEncodedBody
      -- * Exceptions
    , HttpException (..)
    ) where

import Network.HTTP.Conduit.ConnInfo
import Network.HTTP.Conduit.Manager
import Network (connectTo, PortID (PortNumber))

import qualified Network.Socket as NS
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Conduit.Parser
import Control.Exception (Exception, bracket, SomeException)
import Control.Exception.Lifted (mask, try, throwIO)
import Control.Arrow (first)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Base (liftBase)
import Control.Monad.Trans.Class (lift)
import Control.Failure (Failure (failure))
import Data.Typeable (Typeable)
import Codec.Binary.UTF8.String (encodeString)
import qualified Data.Text as T
import qualified Blaze.ByteString.Builder as Blaze
import Blaze.ByteString.Builder.HTTP (chunkedTransferEncoding, chunkedTransferTerminator)
import Data.Monoid (Monoid (..))
import qualified Network.HTTP.Types as W
import qualified Data.CaseInsensitive as CI
import Data.Int (Int64)
import qualified Codec.Zlib.Enum as Z
import Control.Monad.Trans.Resource (with, release)
#if 1
-- FIXME MIN_VERSION_monad_control(0,3,0)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)
#else
import Control.Monad.IO.Control (MonadControlIO, liftBaseOp)
#endif
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IORef as I
import Control.Applicative ((<$>))
import Data.Certificate.X509 (X509)
import Network.TLS.Extra (certificateVerifyChain, certificateVerifyDomain)
import qualified Data.ByteString.Base64 as B64
import System.IO (hClose, hFlush)
import Blaze.ByteString.Builder (toByteString)
import Data.Maybe (fromMaybe)
import Data.Default (Default (def))
import Numeric (showHex)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Network.HTTP.Conduit.Request
import Network.HTTP.Conduit.Util

-- | Add a Basic Auth header (with the specified user name and password) to the
-- given Request. Ignore error handling:
--
--    applyBasicAuth "user" "pass" $ fromJust $ parseUrl url

applyBasicAuth :: S.ByteString -> S.ByteString -> Request m -> Request m
applyBasicAuth user passwd req =
    req { requestHeaders = authHeader : requestHeaders req }
  where
    authHeader = (CI.mk "Authorization", basic)
    basic = S8.append "Basic " (B64.encode $ S8.concat [ user, ":", passwd ])


-- | Add a proxy to the the Request so that the Request when executed will use
-- the provided proxy.
addProxy :: S.ByteString -> Int -> Request m -> Request m
addProxy hst prt req =
    req { proxy = Just $ Proxy hst prt }


-- | A simple representation of the HTTP response created by 'lbsIter'.
data Response = Response
    { statusCode :: Int
    , responseHeaders :: W.ResponseHeaders
    , responseBody :: L.ByteString
    }
    deriving (Show, Read, Eq, Typeable)

enumSingle :: a
enumSingle = error "379 enumSingle"
{- FIXME
enumSingle :: Monad m => a -> Enumerator a m b
enumSingle x (Continue k) = k $ Chunks [x]
enumSingle _ step = returnI step
-}

-- | Always decompress a compressed stream.
alwaysDecompress :: ContentType -> Bool
alwaysDecompress = const True

-- | Decompress a compressed stream unless the content-type is 'application/x-tar'.
browserDecompress :: ContentType -> Bool
browserDecompress = (/= "application/x-tar")


-- | The most low-level function for initiating an HTTP request.
--
-- The first argument to this function gives a full specification on the
-- request: the host to connect to, whether to use SSL, headers, etc. Please
-- see 'Request' for full details.
--
-- The second argument specifies how the response should be handled. It's a
-- function that takes two arguments: the first is the HTTP status code of the
-- response, and the second is a list of all response headers. This module
-- exports 'lbsIter', which generates a 'Response' value.
--
-- Note that this allows you to have fully interleaved IO actions during your
-- HTTP download, making it possible to download very large responses in
-- constant memory.
http
     :: MonadBaseControl IO m
     => Request m
     -> (W.Status -> W.ResponseHeaders -> C.BSource m S.ByteString -> ResourceT m a)
     -> Manager
     -> ResourceT m a
http req@(Request {..}) bodyStep m = do
    (bsrc, sink) <- withConn req m useConn
    (_FIXMEbool, a) <- getResponse req bodyStep bsrc
    return a
  where
    useConn :: ConnInfo -> ResourceT m (Bool, a)
    useConn = error "useConn"
    (contentLength, bodyEnum) =
        case requestBody of
            RequestBodyLBS lbs -> (Just $ L.length lbs, enumSingle $ Blaze.fromLazyByteString lbs)
            RequestBodyBS bs -> (Just $ fromIntegral $ S.length bs, enumSingle $ Blaze.fromByteString bs)
            RequestBodyBuilder i b -> (Just $ i, enumSingle b)
            RequestBodySource i enum -> (Just i, enum)
            -- FIXME RequestBodySourceChunked enum -> (Nothing, \step -> enum C.$$ chunkIt C.=$ step)
    hh
        | port == 80 && not secure = host
        | port == 443 && secure = host
        | otherwise = host `mappend` S8.pack (':' : show port)
    contentLengthHeader (Just contentLength') =
            if method `elem` ["GET", "HEAD"] && contentLength' == 0
                then id
                else (:) ("Content-Length", S8.pack $ show contentLength')
    contentLengthHeader Nothing = (:) ("Transfer-Encoding", "chunked")
    headers' = ("Host", hh)
                 : (contentLengthHeader contentLength)
                 (("Accept-Encoding", "gzip") : requestHeaders)
    requestHeaders' =
            Blaze.fromByteString method
            `mappend` Blaze.fromByteString " "
            `mappend`
                (if error "FIXME useProxy"
                    then Blaze.fromByteString (if secure then "https://" else "http://")
                            `mappend` Blaze.fromByteString hh
                    else mempty)
            `mappend`
                (case S8.uncons path of
                    Just ('/', _) -> Blaze.fromByteString path
                    _ -> Blaze.fromByteString "/"
                            `mappend` Blaze.fromByteString path)
            `mappend` (if null queryString
                        then mempty
                        else W.renderQueryBuilder True queryString)
            `mappend` Blaze.fromByteString " HTTP/1.1\r\n"
            `mappend` mconcat (flip map headers' $ \(k, v) ->
                Blaze.fromByteString (CI.original k)
                `mappend`  Blaze.fromByteString ": "
                `mappend` Blaze.fromByteString v
                `mappend` Blaze.fromByteString "\r\n")
            `mappend` Blaze.fromByteString "\r\n"
    requestEnum sink = CL.fromList [requestHeaders'] C.<$$> sink

getResponse :: MonadBaseControl IO m
            => Request m
            -> (W.Status -> [W.Header] -> C.BSource m S8.ByteString -> ResourceT m a)
            -> C.BSource m S8.ByteString
            -> ResourceT m (Bool, a)
getResponse Request {..} bodyStep bsrc = do
    ((_, sc, sm), hs) <- bsrc C.$$ sinkHeaders
    let s = W.Status sc sm
    let hs' = map (first CI.mk) hs
    let mcl = lookup "content-length" hs'
    let body' = error "472" {-
            case (rawBody, ("transfer-encoding", "chunked") `elem` hs') of
                (False, True) -> (chunkedEnumeratee =$)
                (True , True) -> (chunkedTerminator =$)
                (_    , False) -> case mcl >>= readMay . S8.unpack of
                                      Just len -> (takeLBS len =$)
                                      Nothing  -> id-}
    let decompresser = error "decompresser 489" {-
            if needsGunzip hs'
                then (Z.ungzip C.=$)
                else id -}
    -- RFC 2616 section 4.4_1 defines responses that must not include a body
    res <-
        if hasNoBody method sc
            then do
                bsrcNull <- C.bsourceM $ CL.fromList []
                bodyStep s hs' bsrcNull
            else body' $ decompresser $ do
                    x <- bodyStep s hs'
                    -- FIXME flushStream
                    return x

    -- should we put this connection back into the connection manager?
    let toPut = Just "close" /= lookup "connection" hs'
    return (toPut, res)
  where
    hasNoBody :: S8.ByteString -- ^ request method
              -> Int -- ^ status code
              -> Bool
    hasNoBody "HEAD" _ = True
    hasNoBody _ 204 = True
    hasNoBody _ 304 = True
    hasNoBody _ i = 100 <= i && i < 200

    needsGunzip :: [W.Header] -> Bool
    needsGunzip hs' =
            not rawBody
         && ("content-encoding", "gzip") `elem` hs'
         && decompress (fromMaybe "" $ lookup "content-type" hs')

flushStream :: MonadBaseControl IO m => C.SinkM a m ()
flushStream = do
    x <- CL.head
    case x of
        Nothing -> return ()
        Just _ -> flushStream

{- FIXME
chunkedEnumeratee :: MonadBaseControl IO m => Enumeratee S.ByteString S.ByteString m a
chunkedEnumeratee k@(Continue _) = do
    len <- catchParser "Chunk header" iterChunkHeader
    if len == 0
        then return k
        else do
            k' <- takeLBS len k
            catchParser "End of chunk newline" iterNewline
            chunkedEnumeratee k'
chunkedEnumeratee step = return step

chunkedTerminator :: MonadBaseControl IO m => Enumeratee S.ByteString S.ByteString m a
chunkedTerminator (Continue k) = do
    len <- catchParser "Chunk header" iterChunkHeader
    k' <- sendCont k $ S8.pack $ showHex len "\r\n"
    if len == 0
        then return k'
        else do
            step <- takeLBS len k'
            catchParser "End of chunk newline" iterNewline
            case step of
                Continue k'' -> do
                    k''' <- sendCont k'' "\r\n"
                    chunkedTerminator k'''
                _ -> return step
chunkedTerminator step = return step

sendCont :: Monad m
         => (Stream S8.ByteString -> Iteratee S8.ByteString m a)
         -> S8.ByteString
         -> Iteratee S8.ByteString m (Step S8.ByteString m a)
sendCont k bs = lift $ runIteratee $ k $ Chunks [bs]

chunkIt :: Monad m => Enumeratee Blaze.Builder Blaze.Builder m a
chunkIt = checkDone $ continue . step
  where
    step k EOF = k (Chunks [chunkedTransferTerminator]) >>== return
    step k (Chunks []) = continue $ step k
    step k (Chunks xs) = k (Chunks [chunkedTransferEncoding $ mconcat xs])
                         >>== chunkIt
-}

chunkIt :: a
chunkIt = error "561 chunkIt"


takeLBS :: Int -> C.ConduitM S.ByteString m S.ByteString
takeLBS = error "takeLBS"
-- FIXME this is just isolate
{-
takeLBS :: MonadBaseControl IO m => Int -> Enumeratee S.ByteString S.ByteString m a
takeLBS 0 step = return step
takeLBS len (Continue k) = do
    mbs <- EL.head
    case mbs of
        Nothing -> return $ Continue k
        Just bs -> do
            let (len', chunk, rest) =
                    if S.length bs > len
                        then (0, S.take len bs,
                                if S.length bs == len
                                    then Chunks []
                                    else Chunks [S.drop len bs])
                        else (len - S.length bs, bs, Chunks [])
            step' <- lift $ runIteratee $ k $ Chunks [chunk]
            if len' == 0
                then yield step' rest
                else takeLBS len' step'
takeLBS _ step = return step
-}

encodeUrlCharPI :: Bool -> Char -> String
encodeUrlCharPI _ '/' = "/"
encodeUrlCharPI False '?' = "?"
encodeUrlCharPI False '&' = "&"
encodeUrlCharPI False '=' = "="
encodeUrlCharPI _ c = encodeUrlChar c

encodeUrlChar :: Char -> String
encodeUrlChar c
    -- List of unreserved characters per RFC 3986
    -- Gleaned from http://en.wikipedia.org/wiki/Percent-encoding
    | 'A' <= c && c <= 'Z' = [c]
    | 'a' <= c && c <= 'z' = [c]
    | '0' <= c && c <= '9' = [c]
encodeUrlChar c@'-' = [c]
encodeUrlChar c@'_' = [c]
encodeUrlChar c@'.' = [c]
encodeUrlChar c@'~' = [c]
encodeUrlChar ' ' = "+"
encodeUrlChar y =
    let (a, c) = fromEnum y `divMod` 16
        b = a `mod` 16
        showHex' x
            | x < 10 = toEnum $ x + (fromEnum '0')
            | x < 16 = toEnum $ x - 10 + (fromEnum 'A')
            | otherwise = error $ "Invalid argument to showHex: " ++ show x
     in ['%', showHex' b, showHex' c]

-- | Convert a URL into a 'Request'.
--
-- This defaults some of the values in 'Request', such as setting 'method' to
-- GET and 'requestHeaders' to @[]@.
--
-- Since this function uses 'Failure', the return monad can be anything that is
-- an instance of 'Failure', such as 'IO' or 'Maybe'.
parseUrl :: Failure HttpException m => String -> m (Request m')
parseUrl = parseUrlHelper True

-- | Same as 'parseUrl', with one distinction: this function will not attempt
-- to parse the query string, but instead leave it with the path info. This can
-- be useful if you need precise control of the rendering of the query string,
-- such as using semicolons instead of ampersands.
semiParseUrl :: Failure HttpException m => String -> m (Request m')
semiParseUrl = parseUrlHelper False

parseUrlHelper :: Failure HttpException m => Bool -> String -> m (Request m')
parseUrlHelper parsePath s@('h':'t':'t':'p':':':'/':'/':rest) = parseUrl1 s False parsePath rest
parseUrlHelper parsePath s@('h':'t':'t':'p':'s':':':'/':'/':rest) = parseUrl1 s True parsePath rest
parseUrlHelper _ x = failure $ InvalidUrlException x "Invalid scheme"

parseUrl1 :: Failure HttpException m
          => String -> Bool -> Bool -> String -> m (Request m')
parseUrl1 full sec parsePath s =
    parseUrl2 full sec parsePath s'
  where
    s' = encodeString s

defaultCheckCerts :: W.Ascii -> [X509] -> IO TLSCertificateUsage
defaultCheckCerts host' certs =
    case certificateVerifyDomain (S8.unpack host') certs of
        CertificateUsageAccept -> certificateVerifyChain certs
        _                          -> return CertificateUsageAccept

instance Default (Request m) where
    def = Request
        { host = "localhost"
        , port = 80
        , secure = False
        , checkCerts = defaultCheckCerts
        , requestHeaders = []
        , path = "/"
        , queryString = []
        , requestBody = RequestBodyLBS L.empty
        , method = "GET"
        , proxy = Nothing
        , rawBody = False
        , decompress = alwaysDecompress
        }

parseUrl2 :: Failure HttpException m
          => String -> Bool -> Bool -> String -> m (Request m')
parseUrl2 full sec parsePath s = do
    port' <- mport
    return def
        { host = S8.pack hostname
        , port = port'
        , secure = sec
        , path = S8.pack
                    $ if null path''
                            then "/"
                            else concatMap (encodeUrlCharPI parsePath) path''
        , queryString = if parsePath
                            then W.parseQuery $ S8.pack qstring
                            else []
        }
  where
    (beforeSlash, afterSlash) = break (== '/') s
    (hostname, portStr) = break (== ':') beforeSlash
    (path', qstring') = break (== '?') afterSlash
    path'' = if parsePath then path' else afterSlash
    qstring'' = case qstring' of
                '?':x -> x
                _ -> qstring'
    qstring = takeWhile (/= '#') qstring''
    mport =
        case (portStr, sec) of
            ("", False) -> return 80
            ("", True) -> return 443
            (':':rest, _) ->
                case readMay rest of
                    Just i -> return i
                    Nothing -> failure $ InvalidUrlException full "Invalid port"
            x -> error $ "parseUrl1: this should never happen: " ++ show x

-- | Convert the HTTP response into a 'Response' value.
--
-- Even though a 'Response' contains a lazy bytestring, this function does
-- /not/ utilize lazy I/O, and therefore the entire response body will live in
-- memory. If you want constant memory usage, you'll need to write your own
-- iteratee and use 'http' or 'httpRedirect' directly.
lbsIter :: MonadBaseControl IO m
        => W.Status
        -> W.ResponseHeaders
        -> C.BSource m S.ByteString
        -> ResourceT m Response
lbsIter (W.Status sc _) hs bsrc = do
    lbs <- fmap L.fromChunks $ bsrc C.$$ CL.consume
    return $ Response sc hs lbs

-- | Download the specified 'Request', returning the results as a 'Response'.
--
-- This is a simplified version of 'http' for the common case where you simply
-- want the response data as a simple datatype. If you want more power, such as
-- interleaved actions on the response body during download, you'll need to use
-- 'http' directly. This function is defined as:
--
-- @httpLbs = http lbsIter@
--
-- Please see 'lbsIter' for more information on how the 'Response' value is
-- created.
--
-- Even though a 'Response' contains a lazy bytestring, this function does
-- /not/ utilize lazy I/O, and therefore the entire response body will live in
-- memory. If you want constant memory usage, you'll need to write your own
-- iteratee and use 'http' or 'httpRedirect' directly.
httpLbs :: MonadBaseControl IO m => Request m -> Manager -> m Response
httpLbs req m = runResourceT (http req lbsIter m)

-- | Download the specified URL, following any redirects, and return the
-- response body.
--
-- This function will 'throwIO' an 'HttpException' for any response with a
-- non-2xx status code. It uses 'parseUrl' to parse the input. This function
-- essentially wraps 'httpLbsRedirect'.
--
-- Note: Even though this function returns a lazy bytestring, it does /not/
-- utilize lazy I/O, and therefore the entire response body will live in
-- memory. If you want constant memory usage, you'll need to write your own
-- iteratee and use 'http' or 'httpRedirect' directly.
simpleHttp :: MonadBaseControl IO m => String -> m L.ByteString
simpleHttp url = runResourceT $ do
    url' <- liftBase $ parseUrl url
    man <- newManager
    Response sc _ b <- httpLbsRedirect url'
        { decompress = browserDecompress
        } man
    if 200 <= sc && sc < 300
        then return b
        else liftBase $ throwIO $ StatusCodeException sc b

data HttpException = StatusCodeException Int L.ByteString
                   | InvalidUrlException String String
                   | TooManyRedirects
                   | HttpParserException String
    deriving (Show, Typeable)
instance Exception HttpException

-- | Same as 'http', but follows all 3xx redirect status codes that contain a
-- location header.
httpRedirect
    :: MonadBaseControl IO m
    => Request m
    -> (W.Status -> W.ResponseHeaders -> C.BSource m S.ByteString -> ResourceT m a)
    -> Manager
    -> ResourceT m a
httpRedirect req bodyStep manager =
    http req (redirectIter 10 req bodyStep manager) manager

-- | Make a request automatically follow 3xx redirects.
--
-- Used internally by 'httpRedirect' and family.
redirectIter :: MonadBaseControl IO m
             => Int -- ^ number of redirects to attempt
             -> Request m -- ^ Original request
             -> (W.Status -> W.ResponseHeaders -> C.BSource m S.ByteString -> ResourceT m a)
             -> Manager
             -> (W.Status -> W.ResponseHeaders -> C.BSource m S.ByteString -> ResourceT m a)
redirectIter redirects req bodyStep manager s@(W.Status code _) hs bsrc
    | 300 <= code && code < 400 =
        case lookup "location" hs of
            Just l'' -> do
                -- Prepend scheme, host and port if missing
                let l' =
                        case S8.uncons l'' of
                            Just ('/', _) -> concat
                                [ "http"
                                , if secure req then "s" else ""
                                , "://"
                                , S8.unpack $ host req
                                , ":"
                                , show $ port req
                                , S8.unpack l''
                                ]
                            _ -> S8.unpack l''
                l <- liftBase $ parseUrl l'
                let req' = req
                        { host = host l
                        , port = port l
                        , secure = secure l
                        , path = path l
                        , queryString = queryString l
                        , method =
                            if code == 303
                                then "GET"
                                else method l
                        }
                if redirects == 0
                    then liftBase $ throwIO TooManyRedirects
                    else (http req') (redirectIter (redirects - 1) req' bodyStep manager) manager
            Nothing -> bodyStep s hs bsrc
    | otherwise = bodyStep s hs bsrc

-- | Download the specified 'Request', returning the results as a 'Response'
-- and automatically handling redirects.
--
-- This is a simplified version of 'httpRedirect' for the common case where you
-- simply want the response data as a simple datatype. If you want more power,
-- such as interleaved actions on the response body during download, you'll
-- need to use 'httpRedirect' directly. This function is defined as:
--
-- @httpLbsRedirect = httpRedirect lbsIter@
--
-- Please see 'lbsIter' for more information on how the 'Response' value is
-- created.
--
-- Even though a 'Response' contains a lazy bytestring, this function does
-- /not/ utilize lazy I/O, and therefore the entire response body will live in
-- memory. If you want constant memory usage, you'll need to write your own
-- iteratee and use 'http' or 'httpRedirect' directly.
httpLbsRedirect :: MonadBaseControl IO m => Request m -> Manager -> ResourceT m Response
httpLbsRedirect req m = httpRedirect req lbsIter m

readMay :: Read a => String -> Maybe a
readMay s = case reads s of
                [] -> Nothing
                (x, _):_ -> Just x

-- FIXME add a helper for generating POST bodies

-- | Add url-encoded paramters to the 'Request'.
--
-- This sets a new 'requestBody', adds a content-type request header and
-- changes the 'method' to POST.
urlEncodedBody :: Monad m => [(S.ByteString, S.ByteString)] -> Request m' -> Request m
urlEncodedBody headers req = req
    { requestBody = RequestBodyLBS body
    , method = "POST"
    , requestHeaders =
        (ct, "application/x-www-form-urlencoded")
      : filter (\(x, _) -> x /= ct) (requestHeaders req)
    }
  where
    ct = "Content-Type"
    body = L.fromChunks . return $ W.renderSimpleQuery False headers

catchParser :: String -> a -> a -- FIXME
--catchParser s i = catchError i (const $ throwError $ HttpParserException s)
catchParser _ = id
