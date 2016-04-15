{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.HTTP.Client.Request
    ( parseUrl
    , setUriRelative
    , getUri
    , setUri
    , browserDecompress
    , alwaysDecompress
    , addProxy
    , applyBasicAuth
    , applyBasicProxyAuth
    , urlEncodedBody
    , needsGunzip
    , requestBuilder
    , useDefaultTimeout
    , setQueryString
    , streamFile
    , observedStreamFile
    , extractBasicAuthInfo
    ) where

import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (mempty, mappend)
import Data.String (IsString(..))
import Data.Char (toLower)
import Control.Applicative ((<$>))
import Control.Monad (when, unless, guard)
import Numeric (showHex)

import Data.Default.Class (Default (def))

import Blaze.ByteString.Builder (Builder, fromByteString, fromLazyByteString, toByteStringIO, flush)
import Blaze.ByteString.Builder.Char8 (fromChar, fromShow)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal (defaultChunkSize)

import qualified Network.HTTP.Types as W
import Network.URI (URI (..), URIAuth (..), parseURI, relativeTo, escapeURIString, unEscapeString, isAllowedInURI, isReserved)

import Control.Monad.IO.Class (liftIO)
import Control.Exception (Exception, toException, throw, throwIO, IOException)
import qualified Control.Exception as E
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Base64 as B64

import Network.HTTP.Client.Types
import Network.HTTP.Client.Util
import Network.HTTP.Client.Connection

import Network.HTTP.Client.Util (readDec, (<>))
import Data.Time.Clock
import Control.Monad.Catch (MonadThrow, throwM)
import Data.IORef

import System.IO (withBinaryFile, hTell, hFileSize, Handle, IOMode (ReadMode))
import Control.Monad (liftM)

-- | Convert a URL into a 'Request'.
--
-- This defaults some of the values in 'Request', such as setting 'method' to
-- GET and 'requestHeaders' to @[]@.
--
-- Since this function uses 'MonadThrow', the return monad can be anything that is
-- an instance of 'MonadThrow', such as 'IO' or 'Maybe'.
--
-- You can place the request method at the beginning of the URL separated by a
-- space, e.g.:
--
-- @@@
-- parseUrl "POST http://httpbin.org/post"
-- @@@
--
-- Note that the request method must be provided as all capital letters.
--
-- Since 0.1.0
parseUrl :: MonadThrow m => String -> m Request
parseUrl s' =
    case parseURI (encode s) of
        Just uri -> liftM setMethod (setUri def uri)
        Nothing  -> throwM $ InvalidUrlException s "Invalid URL"
  where
    encode = escapeURIString isAllowedInURI
    (mmethod, s) =
        case break (== ' ') s' of
            (x, ' ':y) | all (\c -> 'A' <= c && c <= 'Z') x -> (Just x, y)
            _ -> (Nothing, s')

    setMethod req =
        case mmethod of
            Nothing -> req
            Just m -> req { method = S8.pack m }

-- | Add a 'URI' to the request. If it is absolute (includes a host name), add
-- it as per 'setUri'; if it is relative, merge it with the existing request.
setUriRelative :: MonadThrow m => Request -> URI -> m Request
setUriRelative req uri =
#ifndef MIN_VERSION_network
#define MIN_VERSION_network(x,y,z) 1
#endif
#if MIN_VERSION_network(2,4,0)
    setUri req $ uri `relativeTo` getUri req
#else
    case uri `relativeTo` getUri req of
        Just uri' -> setUri req uri'
        Nothing   -> throwM $ InvalidUrlException (show uri) "Invalid URL"
#endif

-- | Extract a 'URI' from the request.
--
-- Since 0.1.0
getUri :: Request -> URI
getUri req = URI
    { uriScheme = if secure req
                    then "https:"
                    else "http:"
    , uriAuthority = Just URIAuth
        { uriUserInfo = ""
        , uriRegName = S8.unpack $ host req
        , uriPort = ':' : show (port req)
        }
    , uriPath = S8.unpack $ path req
    , uriQuery =
        case S8.uncons $ queryString req of
            Just (c, _) | c /= '?' -> '?' : (S8.unpack $ queryString req)
            _ -> S8.unpack $ queryString req
    , uriFragment = ""
    }

applyAnyUriBasedAuth :: URI -> Request -> Request
applyAnyUriBasedAuth uri req =
    case extractBasicAuthInfo uri of
        Just auth -> uncurry applyBasicAuth auth req
        Nothing -> req

-- | Extract basic access authentication info in URI.
-- Return Nothing when there is no auth info in URI.
extractBasicAuthInfo :: URI -> Maybe (S8.ByteString, S8.ByteString)
extractBasicAuthInfo uri = do
    userInfo <- uriUserInfo <$> uriAuthority uri
    guard (':' `elem` userInfo)
    let (username, ':':password) = break (==':') . takeWhile (/='@') $ userInfo
    return (toLiteral username, toLiteral password)
  where
    toLiteral = S8.pack . unEscapeString

-- | Validate a 'URI', then add it to the request.
setUri :: MonadThrow m => Request -> URI -> m Request
setUri req uri = do
    sec <- parseScheme uri
    auth <- maybe (failUri "URL must be absolute") return $ uriAuthority uri
    port' <- parsePort sec auth
    return $ applyAnyUriBasedAuth uri req
        { host = S8.pack $ uriRegName auth
        , port = port'
        , secure = sec
        , path = S8.pack $
                    if null $ uriPath uri
                        then "/"
                        else uriPath uri
        , queryString = S8.pack $ uriQuery uri
        }
  where
    failUri :: MonadThrow m => String -> m a
    failUri = throwM . InvalidUrlException (show uri)

    parseScheme URI{uriScheme = scheme} =
        case map toLower scheme of
            "http:"  -> return False
            "https:" -> return True
            _        -> failUri "Invalid scheme"

    parsePort sec URIAuth{uriPort = portStr} =
        case portStr of
            -- If the user specifies a port, then use it
            ':':rest -> maybe
                (failUri "Invalid port")
                return
                (readDec rest)
            -- Otherwise, use the default port
            _ -> case sec of
                    False {- HTTP -} -> return 80
                    True {- HTTPS -} -> return 443

instance Show Request where
    show x = unlines
        [ "Request {"
        , "  host                 = " ++ show (host x)
        , "  port                 = " ++ show (port x)
        , "  secure               = " ++ show (secure x)
        , "  requestHeaders       = " ++ show (requestHeaders x)
        , "  path                 = " ++ show (path x)
        , "  queryString          = " ++ show (queryString x)
        --, "  requestBody          = " ++ show (requestBody x)
        , "  method               = " ++ show (method x)
        , "  proxy                = " ++ show (proxy x)
        , "  rawBody              = " ++ show (rawBody x)
        , "  redirectCount        = " ++ show (redirectCount x)
        , "  responseTimeout      = " ++ show (responseTimeout x)
        , "  requestVersion       = " ++ show (requestVersion x)
        , "}"
        ]

-- | Magic value to be placed in a 'Request' to indicate that we should use the
-- timeout value in the @Manager@.
--
-- Since 1.9.3
useDefaultTimeout :: Maybe Int
useDefaultTimeout = Just (-3425)

instance Default Request where
    def = Request
        { host = "localhost"
        , port = 80
        , secure = False
        , requestHeaders = []
        , path = "/"
        , queryString = S8.empty
        , requestBody = RequestBodyLBS L.empty
        , method = "GET"
        , proxy = Nothing
        , hostAddress = Nothing
        , rawBody = False
        , decompress = browserDecompress
        , redirectCount = 10
        , checkStatus = \s@(W.Status sci _) hs cookie_jar ->
            if 200 <= sci && sci < 300
                then Nothing
                else Just $ toException $ StatusCodeException s hs cookie_jar
        , responseTimeout = useDefaultTimeout
        , getConnectionWrapper = \mtimeout exc f ->
            case mtimeout of
                Nothing -> fmap ((,) Nothing) f
                Just timeout' -> do
                    before <- getCurrentTime
                    mres <- timeout timeout' f
                    case mres of
                        Nothing -> throwIO exc
                        Just res -> do
                            now <- getCurrentTime
                            let timeSpentMicro = diffUTCTime now before * 1000000
                                remainingTime = round $ fromIntegral timeout' - timeSpentMicro
                            if remainingTime <= 0
                                then throwIO exc
                                else return (Just remainingTime, res)
        , cookieJar = Just def
        , requestVersion = W.http11
        , onRequestBodyException = \se ->
            case E.fromException se of
                Just (_ :: IOException) -> return ()
                Nothing -> throwIO se
        , requestManagerOverride = Nothing
        }

instance IsString Request where
    fromString s =
        case parseUrl s of
            Left e -> throw e
            Right r -> r

-- | Always decompress a compressed stream.
alwaysDecompress :: S.ByteString -> Bool
alwaysDecompress = const True

-- | Decompress a compressed stream unless the content-type is 'application/x-tar'.
browserDecompress :: S.ByteString -> Bool
browserDecompress = (/= "application/x-tar")

-- | Add a Basic Auth header (with the specified user name and password) to the
-- given Request. Ignore error handling:
--
-- >  applyBasicAuth "user" "pass" $ fromJust $ parseUrl url
--
-- Since 0.1.0
applyBasicAuth :: S.ByteString -> S.ByteString -> Request -> Request
applyBasicAuth user passwd req =
    req { requestHeaders = authHeader : requestHeaders req }
  where
    authHeader = (CI.mk "Authorization", basic)
    basic = S8.append "Basic " (B64.encode $ S8.concat [ user, ":", passwd ])

-- | Add a proxy to the Request so that the Request when executed will use
-- the provided proxy.
--
-- Since 0.1.0
addProxy :: S.ByteString -> Int -> Request -> Request
addProxy hst prt req =
    req { proxy = Just $ Proxy hst prt }

-- | Add a Proxy-Authorization header (with the specified username and
-- password) to the given 'Request'. Ignore error handling:
--
-- > applyBasicProxyAuth "user" "pass" <$> parseUrl "http://example.org"
--
-- Since 0.3.4

applyBasicProxyAuth :: S.ByteString -> S.ByteString -> Request -> Request
applyBasicProxyAuth user passwd req =
    req { requestHeaders = authHeader : requestHeaders req }
  where
    authHeader = (CI.mk "Proxy-Authorization", basic)
    basic = S8.append "Basic " (B64.encode $ S8.concat [ user , ":", passwd ])

-- | Add url-encoded parameters to the 'Request'.
--
-- This sets a new 'requestBody', adds a content-type request header and
-- changes the 'method' to POST.
--
-- Since 0.1.0
urlEncodedBody :: [(S.ByteString, S.ByteString)] -> Request -> Request
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

needsGunzip :: Request
            -> [W.Header] -- ^ response headers
            -> Bool
needsGunzip req hs' =
        not (rawBody req)
     && ("content-encoding", "gzip") `elem` hs'
     && decompress req (fromMaybe "" $ lookup "content-type" hs')

requestBuilder :: Request -> Connection -> IO (Maybe (IO ()))
requestBuilder req Connection {..} = do
    (contentLength, sendNow, sendLater) <- toTriple (requestBody req)
    if expectContinue
        then flushHeaders contentLength >> return (Just (checkBadSend sendLater))
        else sendNow >> return Nothing
  where
    expectContinue   = Just "100-continue" == lookup "Expect" (requestHeaders req)
    checkBadSend f   = f `E.catch` onRequestBodyException req
    writeBuilder     = toByteStringIO connectionWrite
    writeHeadersWith contentLength = writeBuilder . (builder contentLength `mappend`)
    flushHeaders contentLength     = writeHeadersWith contentLength flush

    toTriple (RequestBodyLBS lbs) = do
        let body  = fromLazyByteString lbs
            len   = Just $ L.length lbs
            now   = checkBadSend $ writeHeadersWith len body
            later = writeBuilder body
        return (len, now, later)
    toTriple (RequestBodyBS bs) = do
        let body  = fromByteString bs
            len   = Just $ fromIntegral $ S.length bs
            now   = checkBadSend $ writeHeadersWith len body
            later = writeBuilder body
        return (len, now, later)
    toTriple (RequestBodyBuilder len body) = do
        let now   = checkBadSend $ writeHeadersWith (Just len) body
            later = writeBuilder body
        return (Just len, now, later)
    toTriple (RequestBodyStream len stream) = do
        -- See https://github.com/snoyberg/http-client/issues/74 for usage
        -- of flush here.
        let body = writeStream False stream
            -- Don't check for a bad send on the headers themselves.
            -- Ideally, we'd do the same thing for the other request body
            -- types, but it would also introduce a performance hit since
            -- we couldn't merge request headers and bodies together.
            now  = flushHeaders (Just len) >> checkBadSend body
        return (Just len, now, body)
    toTriple (RequestBodyStreamChunked stream) = do
        let body = writeStream True stream
            now  = flushHeaders Nothing >> checkBadSend body
        return (Nothing, now, body)
    toTriple (RequestBodyIO mbody) = mbody >>= toTriple

    writeStream isChunked withStream =
        withStream loop
      where
        loop stream = do
            bs <- stream
            if S.null bs
                then when isChunked $ connectionWrite "0\r\n\r\n"
                else do
                    connectionWrite $
                        if isChunked
                            then S.concat
                                [ S8.pack $ showHex (S.length bs) "\r\n"
                                , bs
                                , "\r\n"
                                ]
                            else bs
                    loop stream


    hh
        | port req == 80 && not (secure req) = host req
        | port req == 443 && secure req = host req
        | otherwise = host req <> S8.pack (':' : show (port req))

    requestProtocol
        | secure req = fromByteString "https://"
        | otherwise  = fromByteString "http://"

    requestHostname
        | isJust (proxy req) && not (secure req)
            = requestProtocol <> fromByteString hh
        | otherwise          = mempty

    contentLengthHeader (Just contentLength') =
            if method req `elem` ["GET", "HEAD"] && contentLength' == 0
                then id
                else (:) ("Content-Length", S8.pack $ show contentLength')
    contentLengthHeader Nothing = (:) ("Transfer-Encoding", "chunked")

    acceptEncodingHeader =
        case lookup "Accept-Encoding" $ requestHeaders req of
            Nothing -> (("Accept-Encoding", "gzip"):)
            Just "" -> filter (\(k, _) -> k /= "Accept-Encoding")
            Just _ -> id

    hostHeader x =
        case lookup "Host" x of
            Nothing -> ("Host", hh) : x
            Just{} -> x

    headerPairs :: Maybe Int64 -> W.RequestHeaders
    headerPairs contentLength
                = hostHeader
                $ acceptEncodingHeader
                $ contentLengthHeader contentLength
                $ requestHeaders req

    builder :: Maybe Int64 -> Builder
    builder contentLength =
            fromByteString (method req)
            <> fromByteString " "
            <> requestHostname
            <> (case S8.uncons $ path req of
                    Just ('/', _) -> fromByteString $ path req
                    _ -> fromChar '/' <> fromByteString (path req))
            <> (case S8.uncons $ queryString req of
                    Nothing -> mempty
                    Just ('?', _) -> fromByteString $ queryString req
                    _ -> fromChar '?' <> fromByteString (queryString req))
            <> (case requestVersion req of
                    W.HttpVersion 1 1 -> fromByteString " HTTP/1.1\r\n"
                    W.HttpVersion 1 0 -> fromByteString " HTTP/1.0\r\n"
                    version ->
                        fromChar ' ' <>
                        fromShow version <>
                        fromByteString "\r\n")
            <> foldr
                (\a b -> headerPairToBuilder a <> b)
                (fromByteString "\r\n")
                (headerPairs contentLength)

    headerPairToBuilder (k, v) =
           fromByteString (CI.original k)
        <> fromByteString ": "
        <> fromByteString v
        <> fromByteString "\r\n"

-- | Set the query string to the given key/value pairs.
--
-- Since 0.3.6
setQueryString :: [(S.ByteString, Maybe S.ByteString)] -> Request -> Request
setQueryString qs req = req { queryString = W.renderQuery True qs }

-- | Send a file as the request body.
--
-- It is expected that the file size does not change between calling
-- `streamFile` and making any requests using this request body.
--
-- Since 0.4.9
streamFile :: FilePath -> IO RequestBody
streamFile = observedStreamFile (\_ -> return ())

-- | Send a file as the request body, while observing streaming progress via
-- a `PopObserver`. Observations are made between reading and sending a chunk.
--
-- It is expected that the file size does not change between calling
-- `observedStreamFile` and making any requests using this request body.
--
-- Since 0.4.9
observedStreamFile :: (StreamFileStatus -> IO ()) -> FilePath -> IO RequestBody
observedStreamFile obs path = do
    size <- fromIntegral <$> withBinaryFile path ReadMode hFileSize

    let filePopper :: Handle -> Popper
        filePopper h = do
            bs <- S.hGetSome h defaultChunkSize
            currentPosition <- fromIntegral <$> hTell h
            obs $ StreamFileStatus
                { fileSize = size
                , readSoFar = currentPosition
                , thisChunkSize = S.length bs
                }
            return bs

        givesFilePopper :: GivesPopper ()
        givesFilePopper k = withBinaryFile path ReadMode $ \h -> do
            k (filePopper h)

    return $ RequestBodyStream size givesFilePopper
