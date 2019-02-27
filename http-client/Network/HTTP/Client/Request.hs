{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.HTTP.Client.Request
    ( parseUrl
    , parseUrlThrow
    , parseRequest
    , parseRequest_
    , requestFromURI
    , requestFromURI_
    , defaultRequest
    , setUriRelative
    , getUri
    , setUri
    , setUriEither
    , browserDecompress
    , alwaysDecompress
    , addProxy
    , applyBasicAuth
    , applyBasicProxyAuth
    , urlEncodedBody
    , needsGunzip
    , requestBuilder
    , setRequestIgnoreStatus
    , setRequestCheckStatus
    , setQueryString
#if MIN_VERSION_http_types(0,12,1)
    , setQueryStringPartialEscape
#endif
    , streamFile
    , observedStreamFile
    , extractBasicAuthInfo
    , throwErrorStatusCodes
    ) where

import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Monoid (mempty, mappend, (<>))
import Data.String (IsString(..))
import Data.Char (toLower)
import Control.Applicative as A ((<$>))
import Control.Monad (unless, guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Numeric (showHex)

import Blaze.ByteString.Builder (Builder, fromByteString, fromLazyByteString, toByteStringIO, flush)
import Blaze.ByteString.Builder.Char8 (fromChar, fromShow)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal (defaultChunkSize)

import qualified Network.HTTP.Types as W
import Network.URI (URI (..), URIAuth (..), parseURI, relativeTo, escapeURIString, unEscapeString, isAllowedInURI)

import Control.Exception (throw, throwIO, IOException)
import qualified Control.Exception as E
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteArray.Encoding as BAE

import Network.HTTP.Client.Body
import Network.HTTP.Client.Types
import Network.HTTP.Client.Util

import Control.Monad.Catch (MonadThrow, throwM)

import System.IO (withBinaryFile, hTell, hFileSize, Handle, IOMode (ReadMode))
import Control.Monad (liftM)

-- | Deprecated synonym for 'parseUrlThrow'. You probably want
-- 'parseRequest' or 'parseRequest_' instead.
--
-- @since 0.1.0
parseUrl :: MonadThrow m => String -> m Request
parseUrl = parseUrlThrow
{-# DEPRECATED parseUrl "Please use parseUrlThrow, parseRequest, or parseRequest_ instead" #-}

-- | Same as 'parseRequest', except will throw an 'HttpException' in the
-- event of a non-2XX response. This uses 'throwErrorStatusCodes' to
-- implement 'checkResponse'.
--
-- @since 0.4.30
parseUrlThrow :: MonadThrow m => String -> m Request
parseUrlThrow =
    liftM yesThrow . parseRequest
  where
    yesThrow req = req { checkResponse = throwErrorStatusCodes }

-- | Throws a 'StatusCodeException' wrapped in 'HttpExceptionRequest',
-- if the response's status code indicates an error (if it isn't 2xx).
-- This can be used to implement 'checkResponse'.
--
-- @since 0.5.13
throwErrorStatusCodes :: MonadIO m => Request -> Response BodyReader -> m ()
throwErrorStatusCodes req res = do
    let W.Status sci _ = responseStatus res
    if 200 <= sci && sci < 300
        then return ()
        else liftIO $ do
            chunk <- brReadSome (responseBody res) 1024
            let res' = fmap (const ()) res
            let ex = StatusCodeException res' (L.toStrict chunk)
            throwIO $ HttpExceptionRequest req ex

-- | Convert a URL into a 'Request'.
--
-- This function defaults some of the values in 'Request', such as setting 'method' to
-- @"GET"@ and 'requestHeaders' to @[]@.
--
-- Since this function uses 'MonadThrow', the return monad can be anything that is
-- an instance of 'MonadThrow', such as 'IO' or 'Maybe'.
--
-- You can place the request method at the beginning of the URL separated by a
-- space, e.g.:
--
-- @@@
-- parseRequest "POST http://httpbin.org/post"
-- @@@
--
-- Note that the request method must be provided as all capital letters.
--
-- A 'Request' created by this function won't cause exceptions on non-2XX
-- response status codes.
--
-- To create a request which throws on non-2XX status codes, see 'parseUrlThrow'
--
-- @since 0.4.30
parseRequest :: MonadThrow m => String -> m Request
parseRequest s' =
    case parseURI (encode s) of
        Just uri -> liftM setMethod (setUri defaultRequest uri)
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

-- | Same as 'parseRequest', but parse errors cause an impure exception.
-- Mostly useful for static strings which are known to be correctly
-- formatted.
parseRequest_ :: String -> Request
parseRequest_ = either throw id . parseRequest

-- | Convert a 'URI' into a 'Request'.
--
-- This can fail if the given 'URI' is not absolute, or if the
-- 'URI' scheme is not @"http"@ or @"https"@. In these cases the function
-- will throw an error via 'MonadThrow'.
--
-- This function defaults some of the values in 'Request', such as setting 'method' to
-- @"GET"@ and 'requestHeaders' to @[]@.
--
-- A 'Request' created by this function won't cause exceptions on non-2XX
-- response status codes.
--
-- @since 0.5.12
requestFromURI :: MonadThrow m => URI -> m Request
requestFromURI = setUri defaultRequest

-- | Same as 'requestFromURI', but if the conversion would fail,
-- throws an impure exception.
--
-- @since 0.5.12
requestFromURI_ :: URI -> Request
requestFromURI_ = either throw id . requestFromURI

-- | Add a 'URI' to the request. If it is absolute (includes a host name), add
-- it as per 'setUri'; if it is relative, merge it with the existing request.
setUriRelative :: MonadThrow m => Request -> URI -> m Request
setUriRelative req uri = setUri req $ uri `relativeTo` getUri req

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
        , uriPort = port'
        }
    , uriPath = S8.unpack $ path req
    , uriQuery =
        case S8.uncons $ queryString req of
            Just (c, _) | c /= '?' -> '?' : (S8.unpack $ queryString req)
            _ -> S8.unpack $ queryString req
    , uriFragment = ""
    }
  where
    port'
      | secure req && (port req) == 443 = ""
      | not (secure req) && (port req) == 80 = ""
      | otherwise = ':' : show (port req)

applyAnyUriBasedAuth :: URI -> Request -> Request
applyAnyUriBasedAuth uri req =
    case extractBasicAuthInfo uri of
        Just auth -> uncurry applyBasicAuth auth req
        Nothing -> req

-- | Extract basic access authentication info in URI.
-- Return Nothing when there is no auth info in URI.
extractBasicAuthInfo :: URI -> Maybe (S8.ByteString, S8.ByteString)
extractBasicAuthInfo uri = do
    userInfo <- uriUserInfo A.<$> uriAuthority uri
    guard (':' `elem` userInfo)
    let (username, ':':password) = break (==':') . takeWhile (/='@') $ userInfo
    return (toLiteral username, toLiteral password)
  where
    toLiteral = S8.pack . unEscapeString

-- | Validate a 'URI', then add it to the request.
setUri :: MonadThrow m => Request -> URI -> m Request
setUri req uri = either throwInvalidUrlException return (setUriEither req uri)
  where
    throwInvalidUrlException = throwM . InvalidUrlException (show uri)

-- | A variant of `setUri` that returns an error message on validation errors,
-- instead of propagating them with `throwM`.
--
-- @since 0.6.1
setUriEither :: Request -> URI -> Either String Request
setUriEither req uri = do
    sec <- parseScheme uri
    auth <- maybe (Left "URL must be absolute") return $ uriAuthority uri
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
    parseScheme URI{uriScheme = scheme} =
        case map toLower scheme of
            "http:"  -> return False
            "https:" -> return True
            _        -> Left "Invalid scheme"

    parsePort sec URIAuth{uriPort = portStr} =
        case portStr of
            -- If the user specifies a port, then use it
            ':':rest -> maybe
                (Left "Invalid port")
                return
                (readPositiveInt rest)
            -- Otherwise, use the default port
            _ -> case sec of
                    False {- HTTP -} -> return 80
                    True {- HTTPS -} -> return 443

-- | A default request value, a GET request of localhost/:80, with an
-- empty request body.
--
-- Note that the default 'checkResponse' does nothing.
--
-- @since 0.4.30
defaultRequest :: Request
defaultRequest = Request
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
        , checkResponse = \_ _ -> return ()
        , responseTimeout = ResponseTimeoutDefault
        , cookieJar = Just Data.Monoid.mempty
        , requestVersion = W.http11
        , onRequestBodyException = \se ->
            case E.fromException se of
                Just (_ :: IOException) -> return ()
                Nothing -> throwIO se
        , requestManagerOverride = Nothing
        , shouldStripHeaderOnRedirect = const False
        }

-- | Parses a URL via 'parseRequest_'
--
-- /NOTE/: Prior to version 0.5.0, this instance used 'parseUrlThrow'
-- instead.
instance IsString Request where
    fromString = parseRequest_
    {-# INLINE fromString #-}

-- | Always decompress a compressed stream.
alwaysDecompress :: S.ByteString -> Bool
alwaysDecompress = const True

-- | Decompress a compressed stream unless the content-type is 'application/x-tar'.
browserDecompress :: S.ByteString -> Bool
browserDecompress = (/= "application/x-tar")

-- | Build a basic-auth header value
buildBasicAuth ::
    S8.ByteString -- ^ Username
    -> S8.ByteString -- ^ Password
    -> S8.ByteString
buildBasicAuth user passwd =
    S8.append "Basic " (BAE.convertToBase BAE.Base64 (S8.concat [ user, ":", passwd ]))

-- | Add a Basic Auth header (with the specified user name and password) to the
-- given Request. Ignore error handling:
--
-- >  applyBasicAuth "user" "pass" $ parseRequest_ url
--
-- NOTE: The function @applyDigestAuth@ is provided by the @http-client-tls@
-- package instead of this package due to extra dependencies. Please use that
-- package if you need to use digest authentication.
--
-- Since 0.1.0
applyBasicAuth :: S.ByteString -> S.ByteString -> Request -> Request
applyBasicAuth user passwd req =
    req { requestHeaders = authHeader : requestHeaders req }
  where
    authHeader = (CI.mk "Authorization", buildBasicAuth user passwd)

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
-- > applyBasicProxyAuth "user" "pass" <$> parseRequest "http://example.org"
--
-- Since 0.3.4

applyBasicProxyAuth :: S.ByteString -> S.ByteString -> Request -> Request
applyBasicProxyAuth user passwd req =
    req { requestHeaders = authHeader : requestHeaders req }
  where
    authHeader = (CI.mk "Proxy-Authorization", buildBasicAuth user passwd)

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
    writeHeadersWith contentLength = writeBuilder . (builder contentLength `Data.Monoid.mappend`)
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
        let body = writeStream (Just . fromIntegral $ len) stream
            -- Don't check for a bad send on the headers themselves.
            -- Ideally, we'd do the same thing for the other request body
            -- types, but it would also introduce a performance hit since
            -- we couldn't merge request headers and bodies together.
            now  = flushHeaders (Just len) >> checkBadSend body
        return (Just len, now, body)
    toTriple (RequestBodyStreamChunked stream) = do
        let body = writeStream Nothing stream
            now  = flushHeaders Nothing >> checkBadSend body
        return (Nothing, now, body)
    toTriple (RequestBodyIO mbody) = mbody >>= toTriple

    writeStream mlen withStream =
        withStream (loop 0)
      where
        loop !n stream = do
            bs <- stream
            if S.null bs
                then case mlen of
                    -- If stream is chunked, no length argument
                    Nothing -> connectionWrite "0\r\n\r\n"
                    -- Not chunked - validate length argument
                    Just len -> unless (len == n) $ throwHttp $ WrongRequestBodyStreamSize (fromIntegral len) (fromIntegral n)
                else do
                    connectionWrite $
                        if (isNothing mlen) -- Chunked
                            then S.concat
                                [ S8.pack $ showHex (S.length bs) "\r\n"
                                , bs
                                , "\r\n"
                                ]
                            else bs
                    loop (n + (S.length bs)) stream

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

-- | Modify the request so that non-2XX status codes do not generate a runtime
-- 'StatusCodeException'.
--
-- @since 0.4.29
setRequestIgnoreStatus :: Request -> Request
setRequestIgnoreStatus req = req { checkResponse = \_ _ -> return () }

-- | Modify the request so that non-2XX status codes generate a runtime
-- 'StatusCodeException', by using 'throwErrorStatusCodes'
--
-- @since 0.5.13
setRequestCheckStatus :: Request -> Request
setRequestCheckStatus req = req { checkResponse = throwErrorStatusCodes }

-- | Set the query string to the given key/value pairs.
--
-- Since 0.3.6
setQueryString :: [(S.ByteString, Maybe S.ByteString)] -> Request -> Request
setQueryString qs req = req { queryString = W.renderQuery True qs }

#if MIN_VERSION_http_types(0,12,1)
-- | Set the query string to the given key/value pairs.
--
-- @since 0.5.10
setQueryStringPartialEscape :: [(S.ByteString, [W.EscapeItem])] -> Request -> Request
setQueryStringPartialEscape qs req = req { queryString = W.renderQueryPartialEscape True qs }
#endif

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
