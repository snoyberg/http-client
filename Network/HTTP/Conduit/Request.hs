{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.HTTP.Conduit.Request
    ( Request (..)
    , RequestBody (..)
    , ContentType
    , Proxy (..)
    , parseUrl
    , setUriRelative
    , getUri
    , setUri
    , browserDecompress
    , HttpException (..)
    , alwaysDecompress
    , addProxy
    , applyBasicAuth
    , urlEncodedBody
    , needsGunzip
    , requestBuilder
    , useDefaultTimeout
    ) where

import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (mempty, mappend)

import Data.Default (Default (def))

import Blaze.ByteString.Builder (Builder, fromByteString, fromLazyByteString)
import Blaze.ByteString.Builder.Char8 (fromChar)

import qualified Data.Conduit as C

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L

import qualified Network.HTTP.Types as W
import Network.URI (URI (..), URIAuth (..), parseURI, relativeTo, escapeURIString, isAllowedInURI)

import Control.Monad.IO.Class (liftIO)
import Control.Exception.Lifted (Exception, toException, throwIO)
import Control.Failure (Failure (failure))
import Codec.Binary.UTF8.String (encodeString)
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Base64 as B64

import Network.HTTP.Conduit.Types (Request (..), RequestBody (..), ContentType, Proxy (..), HttpException (..))

import Network.HTTP.Conduit.Chunk (chunkIt)
import Network.HTTP.Conduit.Util (readDec, (<>))
import System.Timeout.Lifted (timeout)
import Data.Time.Clock

-- | Convert a URL into a 'Request'.
--
-- This defaults some of the values in 'Request', such as setting 'method' to
-- GET and 'requestHeaders' to @[]@.
--
-- Since this function uses 'Failure', the return monad can be anything that is
-- an instance of 'Failure', such as 'IO' or 'Maybe'.
parseUrl :: Failure HttpException m => String -> m (Request m')
parseUrl s =
    case parseURI (encode s) of
        Just uri -> setUri def uri
        Nothing  -> failure $ InvalidUrlException s "Invalid URL"
  where
    encode = escapeURIString isAllowedInURI . encodeString

-- | Add a 'URI' to the request. If it is absolute (includes a host name), add
-- it as per 'setUri'; if it is relative, merge it with the existing request.
setUriRelative :: Failure HttpException m => Request m' -> URI -> m (Request m')
setUriRelative req uri =
#if MIN_VERSION_network(2,4,0)
    setUri req $ uri `relativeTo` getUri req
#else
    case uri `relativeTo` getUri req of
        Just uri' -> setUri req uri'
        Nothing   -> failure $ InvalidUrlException (show uri) "Invalid URL"
#endif

-- | Extract a 'URI' from the request.
getUri :: Request m' -> URI
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
    , uriQuery = S8.unpack $ queryString req
    , uriFragment = ""
    }

-- | Validate a 'URI', then add it to the request.
setUri :: Failure HttpException m => Request m' -> URI -> m (Request m')
setUri req uri = do
    sec <- parseScheme uri
    auth <- maybe (failUri "URL must be absolute") return $ uriAuthority uri
    if not . null $ uriUserInfo auth
        then failUri "URL auth not supported; use applyBasicAuth instead"
        else return ()
    port' <- parsePort sec auth
    return req
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
    failUri :: Failure HttpException m => String -> m a
    failUri = failure . InvalidUrlException (show uri)

    parseScheme URI{uriScheme = scheme} =
        case scheme of
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

instance Show (Request m) where
    show x = unlines
        [ "Request {"
        , "  host                 = " ++ show (host x)
        , "  port                 = " ++ show (port x)
        , "  secure               = " ++ show (secure x)
        , "  clientCertificates   = " ++ show (clientCertificates x)
        , "  requestHeaders       = " ++ show (requestHeaders x)
        , "  path                 = " ++ show (path x)
        , "  queryString          = " ++ show (queryString x)
        , "  requestBody          = " ++ show (requestBody x)
        , "  method               = " ++ show (method x)
        , "  proxy                = " ++ show (proxy x)
        , "  rawBody              = " ++ show (rawBody x)
        , "  redirectCount        = " ++ show (redirectCount x)
        , "  responseTimeout      = " ++ show (responseTimeout x)
        , "}"
        ]

-- | Magic value to be placed in a 'Request' to indicate that we should use the
-- timeout value in the @Manager@.
--
-- Since 1.9.3
useDefaultTimeout :: Maybe Int
useDefaultTimeout = Just (-3425)

instance Default (Request m) where
    def = Request
        { host = "localhost"
        , port = 80
        , secure = False
        , clientCertificates = []
        , requestHeaders = []
        , path = "/"
        , queryString = S8.empty
        , requestBody = RequestBodyLBS L.empty
        , method = "GET"
        , proxy = Nothing
        , socksProxy = Nothing
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
                    before <- liftIO getCurrentTime
                    mres <- timeout timeout' f
                    case mres of
                        Nothing -> throwIO exc
                        Just res -> do
                            now <- liftIO getCurrentTime
                            let timeSpentMicro = diffUTCTime now before * 1000000
                                remainingTime = round $ fromIntegral timeout' - timeSpentMicro
                            if remainingTime <= 0
                                then throwIO exc
                                else return (Just remainingTime, res)
        , cookieJar = Just def
        }

-- | Always decompress a compressed stream.
alwaysDecompress :: ContentType -> Bool
alwaysDecompress = const True

-- | Decompress a compressed stream unless the content-type is 'application/x-tar'.
browserDecompress :: ContentType -> Bool
browserDecompress = (/= "application/x-tar")

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


-- | Add a proxy to the Request so that the Request when executed will use
-- the provided proxy.
addProxy :: S.ByteString -> Int -> Request m -> Request m
addProxy hst prt req =
    req { proxy = Just $ Proxy hst prt }

-- FIXME add a helper for generating POST bodies

-- | Add url-encoded parameters to the 'Request'.
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

needsGunzip :: Request m
            -> [W.Header] -- ^ response headers
            -> Bool
needsGunzip req hs' =
        not (rawBody req)
     && ("content-encoding", "gzip") `elem` hs'
     && decompress req (fromMaybe "" $ lookup "content-type" hs')

requestBuilder
    :: Monad m
    => Request m
    -> C.Source m Builder
requestBuilder req =
    bodySource
  where
    (contentLength, bodySource) =
        case requestBody req of
            RequestBodyLBS lbs -> (Just $ L.length lbs, C.yield $ builder `mappend` fromLazyByteString lbs)
            RequestBodyBS bs -> (Just $ fromIntegral $ S.length bs, C.yield $ builder `mappend` fromByteString bs)
            RequestBodyBuilder i b -> (Just $ i, C.yield $ builder `mappend` b)
            RequestBodySource i source -> (Just i, C.yield builder >> source)
            RequestBodySourceChunked source -> (Nothing, C.yield builder >> (source C.$= chunkIt))

    hh
        | port req == 80 && not (secure req) = host req
        | port req == 443 && secure req = host req
        | otherwise = host req <> S8.pack (':' : show (port req))

    requestProtocol
        | secure req = fromByteString "https://"
        | otherwise  = fromByteString "http://"

    requestHostname
        | isJust (proxy req) = requestProtocol <> fromByteString hh
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

    headerPairs :: W.RequestHeaders
    headerPairs = hostHeader
                $ acceptEncodingHeader
                $ contentLengthHeader contentLength
                $ requestHeaders req

    builder :: Builder
    builder =
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
            <> fromByteString " HTTP/1.1\r\n"
            <> foldr
                (\a b -> headerPairToBuilder a <> b)
                (fromByteString "\r\n")
                headerPairs

    headerPairToBuilder (k, v) =
           fromByteString (CI.original k)
        <> fromByteString ": "
        <> fromByteString v
        <> fromByteString "\r\n"
