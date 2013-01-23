{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Network.HTTP.Conduit.Internal
    ( module Network.HTTP.Conduit.Parser
    , getUri
    , setUri
    , setUriRelative
    , httpRedirect
    ) where

import Network.HTTP.Conduit.Parser

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

import Control.Exception.Lifted (throwIO)
import Control.Monad.Trans.Resource

import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import Data.Conduit.List (sinkNull)

import Network.HTTP.Conduit.Request
import Network.HTTP.Conduit.Response

-- | Redirect loop
httpRedirect
     :: (MonadBaseControl IO m, MonadResource m, Monad m1)
     => Int -- ^ 'redirectCount'
     -> (Request m1 -> m (Response (C.ResumableSource m1 S.ByteString), Maybe (Request m1))) -- ^ function which performs a request and returns a response, and possibly another request if there's a redirect.
     -> (forall a. m1 a -> m a) -- ^ 'liftResourceT'
     -> Request m1
     -> m (Response (C.ResumableSource m1 S.ByteString))
httpRedirect count0 http' lift' req0 = go count0 req0 []
  where
    go (-1) _ ress = throwIO . TooManyRedirects =<< lift' (mapM lbsResponse ress)
    go count req' ress = do
        (res, mreq) <- http' req'
        case mreq of
            Just req -> do
                -- Allow the original connection to return to the
                -- connection pool immediately by flushing the body.
                -- If the response body is too large, don't flush, but
                -- instead just close the connection.
                let maxFlush = 1024
                    readMay bs =
                        case S8.readInt bs of
                            Just (i, bs') | S.null bs' -> Just i
                            _ -> Nothing
                    sink =
                        case lookup "content-length" (responseHeaders res) >>= readMay of
                            Just i | i > maxFlush -> return ()
                            _ -> CB.isolate maxFlush C.=$ sinkNull
                lift' $ responseBody res C.$$+- sink

                -- And now perform the actual redirect
                go (count - 1) req (res:ress)
            Nothing -> return res
