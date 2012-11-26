{-# LANGUAGE FlexibleContexts #-}
module Network.HTTP.Conduit.Internal
    ( module Network.HTTP.Conduit.Parser
    , getUri
    , setUri
    , setUriRelative
    , httpRedirect
    ) where

import Network.HTTP.Conduit.Parser

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Control.Exception.Lifted (throwIO)
import Control.Monad.Trans.Resource

import qualified Data.Conduit as C

import Network.HTTP.Conduit.Request
import Network.HTTP.Conduit.Response

-- | Redirect loop
httpRedirect
     :: (MonadBaseControl IO m, MonadResource m)
     => Int -- ^ Redirect count
     -> (Request m1 -> m (Response (C.ResumableSource m1 S.ByteString), Maybe (Request m1)))
     -> (Response (C.ResumableSource m1 S.ByteString) -> m (Response L.ByteString))
     -> Request m1
     -> m (Response (C.ResumableSource m1 S.ByteString))
httpRedirect count0 httpFunc lbsResponse' req0 = go count0 req0 []
  where
    go (-1) _ ress = throwIO $ TooManyRedirects ress
    go count req' ress = do
        (res, mreq) <- httpFunc req'
        case mreq of
            Just req -> do
                lbsres <- lbsResponse' res
                go (count - 1) req (lbsres:ress)
            Nothing -> return res
