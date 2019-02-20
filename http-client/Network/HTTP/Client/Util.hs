{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.HTTP.Client.Util
    ( readPositiveInt
    ) where

import Text.Read (readMaybe)
import Control.Monad (guard)

-- | Read a positive 'Int', accounting for overflow
readPositiveInt :: String -> Maybe Int
readPositiveInt s = do
  i <- readMaybe s
  guard $ i >= 0
  Just i
