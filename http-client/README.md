http-client
===========

An HTTP client engine, intended as a base layer for more user-friendly packages.

This codebase has been refactored from [http-conduit](http://www.stackage.org/package/http-conduit).

Below is a series of cookbook recipes. A number of recipes exist elsewhere,
including `Network.HTTP.Client` and `Network.HTTP.Conduit`. The goal is to
expand this list over time.

## Proxy environment variable

Use the following approach to get proxy settings from the `http_proxy` and
`https_proxy` environment variables.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Client

main :: IO ()
main = do
    let settings = managerSetProxy
            (proxyEnvironment Nothing)
            defaultManagerSettings
    withManager settings $ \man -> do
    let req = "http://httpbin.org"
            -- Note that the following settings will be completely ignored.
            { proxy = Just $ Proxy "localhost" 1234
            }
    httpLbs req man >>= print
```
