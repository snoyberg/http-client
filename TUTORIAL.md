# http-client tutorial

http-client is a minimalistic package with a relatively low-level API and no
support for TLS (HTTPS). This tutorial follows the `Network.HTTP.Simple` module
from the http-conduit package, which provides a higher-level interface.

## API docs

The API documentation can be found at:

* [http-client](https://www.stackage.org/lts/package/http-client)
* [http-conduit](https://www.stackage.org/lts/package/http-conduit)

## Tutorial exercise

To help motivate learning, keep in mind the following exercise while reading through the tutorial, 
and try to implement a solution. Write a program that takes an
input file with one URL per line, and ensures that making a request to each URL
returns a non-error status code.

## Basic usage

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://httpbin.org/get"

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response
```

`httpLBS` makes a request to the given URL and captures the response body as a
lazy `ByteString`. Note that, even though this is a lazy `ByteString`, it is
read fully into memory when making the request. It only returns a lazy
`ByteString` for better memory usage. (See streaming below for more information.)

Once we have our response value, we can use getter functions to look at various
details (status code, headers, and the body).

## Receiving JSON

We can also use aeson to receive a JSON message.

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpJSON "http://httpbin.org/get"

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
```

The main change is that we used `httpJSON` in place of `httpLBS`. This function
will return any instance of `FromJSON`, and perform all necessary parsing and
conversion. If there are any problems, it will throw a runtime exception (use
`httpJSONEither` to avoid the runtime exception and get an `Either`).

Since the return value can be any `FromJSON` instance, we need to somehow
constrain the value returned. In this case, we used an explicit `:: Value`
signature, but usually you'll do the constraining by using a custom data type.

For fun, this example prints out the JSON body in YAML format.

## Advanced use

There are add-on packages which
provide additional functionality, e.g.:

* [http-client-tls](https://www.stackage.org/package/http-client-tls) provides TLS support via the Haskell-native tls package
* [http-conduit](https://www.stackage.org/package/http-conduit) allows for streaming request and responses using conduit

## Concepts

This library makes good use of the `OverloadedStrings` language extension for
converting string literals into `Request`s, `ByteString`s, and case-insensitive
`ByteString`s (for header names). It's strongly recommended to use this library
with this language extension enabled.

## Caveats

There are a few important caveats to mention about this library:

* By default, any non-2XX status code response won't result in a runtime
  exception contrary to previous behaviour of the library (before version
  0.5).
* By default, http-client will respect the `http_proxy` and `https_proxy`
  environment variables. See the proxy examples below for information on how to
  bypass this.

## Request methods and parseRequest

You can specify the request method at the beginning of your URL:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpJSON "POST http://httpbin.org/post"

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
```

What's actually happening is that the `IsString` instance for `Request` is
being used to parse the string literal to a `Request`. But you can also be
more explicit about it with `parseRequest`:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

main :: IO ()
main = do
    request <- parseRequest "POST http://httpbin.org/post"
    response <- httpJSON request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
```

`parseRequest` is more explicit about exceptions, whereas the string literal
approach can result in unexpected runtime exceptions if you have a typo in your
code. Generally, `parseRequest` should be your choice when parsing URLs
generated at runtime.

CAUTION: If you provide an invalid URL as a string literal, it will manifest as
a runtime exception when forcing the pure `Request` value, e.g.:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "BAD URL"
    print response
```

generates:

```
foo.hs: InvalidUrlException "BAD URL" "Invalid URL"
```

## Request building

There's a lot more to a request than just the request method. These can be
modified with various request setter functions:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

main :: IO ()
main = do
    request' <- parseRequest "POST http://httpbin.org/post"
    let request
            = setRequestMethod "PUT"
            $ setRequestPath "/put"
            $ setRequestQueryString [("hello", Just "world")]
            $ setRequestBodyLBS "This is my request body"
            $ setRequestSecure True
            $ setRequestPort 443
            $ request'
    response <- httpJSON request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
```

Exercise for reader: rewrite the code above to not use `parseRequest`.

And in fact, if you want, you can build up a request entirely programmatically,
without any URL parsing:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

main :: IO ()
main = do
    let request
            = setRequestPath "/get"
            $ setRequestHost "httpbin.org"
            $ defaultRequest
    response <- httpJSON request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
```

## Request bodies

Like the response body, there are multiple helper functions for dealing with
different request body formats. These include JSON:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

data Person = Person String Int
instance ToJSON Person where
    toJSON (Person name age) = object
        [ "name" .= name
        , "age"  .= age
        ]

people :: [Person]
people = [Person "Alice" 30, Person "Bob" 35, Person "Charlie" 40]

main :: IO ()
main = do
    let request = setRequestBodyJSON people $ "POST https://httpbin.org/post"
    response <- httpJSON request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
```

Or data from a file:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

data Person = Person String Int
instance ToJSON Person where
    toJSON (Person name age) = object
        [ "name" .= name
        , "age"  .= age
        ]

people :: [Person]
people = [Person "Alice" 30, Person "Bob" 35, Person "Charlie" 40]

main :: IO ()
main = do
    Yaml.encodeFile "people.yaml" people

    let request = setRequestBodyFile "people.yaml"
                $ setRequestHeader "Content-Type" ["application/x-yaml"]
                $ "PUT https://httpbin.org/put"
    response <- httpJSON request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
```

## Non-2XX responses

By default, beginning with version 0.5 of the library every request that
generates a non-2XX response won't throw a runtime exception if you use just
string literals to construct your request. However, one should understand
that whether exception is thrown on non-2XX response status codes or not
depends on a setting in corresponding request, called `checkResponse` in
version 0.5 and `checkStatus` in older versions. Thus, the way you construct
request from string literal determines whether the library will throw
exceptions on non-2XX response status codes.

Let's examine all the parsing functions and specify which of them produces
“throwing” requests:

* `parseUrl` is deprecated, it's the same as `parseUrlThrow`.

* `parseUrlThrow` produces requests that have `checkResponse` action that
  will throw if response has non-2XX status code.

* `parseRequest` produces “safe” requests that won't throw on non-2XX
  response status codes (it doesn't mean that they don't throw at all
  though, as there may be other problems with making a request).

* `parseRequest_` is the same as `parseRequest`, it just will blow up at
  runtime if given string is malformed. This is what is used to parse
  requests from string litreals in `IsString` instance of `Request`.

## Exceptions

There are other potential exceptions that may be thrown by this library, such
as due to failed connections. To catch these, you should catch the
`HttpException` exception type.

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Control.Exception          (try)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple

main :: IO ()
main = do
    eresponse <- try $ httpLBS "http://does-not-exist"

    case eresponse of
        Left e -> print (e :: HttpException)
        Right response -> L8.putStrLn $ getResponseBody response
```

## Streaming

Sometimes you will want to avoid reading the entire response body into memory
at once. For these cases, a streaming data approach is useful.

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as S
import qualified Data.Conduit.List      as CL
import           Network.HTTP.Simple
import           System.IO              (stdout)

main :: IO ()
main = httpSink "http://httpbin.org/get" $ \response -> do
    liftIO $ putStrLn
           $ "The status code was: "
          ++ show (getResponseStatusCode response)

    CL.mapM_ (S.hPut stdout)
```

## Override proxy

By default, requests will use any proxy server specified with the `http_proxy`
or `https_proxy` environment variables. This can be overridden:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple

main :: IO ()
main = do
    let request = setRequestProxy (Just (Proxy "127.0.0.1" 3128))
                $ "https://httpbin.org/get"
    response <- httpLBS request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response
```

## Connection Manager

All HTTP requests are made via a `Manager`. A `Manager` handles the details of
creating connections to servers. It handles things like reusing connections (to
avoid high TCP overhead when making multiple requests to the same host). It
also allows you to configure various settings, most important how to make
secure connections (HTTPS).

For ease of use and to ensure maximum connection sharing in an application, the
`Network.HTTP.Simple` module uses a shared global connection `Manager` by
default. If desired, you can create your own `Manager` and override that
global:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Network.HTTP.Simple

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings

    let request = setRequestManager manager "http://httpbin.org/get"
    response <- httpLBS request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response
```

Exercises:

1. Modify the above to make an HTTPS connection instead. What happens?
2. Fix the error generated by the previous step by using
   `Network.HTTP.Client.TLS.tlsManagerSettings`

You can also override the global manager if, for example, you want to tweak
some settings:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple

main :: IO ()
main = do
    manager <- newManager $ managerSetProxy noProxy tlsManagerSettings
    setGlobalManager manager

    let request = "http://httpbin.org/get"
    response <- httpLBS request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response
```

For our purposes, you should use
`tlsManagerSettings` to ensure you have full HTTP and HTTPS support (as all
examples below do).

## Lower level API

The above docs all cover the `Network.HTTP.Simple` API. However, there is a
lower-level API available in `Network.HTTP.Client`, which may be advantageous
in some cases. The rest of this tutorial provides some examples of this
lower-level API.

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Network.HTTP.Client
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    request <- parseRequest "http://httpbin.org/get"
    response <- httpLbs request manager

    putStrLn $ "The status code was: " ++
               show (statusCode $ responseStatus response)
    print $ responseBody response
```

We're using `newManager tlsManagerSettings` to get a new `Manager`,
`parseRequest` to parse a textual URL into a `Request`, and then making the
request with `httpLbs`. Once we have our `Response`, we can use standard
accessors to inspect its fields.

## Receiving JSON

It's also straightforward to compose this streaming with aeson to parse JSON:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import           Data.Aeson.Parser           (json)
import           Data.Conduit                (($$))
import           Data.Conduit.Attoparsec     (sinkParser)
import           Network.HTTP.Client
import           Network.HTTP.Client.Conduit (bodyReaderSource)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Network.HTTP.Types.Status   (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    request <- parseRequest "http://httpbin.org/get"

    withResponse request manager $ \response -> do
        putStrLn $ "The status code was: " ++
                   show (statusCode $ responseStatus response)

        value <- bodyReaderSource (responseBody response)
              $$ sinkParser json
        print value
```

## Sending JSON

Sending JSON can be done with modifying the request method and body:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson                 (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    -- Create the request
    let requestObject = object
            [ "name" .= ("Alice" :: String)
            , "age"  .= (35 :: Int)
            ]
    initialRequest <- parseRequest "http://httpbin.org/post"
    let request = initialRequest
            { method = "POST"
            , requestBody = RequestBodyLBS $ encode requestObject
            , requestHeaders =
                [ ("Content-Type", "application/json; charset=utf-8")
                ]
            }

    response <- httpLbs request manager
    putStrLn $ "The status code was: "
            ++ show (statusCode $ responseStatus response)
    L8.putStrLn $ responseBody response
```

Another common request body format is URL encoded bodies. The `urlEncodedBody`
function is a convenient function for doing this. Note that it automatically
sets the request method to `POST`, which we can override if desired:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    initialRequest <- parseRequest "http://httpbin.org/put"
    let pairs =
            [ ("name", "Alice")
            , ("age", "35")
            ]
        request = (urlEncodedBody pairs initialRequest)
            { method = "PUT"
            }

    response <- httpLbs request manager
    putStrLn $ "The status code was: "
            ++ show (statusCode $ responseStatus response)
    L8.putStrLn $ responseBody response
```

## Non-2XX responses

The `checkResponse` record selector (starting in version 0.5 of http-client)
allows you to examine the request and response and
throw an exception if something is wrong. In versions older than 0.5 non-2XX
response status codes were throwing exceptions, but now this has been
changed and `checkResponse` does nothing by default.

## Proxy settings

By default, http-client will respect the `http_proxy` and `https_proxy`
environment variables. You can modify this when creating your `Manager`:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)

main :: IO ()
main = do
    manager <- newManager $ managerSetProxy noProxy tlsManagerSettings

    response <- httpLbs "http://httpbin.org/get" manager

    putStrLn $ "The status code was: "
            ++ show (statusCode $ responseStatus response)
    L8.putStrLn $ responseBody response
```

You can also modify the proxy settings per-request:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    let request = "http://httpbin.org/get"
            { proxy = Just $ Proxy "127.0.0.1" 3128
            }
    response <- httpLbs request manager

    putStrLn $ "The status code was: "
            ++ show (statusCode $ responseStatus response)
    L8.putStrLn $ responseBody response
```

If you set both the manager and request proxy overrides, the manager setting
will win out.

## Sharing the Manager

There is a small cost to initializing a `Manager`. More importantly, each
`Manager` maintains its own pool of connections. It is highly advisable to
share your `Manager` value throughout your application. This will decrease TCP
handshake overhead, and make it less likely that you will make too many
connections to a single server at once.

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Control.Concurrent.Async  (Concurrently (..))
import qualified Data.ByteString.Char8     as S8
import qualified Data.ByteString.Lazy      as L
import           Data.Foldable             (sequenceA_)
import qualified Data.Text                 as T
import           Data.Text.Encoding        (encodeUtf8)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    runConcurrently $ sequenceA_ $ replicate 16
                    $ Concurrently $ doSomething manager

doSomething :: Manager -> IO ()
doSomething manager = do
    let request = "http://httpbin.org/get"

    response <- httpLbs request manager

    let msg = encodeUtf8 $ T.pack $ concat
            [ "Got a message with status code "
            , show $ statusCode $ responseStatus response
            , " with response body length "
            , show $ L.length $ responseBody response
            , "\n"
            ]

    -- Using bytestring-based output to avoid interleaving of string-based
    -- output
    S8.putStr msg
```

## Streaming

Beneath the conduit-based streaming API you've already seen, there is a
lower-level streaming API:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import qualified Data.ByteString           as S
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types.Status (statusCode)
import           System.IO                 (stdout)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    request <- parseRequest "http://httpbin.org/get"

    withResponse request manager $ \response -> do
        putStrLn $ "The status code was: " ++
                   show (statusCode $ responseStatus response)

        let loop = do
                bs <- brRead $ responseBody response
                if S.null bs
                    then putStrLn "\nFinished response body"
                    else do
                        S.hPut stdout bs
                        loop
        loop
```

## Exercise

Check all of the URLs in a file to ensure they return non-error status
codes. Each line is its own URL.

Bonuses:

* Support both `http://...` and `https://...` URLs.
* Use conduit to stream the file contents.
* Do the checking concurrently.
* Real bonus: check URLs within downloaded HTML pages, using
  html-conduit to parse.
