## http-client-tls

Full tutorial docs are available at:
https://haskell-lang.org/library/http-client

Use the http-client package with the pure-Haskell tls package for secure
connections. For the most part, you'll just want to replace
`defaultManagerSettings` with `tlsManagerSettings`, e.g.:

```haskell
import Network.HTTP.Client
import Network.HTTP.Client.TLS

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    ...
```
