## 2.3.1

* Reexport Query from Network.HTTP.Types
* Rewrite a type signatures of getRequestQueryString and setRequestQueryString with Query

## 2.3.0

* conduit 1.3 support
    * NOTE: Even for older versions of conduit, this includes dropping
      support for finalizers
* `http` returns a `Source` instead of a `ResumableSource` (due to lack of
  finalizers)
* Drop monad-control for unliftio
* Removed some deprecated functions: `withManager`, `withManagerSettings`,
  `conduitManagerSettings`

## 2.2.4

* Add `httpBS` to `Network.HTTP.Simple`

## 2.2.3.2

* Add proper headers for `httpJSON` and `httpJSONEither` [#284](https://github.com/snoyberg/http-client/issues/284)

## 2.2.3.1

* Minor README improvement

## 2.2.3

* Add `withResponse` to `Network.HTTP.Simple`

## 2.2.2.1

* setRequestBodyJSON works with aeson's toEncoding function (>= 0.11)
  [#230](https://github.com/snoyberg/http-client/pull/230)

## 2.2.2

* Add `httpNoBody` to `Network.HTTP.Simple`

## 2.2.1

* Add `httpSource` to `Network.HTTP.Simple`

## 2.2.0.1

* Doc fixes

## 2.2.0

* Upgrade to http-client 0.5

## 2.1.11

* Switch to non-throwing behavior in `Network.HTTP.Simple` [#193](https://github.com/snoyberg/http-client/issues/193)

## 2.1.10.1

* Fix mistaken `@since` comments

## 2.1.10

* Add the `Network.HTTP.Simple` module

## 2.1.9

* cabal file cleanup

## 2.1.8

* Move HasHttpManager from http-conduit to http-client [#147](https://github.com/snoyberg/http-client/pull/147)

## 2.1.7

* Deprecate `conduitManagerSettings`, re-export `tlsManagerSettings` [#136](https://github.com/snoyberg/http-client/issues/136) [#137](https://github.com/snoyberg/http-client/issues/137)

## 2.1.6

* Deprecate `withManager` and `withManagerSettings`
