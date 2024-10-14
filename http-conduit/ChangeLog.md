# ChangeLog for http-conduit

## 2.3.9

* Fix space leaks when closing responses [#539](https://github.com/snoyberg/http-client/pull/539)

## 2.3.8.3

* aeson 2.2 support [#512](https://github.com/snoyberg/http-client/pull/512)

## 2.3.8.2

* Add missing `crypton-connection` dependency

## 2.3.8.1

* Drop `connection` dependency

## 2.3.8

* Adds `setRequestBearerAuth` convenience function. Note that this is only available for `http-client` versions 0.7.6 or greater. [#457](https://github.com/snoyberg/http-client/pull/457/files)
* Adds a convenience function to set a request's response timeout [#456](https://github.com/snoyberg/http-client/pull/456)

## 2.3.7.4

* Introduces the `aeson` cabal file [#448](https://github.com/snoyberg/http-client/issues/448)

## 2.3.7.3

* Relax test suite version bounds

## 2.3.7.2

* Add the `network3` flag

## 2.3.7.1

* Properly skip whitespace after JSON body [#401](https://github.com/snoyberg/http-client/issues/401)

## 2.3.7

* Ensure entire JSON response body is consumed [#395](https://github.com/snoyberg/http-client/issues/395)

## 2.3.6.1

* Add back compatibility with older http-client version [#393](https://github.com/snoyberg/http-client/pull/393)

## 2.3.6

* Add `httpSource` to `Network.HTTP.Client.Conduit` [#390](https://github.com/snoyberg/http-client/pull/390).

## 2.3.5

* Adds `addToRequestQueryString` helper function

## 2.3.4

* Reexport RequestHeaders from Network.HTTP.Types (what was intended in last version)
* Fix mistake in ChangeLog

## 2.3.3

* Reexport Header, QueryItem and ResponseHeaders from Network.HTTP.Types
* Rewrite a type signature of setRequestHeaders with RequestHeaders

## 2.3.2

* Adds `parseRequestThrow`, `parseRequestThrow_`, and
  `setRequestCheckStatus` to `Network.HTTP.Simple`.
  See [#304](https://github.com/snoyberg/http-client/issues/304)

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
