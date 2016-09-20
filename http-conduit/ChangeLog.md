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
