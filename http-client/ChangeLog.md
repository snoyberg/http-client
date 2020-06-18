# Changelog for http-client

## 0.7.1

* Remove `AI_ADDRCONFIG` [#400](https://github.com/snoyberg/http-client/issues/400)

## 0.7.0

* Remove Eq instances for Cookie, CookieJar, Response, Ord instance for Cookie [#435](https://github.com/snoyberg/http-client/pull/435)

## 0.6.4.1

* Win32 2.8 support [#430](https://github.com/snoyberg/http-client/pull/430)

## 0.6.4

* Avoid throwing an exception when a malformed HTTP header is received,
  to be as robust as commonly used HTTP clients.
  See [#398](https://github.com/snoyberg/http-client/issues/398)

## 0.6.3

* Detect response body termination before reading an extra null chunk
  when possible. This allows connections to be reused in some corner
  cases. See
  [#395](https://github.com/snoyberg/http-client/issues/395)

## 0.6.2

* Add `shouldStripHeaderOnRedirect` option to `Request` [#300](https://github.com/snoyberg/http-client/issues/300)

## 0.6.1.1

* Ensure that `Int` parsing doesn't overflow [#383](https://github.com/snoyberg/http-client/issues/383)

## 0.6.1

* Add `setUriEither` to `Network.HTTP.Client.Internal`

## 0.6.0

* Generalize `renderParts` over arbitrary applicative functors.  One particular
  use case that is enabled by this change is that now `renderParts` can be used
  in pure code by using it in combination with `runIdentity`.

## 0.5.14

* Omit port for `getUri` when protocol is `http` and port is `80`, or when
  protocol is `https` and port is `443`
* Sending requests with invalid headers now throws InvalidRequestHeader exception

## 0.5.13.1

* Add a workaround for a cabal bug [haskell-infra/hackage-trustees#165](https://github.com/haskell-infra/hackage-trustees/issues/165)

## 0.5.13

* Adds `setRequestCheckStatus` and `throwErrorStatusCodes` functions.
  See [#304](https://github.com/snoyberg/http-client/issues/304)
* Add `withConnection` function.
  See [#352](https://github.com/snoyberg/http-client/pull/352).

## 0.5.12.1

* Make the chunked transfer-encoding detection case insensitive
  [#303](https://github.com/snoyberg/http-client/pull/303)
* Remove some unneeded language extensions
* Mark older versions of GHC as unsupported

## 0.5.12

* Added `requestFromURI` and `requestFromURI_` functions.
* Fixed non-TLS connections going though proxy [#337](https://github.com/snoyberg/http-client/issues/337)

## 0.5.11

* Replaced `base64-bytestring` dependency with `memory`.

## 0.5.10

* New function to partial escape query strings

## 0.5.9

* Add `Semigroup` instances for GHC 8.4 [#320](https://github.com/snoyberg/http-client/pull/320)

## 0.5.8

* Switch to the new STM-based manager
  [#254](https://github.com/snoyberg/http-client/pull/254)
* Redact sensitive headers [#318](https://github.com/snoyberg/http-client/pull/318)

## 0.5.7.1

* Code cleanup/delete dead code
* Compat with Win32 2.6 [#309](https://github.com/snoyberg/http-client/issues/309)

## 0.5.7.0

* Support for Windows system proxy settings
  [#274](https://github.com/snoyberg/http-client/pull/274)

## 0.5.6.1

* Revert socks5 and socks5h support from
  [#262](https://github.com/snoyberg/http-client/pull/262); the support was
  untested and did not work as intended.

## 0.5.6

* Added socks5 and socks5h support [#262](https://github.com/snoyberg/http-client/pull/262)

## 0.5.5

* http-client should allow to log requests and responses [#248](https://github.com/snoyberg/http-client/issues/248)

## 0.5.4

* Derive ‘Eq’ for ‘ResponseTimeout’ [#239](https://github.com/snoyberg/http-client/pull/239)

## 0.5.3.4

* Doc improvements

## 0.5.3.3

* Add missing colon in Host header [#235](https://github.com/snoyberg/http-client/pull/235)

## 0.5.3.2

* Minor doc updates

## 0.5.3.1

* The closeConnection method for tls connections should not be called multiple
  times [#225](https://github.com/snoyberg/http-client/issues/225)

## 0.5.3

* Expose `makeConnection` and `socketConnection` as a stable API [#223](https://github.com/snoyberg/http-client/issues/223)

## 0.5.2

* Enable rawConnectionModifySocketSize to expose openSocketConnectionSize [#218](https://github.com/snoyberg/http-client/pull/218)

## 0.5.1

* Enable managerModifyRequest to modify redirectCount [#208](https://github.com/snoyberg/http-client/pull/208)

## 0.5.0.1

* Doc fix

## 0.5.0

* Remove `instance Default Request`
* Modify `instance IsString Request` to use `parseRequest` instead of `parseUrlThrow`
* Clean up the `HttpException` constructors
* Rename `checkStatus` to `checkResponse` and modify type
* Fix the ugly magic constant workaround for responseTimeout
* Remove `getConnectionWrapper`
* Add the `HttpExceptionRequest` wrapper so that all exceptions related to a
  request are thrown with that request's information

## 0.4.31

* Added length validation for RequestBodyStream [#205](https://github.com/snoyberg/http-client/pull/205)

## 0.4.30

* Initial implementation of [#193](https://github.com/snoyberg/http-client/issues/193)
    * Deprecate `parseUrl`
    * Add `parseUrlThrow`, `parseRequest`, and `parseRequest_`

## 0.4.29

* Changed the order of connecting a socket and tweaking a socket, such that the socket tweaking callback now happen before connecting.
* add setRequestIgnoreStatus [#201](https://github.com/snoyberg/http-client/pull/201)
* Added missing Host: HTTP header for https CONNECT [#192](https://github.com/snoyberg/http-client/pull/192)
* Fix: Redirects will be followed in httpRaw' when reusing a dead connection [#195](https://github.com/snoyberg/http-client/issues/195)

## 0.4.28

* Add support for including request method in URL
* `requestManagerOverride`
* `RequestBodyIO`

## 0.4.27.1

* Incorrect idle connection count in HTTP manager [#185](https://github.com/snoyberg/http-client/issues/185)

## 0.4.27

* Enable managerModifyRequest to modify checkStatus [#179](https://github.com/snoyberg/http-client/pull/179)

## 0.4.26.2

* Fix compilation for GHC 7.4

## 0.4.26.1

* Fix compilation for GHC < 7.10

## 0.4.26

* Make sure we never read from or write to closed socket [#170](https://github.com/snoyberg/http-client/pull/170)

## 0.4.25

* Don't error out when response body flushing fails [#169](https://github.com/snoyberg/http-client/issues/169)

## 0.4.24

* Use a new `TlsExceptionHostPort` exception to indicate the host and port of the server we were trying to connect to when a TLS exception occurred. See [commercialhaskell/stack#1010](https://github.com/commercialhaskell/stack/issues/1010)

## 0.4.23

* Case insensitive cookie domains [#158](https://github.com/snoyberg/http-client/issues/158)

## 0.4.22

* ProxyConnectException now returns Right HttpException. [#155](https://github.com/snoyberg/http-client/pull/155)

## 0.4.21

* Support `no_proxy` environment variable. [#140](https://github.com/snoyberg/http-client/issues/140) [#145](https://github.com/snoyberg/http-client/pull/145)

## 0.4.20

* Expose `brReadSome`

## 0.4.19

* Move HasHttpManager from http-conduit to http-client [#147](https://github.com/snoyberg/http-client/pull/147)
* Chunked request bodies use less TCP packets [#149](https://github.com/snoyberg/http-client/issues/149)

## 0.4.18

* Deprecate closeManager [#136](https://github.com/snoyberg/http-client/issues/136) [#137](https://github.com/snoyberg/http-client/issues/137)

## 0.4.17

* Case insensitive proxy environment variables [#135](https://github.com/snoyberg/http-client/issues/135)

## 0.4.16

* Proxy auth for HTTPS [#132](https://github.com/snoyberg/http-client/issues/132)

## 0.4.15

* Support proxy authentication in environment variables [#129](https://github.com/snoyberg/http-client/issues/129)

## 0.4.14

* Ignore empty `http_proxy` [#128](https://github.com/snoyberg/http-client/pull/128)

## 0.4.13

* Support for auth via url [#124](https://github.com/snoyberg/http-client/pull/124)

## 0.4.12

* Added `IsString RequestBody` instance [#126](https://github.com/snoyberg/http-client/pull/126)

## 0.4.11.3

* Fix getUri to insert "?" to uriQuery when necessary. [#123](https://github.com/snoyberg/http-client/pull/123)

## 0.4.11.2

* Removed publicsuffixlist dependency, see [Github discussion](https://github.com/litherum/publicsuffixlist/pull/7)

## 0.4.11.1

* Disable custom timeout code [#116](https://github.com/snoyberg/http-client/issues/116)

## 0.4.11

* Ignore the 'Content-Length' header if the body contains chunked data [#115](https://github.com/snoyberg/http-client/pull/115)

## 0.4.10

* Expect: 100-continue [#114](https://github.com/snoyberg/http-client/pull/114)

## 0.4.9

* Add RequestBody smart constructors `streamFile` and `streamFileObserved`, the latter with accompanying type `StreamFileStatus`.

## 0.4.8.1

* Automatically call withSocketsDo everywhere [#107](https://github.com/snoyberg/http-client/issues/107)

## 0.4.8

* Add the `ResponseLengthAndChunkingBothUsed` exception constructor [#108](https://github.com/snoyberg/http-client/issues/108)

## 0.4.7.2

* Improved `timeout` implementation for high contention cases [#98](https://github.com/snoyberg/http-client/issues/98)

## 0.4.7.1

* Fix for shared connections in proxy servers [#103](https://github.com/snoyberg/http-client/issues/103)

## 0.4.7

* [Support http\_proxy and https\_proxy environment variables](https://github.com/snoyberg/http-client/issues/94)

## 0.4.6.1

Separate tests not requiring internet access. [#93](https://github.com/snoyberg/http-client/pull/93)

## 0.4.6

Add `onRequestBodyException` to `Request` to allow for recovering from
exceptions when sending the request. Most useful for servers which terminate
the connection after sending a response body without flushing the request body.

## 0.4.5

Add `openSocketConnectionSize` and increase default chunk size to 8192.

## 0.4.4

Add `managerModifyRequest` field to `ManagerSettings`.

## 0.4.3

Add `requestVersion` field to `Request`.

## 0.4.2

The reaper thread for a manager will go to sleep completely when there are no connection to manage. See: https://github.com/snoyberg/http-client/issues/70

## 0.4.1

* Provide the `responseOpenHistory`/`withResponseHistory` API. See: https://github.com/snoyberg/http-client/pull/79

## 0.4.0

* Hide the `Part` constructor, and allow for additional headers. See: https://github.com/snoyberg/http-client/issues/76
