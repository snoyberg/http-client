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
