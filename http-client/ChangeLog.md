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
