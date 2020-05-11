## 0.3.1.0
* Fix a bug with http-proxy that would cause SNI to be set incorrectly; (would
  use the domain of the proxy, instead of the server we're trying to reach
  _through_ the proxy)

## 0.3.0.0

* Wrap HsOpenSSL specific exceptions into http-clients own `HttpExceptionRequest`. This is a breaking change and might need adjustment with respect to exception handling in user code.
* More robust handling of unexpectedly closed connections

## 0.2.2.0

* Tell OpenSSL what host is being contacted, so it can use the SNI extension for certificate selection if the server requires it.

## 0.2.1.1

* Fix a connection-bug with http-proxy(Previous version closes a connection before reading all respose-data.)

## 0.2.1.0

* Add support for http-proxy

## 0.2.0.5

* Use different domain name so tests pass

## 0.2.0.4

* Minor doc updates

## 0.2.0.3

* Simplified a test case

## 0.2.0.1

[Expand hints for getAddrInfo. More exception safety.](https://github.com/snoyberg/http-client/pull/91)
