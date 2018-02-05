## 0.3.5.3

* Fix `newTlsManager` [#325](https://github.com/snoyberg/http-client/issues/325)

## 0.3.5.2

* [#289](https://github.com/snoyberg/http-client/issues/289):
  Keep original `TLSSettings` when creating a `Manager` using `newTlsManagerWith`.

## 0.3.5.1

* Also catch TLSError exceptions [#273](https://github.com/snoyberg/http-client/pull/273)

## 0.3.5

* Add `newTlsManagerWith`
  [#278](https://github.com/snoyberg/http-client/issues/278), which
  provides a variant of `newTlsManager` that takes a `ManagerSettings`
  to base its settings off of.

## 0.3.4.2

* Never throw exceptions on 401 status in `applyDigestAuth`

## 0.3.4.1

* Better exception cleanup behavior

## 0.3.4

* Add 'newTlsManager'
  [#263](https://github.com/snoyberg/http-client/issues/263), which adds
  support for respecting `socks5://` and `socks5h://` `http_proxy` and
  `https_proxy` environment variables.

## 0.3.3.2

* Better handling of internal exceptions

## 0.3.3.1

* Better exception safety via `bracketOnError`

## 0.3.3

* Add `DigestAuthException` and generalize `applyDigestAuth`
* Global manager uses a shared TLS context (faster init)

## 0.3.2

* Add `mkManagerSettingsContext` [#228](https://github.com/snoyberg/http-client/issues/228)

## 0.3.1.1

* Minor doc updates

## 0.3.1

* Add `applyDigestAuth`

## 0.3.0

* Support http-client 0.5

## 0.2.4.1

* Cabal description fix

## 0.2.4

* Global manager

## 0.2.3

* Exception catching cleanup
