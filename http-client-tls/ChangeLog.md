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
