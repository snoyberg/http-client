# http-client megarepo

[![Build Status](https://dev.azure.com/snoyberg/http-client/_apis/build/status/snoyberg.http-client?branchName=master)](https://dev.azure.com/snoyberg/http-client/_build/latest?definitionId=2&branchName=master)

This is a mega-repo for housing the http-client family of packages for Haskell.
These packages provide a low level HTTP client engine (http-client), different
backends for providing SSL support (http-client-tls and http-client-openssl),
and higher-level APIs for user convenience (http-conduit).

Please check out the [tutorial](TUTORIAL.md).

Due to Haddock pulling the documentation directly from http-client, some "Since"
notations give the version numbers of the http-client package. For reference,
http-client-0.5.0 corresponds to http-conduit-2.2.0, and 
http-client-0.4.30 corresponds to http-conduit-2.1.11 .
