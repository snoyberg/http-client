Status :
[![Build Status](https://travis-ci.org/snoyberg/http-client.svg?branch=master)](https://travis-ci.org/search/snoyberg%2Fhttp-client)

This is a mega-repo for housing the http-client family of packages for Haskell.
These packages provide a low level HTTP client engine (http-client), different
backends for providing SSL support (http-client-tls and http-client-openssl),
and higher-level APIs for user convenience (http-conduit).

If you're just getting started, it is recommended to use http-conduit, which
[is documented in the Yesod book](http://www.yesodweb.com/book/http-conduit).

Do to Haddock pulling the documentation directly from http-client, some "Since"
notations give the version numbers of the http-client package. For reference,
http-client-0.5.0 corresponds to http-conduit-2.2.0, and 
http-client-0.4.30 corresponds to http-conduit-2.1.11 .
