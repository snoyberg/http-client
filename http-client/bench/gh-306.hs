-- This is a small benchmark meant to see how fast the http-client library can
-- make requests. In particular, it's meant to compare the network library and
-- the socket library. <https://github.com/snoyberg/http-client/pull/306>

import qualified Control.Concurrent.Async as Async
import qualified Control.Monad as Monad
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Time as Time
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Printf as Printf

main :: IO ()
main = do
  let
    status = Http.status200
    body = LazyText.encodeUtf8 (LazyText.pack "null")
    port = 8080
    server = Warp.run port (\ _ respond -> respond (Wai.responseLBS
      status
      [(Http.hContentType, Text.encodeUtf8 (Text.pack "application/json"))]
      body))
    threads = 4
    count = 10000

  thread <- Async.async server
  request <- Client.parseRequest ("http://localhost:" ++ show port)
  manager <- Client.newManager Client.defaultManagerSettings
  start <- Time.getCurrentTime
  Async.replicateConcurrently_ threads (Monad.replicateM_ count (do
    response <- Client.httpLbs request manager
    Monad.guard (Client.responseStatus response == status)
    Monad.guard (Client.responseBody response == body)))
  end <- Time.getCurrentTime
  Async.cancel thread

  let elapsed = Time.diffUTCTime end start
  let rate = fromIntegral count / realToFrac elapsed
  Printf.printf
    "%d threads\n\
    \%d requests per thread\n\
    \%.1f requests per second\n"
    threads
    count
    (rate :: Double)
