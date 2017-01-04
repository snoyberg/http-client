import Network.HTTP.Client

main :: IO ()
main = do
    req <- parseRequest "http://httpbin.org/ip"
    man <- newManager defaultManagerSettings
    httpLbs req man >>= print
