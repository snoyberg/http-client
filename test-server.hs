{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.SimpleServer
import Network.Wai.Parse
import Data.ByteString.UTF8 (toString)
import Data.ByteString.Lazy.UTF8 (fromString)

main = run 3000 $ \req -> do
    body <- parseRequestBody lbsSink req
    return $ Response status200 [("Content-Type", "plain")] $ ResponseLBS $ fromString $ unlines
        [ "Path info: " ++ toString (pathInfo req)
        , "Query string: " ++ toString (queryString req)
        , "Request body: " ++ show body
        , "Method: " ++ toString (requestMethod req)
        ]
