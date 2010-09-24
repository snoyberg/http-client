{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.SimpleServer
import Data.ByteString.UTF8 (toString)
import Data.ByteString.Lazy.UTF8 (fromString)

main = run 3000 $ \req -> return $ Response status200 [("Content-Type", "plain")] $ ResponseLBS $ fromString $ unlines
    [ "Path info: " ++ toString (pathInfo req)
    , "Query string: " ++ toString (queryString req)
    ]
