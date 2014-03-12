{-# LANGUAGE OverloadedStrings #-}
import           Control.Concurrent               (threadDelay)
import           Control.Concurrent.Async         (mapConcurrently, withAsync)
import           Control.Exception.Enclosed       (tryAny)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (execStateT, get, put)
import qualified Data.ByteString.Lazy             as L
import           Data.Time                        (diffUTCTime, getCurrentTime)
import qualified Network.HTTP.Client              as HC
import           Network.HTTP.Types               (ResponseHeaders, status200)
import qualified Network.Wai                      as W
import qualified Network.Wai.Handler.Warp         as Warp
import qualified System.Random.MWC                as MWC

threads = 40
perThread = 3000

main :: IO ()
main = do
    gen <- MWC.createSystemRandom
    withAsync (Warp.run 4567 (app gen)) $ \_ -> do
        threadDelay 1000000
        man <- HC.newManager HC.defaultManagerSettings
        res <- mapConcurrently (client man gen) [1..threads]
        mapM_ putStrLn res

headers :: ResponseHeaders
headers =
    [ ("foo", "bar")
    , ("baz", "bin")
    ]

body :: L.ByteString
body = "Hello World!"

app :: MWC.GenIO -> W.Application
app gen _ = do
    delay <- MWC.uniformR (50000, 150000) gen
    threadDelay delay
    return $ W.responseLBS status200 headers body

req :: HC.Request
req =
    case HC.parseUrl "http://localhost:4567" of
        Nothing -> error "bad request"
        Just x -> x

client :: HC.Manager -> MWC.GenIO -> Int -> IO String
client man gen threadid = do
    (good, bad) <- execStateT (mapM_ single [1..perThread]) (0, 0)
    return $ concat
        [ "Successes: "
        , show good
        , ". Failures: "
        , show bad
        ]
  where
    single reqid = do
        isGood <- lift $ do
            -- Random delay to try and trigger race conditions
            delay <- MWC.uniformR (5000, 20000) gen
            threadDelay delay

            before <- getCurrentTime
            eres <- tryAny $ HC.httpLbs req man
            after <- getCurrentTime
            return (before, eres, after)
            case eres of
                Left e -> do
                    putStrLn $ "Some exception occurred: " ++ show (threadid, reqid, e)
                    return False
                Right res
                    | filter checkedHeader (HC.responseHeaders res) /= headers -> do
                        putStrLn $ "Invalid headers: " ++ show (HC.responseHeaders res)
                        return False
                    | HC.responseBody res /= body -> do
                        putStrLn $ "Invalid body: " ++ show (HC.responseBody res)
                        return False
                    | diffUTCTime after before > 0.2 -> do
                        putStrLn $ "Took too long: " ++ show (diffUTCTime after before)
                        return False
                    | otherwise -> return True
        (good, bad) <- get
        put $!
            if isGood
                then (good + 1, bad)
                else (good, bad + 1)

    checkedHeader (k, _) = k `notElem` ["transfer-encoding", "date", "server"]
