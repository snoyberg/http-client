#!/usr/bin/env stack
{- stack --install-ghc exec
     --package typed-process
     --package http-conduit
     --package hspec
     --package safe-exceptions

     -- ghc -O2 -threaded -Wall -Werror
-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Exception.Safe
import           Control.Monad           (forM_, void)
import qualified Data.ByteString         as B
import           Network.HTTP.Client     (HttpExceptionContent (..), newManager)
import           Network.HTTP.Client.TLS (setGlobalManager, tlsManagerSettings)
import           Network.HTTP.Simple
import           System.Directory        (copyFile, createDirectoryIfMissing)
import           System.Environment      as E
import           System.FilePath         (takeDirectory, (</>))
import           System.Process.Typed
import           Test.Hspec

-- | Main entry point. No arguments: launch the wrapper which will kick
-- off the Docker containers. If given inner, runs the test suite
-- inside the Docker container.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> outer
    -- withArgs hides the "inner" argument to avoid confusing hspec
    ["inner"] -> withArgs [] inner
    _ -> error $ "Invalid args: " ++ show args

-- | Run from outside the container
outer :: IO ()
outer = do
      -- Kill a previously launched squid container. We ignore any
      -- errors in case the container isn't running.
  let killit = do
        let run = void
                . runProcess
                . setStdin closed
                . setStdout closed
                . setStderr closed
        run $ proc "docker" $ words "kill http-client-squid"
        run $ proc "docker" $ words "rm http-client-squid"

      -- How to launch the Squid proxy container
      squid = proc "docker"
            $ words "run -d --name=http-client-squid sameersbn/squid:3.3.8-23"

  -- Create the Docker image to run the test suite itself, containing
  -- this executable. NOTE: I tried initially just bind-mounting the
  -- executable into the running container, but this failed on Gitlab
  -- which is using Docker-within-Docker.
  let dockerfile = "/tmp/http-client-base/Dockerfile"
      dockerfileBS =
        "FROM fpco/pid1:16.04\n\
        \RUN apt-get update\n\
        \RUN apt-get install -y iptables curl ca-certificates netbase\n\
        \COPY test-suite /usr/bin/test-suite\n"

  -- Check if the Dockerfile is out of date. We want to avoid updating
  -- if we can to not invalidate the Docker cache.
  ecurrent <- tryIO $ B.readFile dockerfile
  case ecurrent of
    Right current | current == dockerfileBS -> return ()
    _ -> do
      createDirectoryIfMissing True $ takeDirectory dockerfile
      B.writeFile dockerfile dockerfileBS

  -- Copy over the executable to the temporary directory
  exeName <- getExecutablePath
  copyFile exeName (takeDirectory dockerfile </> "test-suite")

  -- Now build the image
  runProcess_ $ proc "docker"
    [ "build"
    , "--tag"
    , "http-client-base"
    , takeDirectory dockerfile
    ]

  -- Launch the squid container, killing a preexisting one if it
  -- exists, and then kill the container when we're done.
  bracket_ (killit >> readProcess_ squid) killit $ do
    -- Run the test suite itself inside a Docker container. We need
    -- --privileged to modify the iptables rules.
    runProcess_ $ proc "docker" $ words
      "run --rm -t --link http-client-squid:squid --privileged\n\
      \http-client-base /usr/bin/test-suite inner"

-- | Run a set of tests with the given environment variable key/value
-- pair set. Reload the global HTTP manager each time.
describe' :: String -- ^ key
          -> String -- ^ value
          -> Spec
          -> Spec
describe' key val =
  describe name . around_ (bracket set unset . const)
  where
  name = concat [key, "=", val]
  set = do
    morig <- lookupEnv key
    E.setEnv key val
    man <- newManager tlsManagerSettings
    setGlobalManager man
    return morig
  unset mval = do
    case mval of
      Nothing -> unsetEnv key
      Just val' -> E.setEnv key val'
    setGlobalManager $ error "should not be used"

-- | Base URL for HTTP or HTTPS httpbin site
base :: Bool -> String
base False = "http://httpbin.org/"
base True = "https://httpbin.org/"

-- | Ensure that connections fail.
fails :: Bool -- ^ use HTTPS?
      -> Spec
fails https = it ("fails to connect " ++ (if https then "secure" else "insecure")) $
  httpLBS (parseRequest_ (base https)) `shouldThrow` (\e ->
  case e of
    HttpExceptionRequest _ (ConnectionFailure _)  -> True
    HttpExceptionRequest _ (InternalException _)  -> True
    _ -> False)

-- | Ensure that connections succeed.
succeeds :: Bool -- ^ use HTTPS?
         -> Spec
succeeds https = describe ("succeeds " ++ (if https then "secure" else "insecure")) $ do
  describe "basic status code check" $ forM_ [200, 400, 500] $ \code -> it (show code) $ do
    res <- httpLBS (parseRequest_ (base https ++ "status/" ++ show code))
    getResponseStatusCode res `shouldBe` code

-- | Code to run the test suites themselves inside the Docker container.
inner :: IO ()
inner = do
  -- Block all outgoing connections to everything except squid
  runProcess_ $ proc "iptables" $ words "-A OUTPUT -p tcp -d squid -j ACCEPT"
  runProcess_ $ proc "iptables" $ words "-A OUTPUT -p tcp -j REJECT"

  -- Run the test suite
  hspec $ do
    describe "no proxy" $ do
      fails False
      fails True
    let values =
          [ "http://squid:3128"
          , "squid:3128"
          , "socks5://squid:3128"
          , "socks5h://squid:3128"
          ]
    forM_ values $ \value -> do
      describe' "http_proxy" value $ do
        succeeds False
        fails True
      describe' "https_proxy" value $ do
        succeeds True
        fails False
