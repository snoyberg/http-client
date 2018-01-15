module Main where

import Gauge.Main
import Network.HTTP.Client
import Network.HTTP.Client.TLS

main :: IO ()
main = defaultMain [
      bgroup "newManager" [
            bench "defaultManagerSettings" $
                whnfIO (newManager defaultManagerSettings)
          , bench "tlsManagerSettings" $
                whnfIO (newManager tlsManagerSettings)
          ]
    ]
