{-# LANGUAGE CPP #-}
module Main where

#if MIN_VERSION_gauge(0, 2, 0)
import Gauge
#else
import Gauge.Main
#endif
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
