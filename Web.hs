module Main where

import Text.Tarzan.Application
import Network.Wai.Handler.Warp

main :: IO ()
main = run 8080 tarzanApplication
