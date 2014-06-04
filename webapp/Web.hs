module Main where

import Data.String (fromString)

import Network.Wai.Handler.Warp

import Application (tarzanApplication)

host :: String
host = "127.0.0.1"

port :: Int
port = 8080

settings :: Settings
settings = setPort port $ setHost (fromString host) defaultSettings

main :: IO ()
main = do
	putStrLn $ "http://" ++ host ++ ":" ++ show port
	runSettings settings tarzanApplication
