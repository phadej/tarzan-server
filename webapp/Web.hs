module Main where

import Control.Applicative

import Data.String (fromString)
import System.Environment (lookupEnv)

import Network.Wai.Handler.Warp

import Application (tarzanApplication)

host :: String
host = "127.0.0.1"

portIO :: IO Int
portIO = maybe 8080 read <$> lookupEnv "PORT"

settings :: Int -> Settings
settings port = setPort port $ setHost (fromString host) defaultSettings

main :: IO ()
main = do
	port <- portIO
	putStrLn $ "http://" ++ host ++ ":" ++ show port
	runSettings (settings port) tarzanApplication
