module Main where

import Control.Applicative

import Text.Tarzan.Evaluator (execute)

main :: IO ()
main = (execute <$> getContents) >>= putStr
