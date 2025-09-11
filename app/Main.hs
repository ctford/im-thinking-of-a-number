module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Starting server on port 8080..."
    startApp