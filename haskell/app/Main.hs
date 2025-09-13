module Main where

import App

main :: IO ()
main = do
    putStrLn "Starting server on port 8080..."
    startApp