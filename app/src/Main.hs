module Main where

import App (runApp)

main :: IO ()
main = do
  putStrLn "Starting server"
  runApp
