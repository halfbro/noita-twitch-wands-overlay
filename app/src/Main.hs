module Main where

import Routes (runApi)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  runApi
