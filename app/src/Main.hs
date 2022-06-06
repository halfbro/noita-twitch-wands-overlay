module Main where

import App (runApp, runTui)
import Control.Concurrent (forkIO)

main :: IO ()
main = do
  putStrLn "Starting server"
  forkIO runApp
  runTui
