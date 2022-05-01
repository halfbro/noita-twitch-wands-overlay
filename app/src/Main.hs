module Main where

import qualified Util
import qualified OnlyWands
import Channel (streamFromChannel)
import Twitch (isStreamingNoita)
import App (runApp)
import qualified Twitch.Auth
import qualified Twitch as Twitch.Api

main :: IO ()
main = do
  putStrLn "Starting server"
  runApp
