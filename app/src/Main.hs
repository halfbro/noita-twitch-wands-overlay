module Main where

import qualified Util
import qualified OnlyWands
import Channel (streamFromChannel)
import Twitch (isStreamingNoita)
import App (runApp)

main :: IO ()
main = do
  --chan <- OnlyWands.getBroadcastChannelForStreamer (isStreamingNoita "Aliasbot") "Aliasbot"
  --streamFromChannel chan print
  putStrLn "Starting WebSocket server"
  runApp
