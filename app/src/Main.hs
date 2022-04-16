module Main where

import qualified Util
import qualified OnlyWands
import Channel (streamFromChannel)
import Twitch (isStreamingNoita)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  chan <- OnlyWands.getBroadcastChannelForStreamer (isStreamingNoita "DunkOrSlam") "DunkOrSlam"
  streamFromChannel chan print
