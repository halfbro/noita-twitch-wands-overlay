module Main where

import Routes (runApi)
import qualified Util
import qualified OnlyWands
import Channel (streamFromChannel)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let onlineCheck = do Util.sleepSeconds 1; print "ping"; return True
  chan <- OnlyWands.getBroadcastChannelForStreamer onlineCheck "xytio"
  streamFromChannel chan print
  runApi
