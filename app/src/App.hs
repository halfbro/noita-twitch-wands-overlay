module App (runApp, runTui) where

import qualified Routes
import qualified Channel

-- On Request:
-- 1. If no stream exists yet, make OnlyWands stream
-- 2. Set up stream to send broadcast updates to Twitch

runApp :: IO ()
runApp =
  Routes.runApi

runTui :: IO ()
runTui =
  let
    loop = do
      putStr "backend > "
      command <- getLine
      case command of
        "show-active-streamers" -> do
          users <- Channel.getAllWritableChannelUsernames
          mapM_ putStrLn users
        "help" -> putStrLn "help show-active-streamers"
        _ -> putStrLn "Invalid command"
      loop
  in
  loop

