module App (runApp) where

import qualified Routes

-- On Request:
-- 1. If no stream exists yet, make OnlyWands stream
-- 2. Set up stream to send broadcast updates to Twitch

runApp :: IO ()
runApp =
  Routes.runApi
