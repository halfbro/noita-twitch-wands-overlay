module Twitch (streamerStoppedStreaming, TwitchAuth, TwitchJwt) where

import Twitch.Api (TwitchAuth)
import Twitch.Auth (TwitchJwt)

streamerStoppedStreaming :: String -> IO Bool
streamerStoppedStreaming streamerName =
  -- TODO
  return False
