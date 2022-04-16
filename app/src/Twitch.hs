module Twitch (streamerStoppedStreaming, TwitchJwt, twitchJwtSettings) where

import Twitch.Auth (TwitchJwt, twitchJwtSettings)

streamerStoppedStreaming :: String -> IO Bool
streamerStoppedStreaming streamerName =
  -- TODO
  return False
