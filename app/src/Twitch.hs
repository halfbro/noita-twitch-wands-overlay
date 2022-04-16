module Twitch (isStreamingNoita) where

import Twitch.Auth (TwitchJwt, twitchJwtSettings)
import Twitch.Api (getActiveStreamByName, TwitchResponse (..), game_id)

noitaGameId = "505705"

isStreamingNoita :: String -> IO Bool
isStreamingNoita streamerName = do
  streamInfoRes <- getActiveStreamByName streamerName
  case streamInfoRes of
    Left err -> return False
    Right TwitchResponse {_data = [info]} ->
      return $ game_id info == noitaGameId
    _ -> return False
