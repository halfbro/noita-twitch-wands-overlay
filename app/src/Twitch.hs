module Twitch (isStreamingNoita, sendWandInfoBroadcast, getTwitchUsername) where

import Control.Exception (Exception, throw)
import Data.Aeson (encode)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Servant (NoContent (NoContent))
import Twitch.Api
  ( ChannelInformation (broadcaster_name),
    TwitchResponse (TwitchResponse, _data),
    StreamInformation(game_id),
    getActiveStreamByName,
    getChannelInformation,
    sendPubSubMessage,
  )
import Twitch.Auth
  ( TwitchJwt (TwitchJwt),
    makeTwitchJwt,
    twitchJwtSettings,
  )
import Twitch.Types
  ( PubSubMessage (broadcaster_id, message),
    broadcastMessage,
  )
import Types (StreamerInformation)

data TwitchException
  = RequestFailed
  deriving (Show)

instance Exception TwitchException

noitaGameId = "505705"

isStreamingNoita :: String -> IO Bool
isStreamingNoita streamerName = do
  streamInfoRes <- getActiveStreamByName streamerName
  case streamInfoRes of
    Right TwitchResponse {_data = [info]} ->
      return $ game_id info == noitaGameId
    _ -> return False

getTwitchUsername :: String -> IO String
getTwitchUsername channelId = do
  channelInfo <- Twitch.Api.getChannelInformation channelId
  case channelInfo of
    Right TwitchResponse {_data = [info]} -> return $ broadcaster_name info
    _ -> throw RequestFailed

sendWandInfoBroadcast :: String -> StreamerInformation -> IO ()
sendWandInfoBroadcast channelId info = do
  jwt <- makeTwitchJwt channelId
  let d = decodeUtf8 $ toStrict $ encode info
  streamInfoRes <- sendPubSubMessage jwt $ broadcastMessage {broadcaster_id = T.pack channelId, message = d}
  case streamInfoRes of
    Left err -> print err
    Right NoContent -> return ()
