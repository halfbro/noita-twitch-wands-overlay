module App (runApp) where

import Channel (streamFromChannel)
import Control.Applicative
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as S
import qualified Network.WebSockets as WS
import qualified Routes
import Twitch (isStreamingNoita, sendWandInfoBroadcast)
import Twitch.Api (StreamInformation (..), TwitchResponse (..), getActiveStreamByName)

-- On Request:
-- 1. If no stream exists yet, make OnlyWands stream
-- 2. Set up stream to send broadcast updates to Twitch

runApp :: IO ()
runApp =
  Routes.runApi
