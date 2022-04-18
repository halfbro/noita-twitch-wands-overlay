module App (runApp) where

import Channel (streamFromChannel)
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Char8 as S
import qualified OnlyWands
import Twitch (isStreamingNoita)
import Data.Aeson (encode)

-- On Request:
-- 1. Get OnlyWands stream
-- 2. Create websocket server connection
-- 3. Send updates from stream to connection

runApp :: IO ()
runApp =
  WS.runServer "127.0.0.1" 9160 app

app :: WS.ServerApp
app pendingConnection = do
  case shouldAccept pendingConnection of
    Nothing -> do
      print "nah"
      WS.rejectRequest pendingConnection "nah"
    Just streamerName -> do
      print streamerName
      conn <- WS.acceptRequest pendingConnection
      serve conn streamerName

shouldAccept :: WS.PendingConnection -> Maybe String
shouldAccept conn =
  let head = WS.pendingRequest conn in
    --if True || WS.requestSecure head then
      Just . S.unpack . S.tail $ WS.requestPath head
    --else
      --Nothing

serve :: WS.Connection -> String -> IO ()
serve conn streamerName= do
  chan <- OnlyWands.getBroadcastChannelForStreamer (isStreamingNoita streamerName) streamerName
  streamFromChannel chan (WS.sendTextData conn <$> encode)
