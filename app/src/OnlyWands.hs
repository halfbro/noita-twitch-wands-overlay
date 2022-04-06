{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module OnlyWands
  ( streamWandsForStreamer,
  )
where

import Control.Concurrent (forkIO)
import Control.Monad (forever, unless)
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (Object),
    decode,
    object,
    (.:),
    (.=),
  )
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Search (breakAfter, breakOn)
import GHC.Base (mzero)
import GHC.Generics (Generic)
import Network.HTTP.Req
  ( BsResponse,
    GET (GET),
    NoReqBody (NoReqBody),
    bsResponse,
    defaultHttpConfig,
    https,
    req,
    responseBody,
    runReq,
    (/:),
    (/~),
  )
import Network.WebSockets (receiveData)
import Storage (insertStreamerData)
import Twitch (streamerStoppedStreaming)
import Types (Inventory, Wand)
import qualified Util
import Wuss (runSecureClient)

data SocketData = SocketData
  { messageType :: String,
    wands :: [Wand],
    inventory :: Inventory
  }
  deriving (Generic, Show, Eq)

instance FromJSON SocketData where
  parseJSON (Object v) =
    SocketData
      <$> v .: "type"
      <*> v .: "wands"
      <*> v .: "inventory"
  parseJSON _ = mzero

instance ToJSON SocketData where
  toJSON sd =
    object
      [ "type" .= messageType sd,
        "wands" .= wands sd,
        "inventory" .= inventory sd
      ]

parseWandFromHttpResponse :: BsResponse -> Maybe ([Wand], Inventory)
parseWandFromHttpResponse res = do
  let html = (responseBody res :: S.ByteString)
  let wandDataStart = snd $ breakAfter "const streamerWands = " html
  let inventoryDataStart = snd $ breakAfter "const streamerInventory = " html
  let wandDataJson = fst $ breakOn "const streamerInventory" wandDataStart
  let inventoryDataJson = fst $ breakOn "</script>" inventoryDataStart
  wandData <- decode $ L.fromStrict wandDataJson
  inventoryData <- decode $ L.fromStrict inventoryDataJson
  return (wandData, inventoryData)

getInitialWandsForStreamer :: String -> IO (Maybe ([Wand], Inventory))
getInitialWandsForStreamer streamerName = do
  -- Get initial state from http get
  let runReq' = runReq defaultHttpConfig
  let url = https "onlywands.com" /: "streamer" /~ streamerName
  response <- runReq' $ req GET url NoReqBody bsResponse mempty
  return $ parseWandFromHttpResponse response

startWandStreamingForStreamer :: String -> IO ()
startWandStreamingForStreamer streamerName = do
  let urlLoc = "/client=" ++ streamerName
  runSecureClient "onlywands.com" 443 urlLoc $ \conn ->
    forever $ do
      msg <- receiveData conn
      case decode msg of
        Nothing -> return ()
        Just
          SocketData
            { wands = wands,
              inventory = inventory
            } ->
            insertStreamerData streamerName wands inventory

streamWandsForStreamer :: String -> IO ()
streamWandsForStreamer streamerName = do
  initialWands <- getInitialWandsForStreamer streamerName
  case initialWands of
    Nothing -> return ()
    Just (wands, inventory) ->
      insertStreamerData streamerName wands inventory

  threadId <- forkIO $ startWandStreamingForStreamer streamerName

  let loop = do
        -- while the user is online and has game set to 'Noita'
        Util.sleepSeconds 300
        --Util.sleepSeconds 10
        stopped <- streamerStoppedStreaming streamerName
        unless stopped loop
  loop
