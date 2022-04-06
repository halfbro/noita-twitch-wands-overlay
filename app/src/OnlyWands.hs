{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module OnlyWands
  ( getBroadcastChannelForStreamer,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( STM,
    TChan,
    atomically,
    newBroadcastTChan,
    newTChan,
    newTVar,
    orElse,
    readTVarIO,
    writeTChan,
    writeTVar, dupTChan
  )
import Control.Monad (forever, unless, void)
import Control.Monad.Cont (liftIO)
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
import Data.Maybe (fromMaybe)
import GHC.Base (mzero)
--import Storage (insertStreamerData)

--import Storage (insertStreamerData)

import GHC.Conc (unsafeIOToSTM)
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
import qualified Network.WebSockets as WS
import Storage (lookupStreamerBroadcastChannel, registerStreamerBroadcastChannel)
import Twitch (streamerStoppedStreaming)
import Types (Inventory, StreamerInformation (..), Wand)
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

parseWandFromHttpResponse :: BsResponse -> Maybe StreamerInformation
parseWandFromHttpResponse res = do
  let html = (responseBody res :: S.ByteString)
  let wandDataStart = snd $ breakAfter "const streamerWands = " html
  let inventoryDataStart = snd $ breakAfter "const streamerInventory = " html
  let wandDataJson = fst $ breakOn "const streamerInventory" wandDataStart
  let inventoryDataJson = fst $ breakOn "</script>" inventoryDataStart
  wandData <- decode $ L.fromStrict wandDataJson
  inventoryData <- decode $ L.fromStrict inventoryDataJson
  return (wandData, inventoryData)

getInitialWandsForStreamer :: String -> IO StreamerInformation
getInitialWandsForStreamer streamerName = do
  -- Get initial state from http get
  let runReq' = runReq defaultHttpConfig
  let url = https "onlywands.com" /: "streamer" /~ streamerName
  response <- runReq' $ req GET url NoReqBody bsResponse mempty
  return . fromMaybe ([], []) $ parseWandFromHttpResponse response

startWandStreamingForStreamer :: String -> IO (TChan StreamerInformation)
startWandStreamingForStreamer streamerName = do
  (newStream, cancelToken) <- atomically $ registerStreamerBroadcastChannel streamerName
  let signalStop () =
        atomically $
          writeTVar cancelToken True

  print $ "Start fetching data for " ++ streamerName
  -- Start 2 forked threads:
  -- one thread handles listening to onlywands and pushing new data
  -- the other handles watching the stream to check the streamer is
  --   still playing Noita
  let urlLoc = "/client=" ++ streamerName
  forkIO . runSecureClient "onlywands.com" 443 urlLoc $ \conn -> do
    initialWands <- getInitialWandsForStreamer streamerName
    atomically $ writeTChan newStream initialWands

    let fetchLoop = do
          stopSignalled <- readTVarIO cancelToken
          if stopSignalled then
            WS.sendClose conn ("Closing" :: S.ByteString)
          else do
            msg <- WS.receiveData conn
            case decode msg of
              Nothing -> return ()
              Just
                SocketData
                  { wands = wands,
                    inventory = inventory
                  } ->
                  atomically $ writeTChan newStream (wands, inventory)
            fetchLoop
    fetchLoop


    print $ "End fetching data for " ++ streamerName

    let readRestOfMessages = do
          asdf <- WS.receiveDataMessage conn
          forever $ void $ WS.receiveDataMessage conn
          readRestOfMessages
    readRestOfMessages

    print $ "Closing socket for " ++ streamerName

    return ()

  return newStream

getBroadcastChannelForStreamer :: String -> IO (TChan StreamerInformation)
getBroadcastChannelForStreamer streamerName = do
  channel <- atomically $ lookupStreamerBroadcastChannel streamerName
  case channel of
    Just existing -> atomically $ dupTChan existing
    Nothing -> do
      newChannel <- startWandStreamingForStreamer streamerName
      atomically $ dupTChan newChannel

