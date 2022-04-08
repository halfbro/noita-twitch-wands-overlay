{-# LANGUAGE OverloadedStrings #-}

module OnlyWands
  ( getBroadcastChannelForStreamer,
  )
where

--import Storage (insertStreamerData)

--import Storage (insertStreamerData)

import qualified Channel
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Monad (void, mzero)
import Control.Monad.Cont (liftIO)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object), decode, object, (.:), (.=))
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Search (breakAfter, breakOn)
import Data.Maybe (fromMaybe)
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
import Types (Inventory, StreamerInformation (..), Wand)
import qualified Util
import Wuss (runSecureClient)

data SocketData = SocketData
  { messageType :: String,
    wands :: [Wand],
    inventory :: Inventory
  }
  deriving (Show, Eq)

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

startWandStreamingForStreamer ::
  String ->
  Channel.WriteChannel ->
  Channel.StopToken ->
  IO Bool ->
  IO Channel.ReadChannel
startWandStreamingForStreamer streamerName chan t onlineCheck = do
  stopToken <- newTVarIO False
  let signalStop = do
        print $ "Stop signalled for " ++ streamerName
        atomically $ writeTVar stopToken True
        Channel.stopChannel t

  print $ "Start fetching data for " ++ streamerName

  -- Start 2 forked threads:
  -- one thread handles listening to onlywands and pushing new data
  -- the other handles watching the stream to check the streamer is
  --   still playing Noita
  let urlLoc = "/client=" ++ streamerName
  let fetchOnlyWandsStream =
        runSecureClient "onlywands.com" 443 urlLoc $ \conn -> do
          initialWands <- getInitialWandsForStreamer streamerName
          Channel.broadcastToChannel chan initialWands

          let fetchLoop = do
                stopSignalled <- readTVarIO stopToken
                if stopSignalled
                  then do
                    print "Closing socket"
                    WS.sendClose conn ("Closing" :: S.ByteString)
                  else do
                    msg <- WS.receiveData conn
                    case decode msg of
                      Nothing -> do
                        print msg
                        return ()
                      Just
                        SocketData
                          { wands = wands,
                            inventory = inventory
                          } -> do
                          print $ "Got new wand data for " ++ streamerName
                          Channel.broadcastToChannel chan (wands, inventory)
                    fetchLoop
          fetchLoop

          print $ "End fetching data for " ++ streamerName

          let clearInFlightMessages = do
                print $ "Clearing websocket for " ++ streamerName
                void $ WS.receiveDataMessage conn
                clearInFlightMessages
          clearInFlightMessages

  let checkStreamerOnline = do
        Util.sleepSeconds 30
        online <- onlineCheck
        if not online
          then signalStop
          else checkStreamerOnline

  t1 <- forkIO fetchOnlyWandsStream
  t2 <- forkIO checkStreamerOnline
  print t1
  print t2

  Channel.makeReadChannel chan

getBroadcastChannelForStreamer :: IO Bool -> String -> IO Channel.ReadChannel
getBroadcastChannelForStreamer onlineCheck streamerName = do
  channel <- Channel.getChannel streamerName
  case channel of
    Channel.Existing readChan -> return readChan
    Channel.NewBroadcast (newChan, token) -> do
      startWandStreamingForStreamer streamerName newChan token onlineCheck