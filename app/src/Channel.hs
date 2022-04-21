module Channel
  ( broadcastToChannel,
    getChannel,
    makeReadChannel,
    stopChannel,
    streamFromChannel,
    ChannelResult (..),
    ReadChannel,
    WriteChannel,
    StopToken,
  )
where

import Control.Concurrent.STM
  ( TChan,
    atomically,
    dupTChan,
    newBroadcastTChan,
    readTChan,
    writeTChan,
  )
import GHC.IO (unsafePerformIO)
import StmContainers.Map as StmMap (Map, delete, insert, lookup, newIO)
import Types (StreamerInformation, blankStreamerInformation)

data ChannelMessage
  = Data StreamerInformation
  | Closing

newtype WriteChannel = WriteChannel (TChan ChannelMessage, String)

newtype ReadChannel = ReadChannel (TChan ChannelMessage)

newtype StopToken = Token String

type StreamerDataMap = StmMap.Map String (TChan ChannelMessage, StreamerInformation)

data ChannelResult
  = Existing (ReadChannel, StreamerInformation)
  | NewBroadcast (WriteChannel, StopToken)

{-# NOINLINE writableChannels #-}
writableChannels :: StreamerDataMap
writableChannels = unsafePerformIO StmMap.newIO

makeReadChannel :: WriteChannel -> IO ReadChannel
makeReadChannel (WriteChannel (chan, _)) = atomically $ ReadChannel <$> dupTChan chan

broadcastToChannel :: WriteChannel -> StreamerInformation -> IO ()
broadcastToChannel (WriteChannel (chan, name)) info =
  atomically $ do
    StmMap.insert (chan, info) name writableChannels
    writeTChan chan $ Data info

streamFromChannel :: ReadChannel -> (StreamerInformation -> IO ()) -> IO ()
streamFromChannel (ReadChannel chan) f = do
  let loop = do
        msg <- atomically $ readTChan chan
        case msg of
          Closing -> return ()
          Data d -> do
            f d
            loop
  loop

stopChannel :: StopToken -> IO ()
stopChannel (Token chanName) = atomically $ do
  chan <- StmMap.lookup chanName writableChannels
  case chan of
    Nothing -> return () -- consider notifying the user their channel was already deleted somehow
    Just (chan, _) -> do
      writeTChan chan Closing
      StmMap.delete chanName writableChannels

getChannel :: String -> IO ChannelResult
getChannel name = atomically $ do
  writableChannel <- StmMap.lookup name writableChannels
  case writableChannel of
    Just (existing, initial) -> do
      channel <- ReadChannel <$> dupTChan existing
      return $ Existing (channel, initial)
    Nothing -> do
      newWritableChan <- newBroadcastTChan
      StmMap.insert (newWritableChan, blankStreamerInformation) name writableChannels
      return $ NewBroadcast (WriteChannel (newWritableChan, name), Token name)
