module Channel
  ( broadcastToChannel,
    getAllWritableChannelUsernames,
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
import Control.Monad.List (ListT (runListT))
import Data.Default (Default (def))
import GHC.IO (unsafePerformIO)
import ListT (toList)
import StmContainers.Map as StmMap (Map, delete, insert, listT, lookup, newIO)

data ChannelMessage a
  = Data a
  | Closing

data WriteChannel a = WriteChannel (TChan (ChannelMessage a)) String

newtype ReadChannel a = ReadChannel (TChan (ChannelMessage a))

newtype StopToken = Token String

type StreamerDataMap a = StmMap.Map String (TChan (ChannelMessage a), a)

data ChannelResult a
  = Existing a
  | NewBroadcast (WriteChannel a) StopToken

{-# NOINLINE writableChannels #-}
writableChannels :: StreamerDataMap a
writableChannels = unsafePerformIO StmMap.newIO

getAllWritableChannelUsernames :: IO [String]
getAllWritableChannelUsernames = do
  let usernamesStream = fst <$> StmMap.listT writableChannels
  atomically $ toList usernamesStream

broadcastToChannel :: WriteChannel a -> a -> IO ()
broadcastToChannel (WriteChannel chan name) info =
  atomically $ do
    StmMap.insert (chan, info) name writableChannels
    writeTChan chan $ Data info

stopChannel :: StopToken -> IO ()
stopChannel (Token chanName) = atomically $ do
  chan <- StmMap.lookup chanName writableChannels
  case chan of
    Nothing -> return () -- consider notifying the user their channel was already deleted somehow
    Just (chan, _) -> do
      writeTChan chan Closing
      StmMap.delete chanName writableChannels

makeReadChannel :: WriteChannel a -> IO (ReadChannel a)
makeReadChannel (WriteChannel chan _) = atomically $ ReadChannel <$> dupTChan chan

streamFromChannel :: ReadChannel a -> (a -> IO ()) -> IO ()
streamFromChannel (ReadChannel chan) f = do
  let loop = do
        msg <- atomically $ readTChan chan
        case msg of
          Closing -> return ()
          Data d -> do
            f d
            loop
  loop

getChannel :: Default a => String -> IO (ChannelResult a)
getChannel name = atomically $ do
  writableChannel <- StmMap.lookup name writableChannels
  case writableChannel of
    Just (existing, initial) -> do
      return $ Existing initial
    Nothing -> do
      newWritableChan <- newBroadcastTChan
      StmMap.insert (newWritableChan, def) name writableChannels
      return $ NewBroadcast (WriteChannel newWritableChan name) (Token name)
