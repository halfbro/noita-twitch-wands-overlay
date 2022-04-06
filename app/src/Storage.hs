module Storage
  ( registerStreamerBroadcastChannel,
    lookupStreamerBroadcastChannel,
  )
where

import Control.Concurrent.STM (STM, TChan, TVar, atomically, newBroadcastTChan, newTVar, retry, throwSTM)
import Control.Exception (Exception)
import GHC.IO (unsafePerformIO)
import StmContainers.Map as StmMap
import Types (Inventory, StreamerInformation, Wand)

type StreamerDataMap = StmMap.Map String (TChan StreamerInformation, TVar Bool)

data StreamerChannelException = FailedLookup
  deriving (Show)

instance Exception StreamerChannelException

{-# NOINLINE streamerMap #-}
streamerMap :: StreamerDataMap
streamerMap = unsafePerformIO StmMap.newIO

registerStreamerBroadcastChannel :: String -> STM (TChan StreamerInformation, TVar Bool)
registerStreamerBroadcastChannel streamerName = do
  newStream <- newBroadcastTChan
  cancelToken <- newTVar False
  StmMap.insert (newStream, cancelToken) streamerName streamerMap
  return (newStream, cancelToken)

lookupStreamerBroadcastChannel :: String -> STM (Maybe (TChan StreamerInformation))
lookupStreamerBroadcastChannel streamerName = do
  res <- StmMap.lookup streamerName streamerMap
  return $ fmap fst res
