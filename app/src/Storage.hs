module Storage (insertStreamerData) where

import Control.Concurrent.STM (atomically)
import GHC.IO (unsafePerformIO)
import StmContainers.Map as StmMap
import Types (Inventory, Wand)

type StreamerDataMap = StmMap.Map String ([Wand], Inventory)

{-# NOINLINE streamerMap #-}
streamerMap :: StreamerDataMap
streamerMap = unsafePerformIO StmMap.newIO

insertStreamerData :: String -> [Wand] -> Inventory -> IO ()
insertStreamerData streamerName wands inventory =
  atomically $
    StmMap.insert (wands, inventory) streamerName streamerMap
