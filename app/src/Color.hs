module Color (getColor, setColor) where

import Control.Concurrent.STM.TVar
import qualified System.IO.Unsafe as System.IO
import qualified Control.Concurrent.STM as Control.Monad.STM

getColor :: IO Int
getColor = readTVarIO colorHandle

setColor :: Int -> IO ()
setColor = Control.Monad.STM.atomically . writeTVar colorHandle

colorHandle :: TVar Int
colorHandle = System.IO.unsafePerformIO $ newTVarIO 0
