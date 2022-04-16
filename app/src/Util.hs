module Util (sleepSeconds, roundUTCTime, testDate) where

import Control.Concurrent (threadDelay)
import Crypto.JWT (NumericDate (..))
import Data.Time (NominalDiffTime, TimeOfDay (TimeOfDay), UTCTime (UTCTime), addUTCTime, fromGregorian, getCurrentTime, timeOfDayToTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

sleepSeconds :: Int -> IO ()
sleepSeconds secs =
  threadDelay $ secs * 1_000_000

roundUTCTime :: UTCTime -> UTCTime
roundUTCTime = posixSecondsToUTCTime . fromIntegral . truncate . utcTimeToPOSIXSeconds

testDate :: NumericDate
testDate = NumericDate (UTCTime (fromGregorian 2 2 2) $ timeOfDayToTime (TimeOfDay 0 0 0))
