module Util where

import Control.Concurrent (threadDelay)
import Crypto.JWT (NumericDate (..))
import Data.Time (NominalDiffTime, TimeOfDay (TimeOfDay), UTCTime (UTCTime), addUTCTime, fromGregorian, getCurrentTime, timeOfDayToTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

toBytestring :: String -> S.ByteString
toBytestring = encodeUtf8 . T.pack

toLazyBytestring :: String -> L.ByteString
toLazyBytestring = L.fromStrict . encodeUtf8 . T.pack

sleepSeconds :: Int -> IO ()
sleepSeconds secs =
  threadDelay $ secs * 1_000_000

roundUTCTime :: UTCTime -> UTCTime
roundUTCTime = posixSecondsToUTCTime . fromIntegral . truncate . utcTimeToPOSIXSeconds

testDate :: NumericDate
testDate = NumericDate (UTCTime (fromGregorian 2 2 2) $ timeOfDayToTime (TimeOfDay 0 0 0))
