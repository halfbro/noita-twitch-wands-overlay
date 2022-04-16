module Twitch.Secrets where

import qualified System.Environment
import qualified System.IO.Unsafe as System.IO

twitchJwtSecret = System.IO.unsafePerformIO $ System.Environment.getEnv "TWITCH_JWT_SECRET"

twitchClientId = System.IO.unsafePerformIO $ System.Environment.getEnv "TWITCH_API_CLIENT_ID"

twitchClientSecret = System.IO.unsafePerformIO $ System.Environment.getEnv "TWITCH_API_CLIENT_SECRET"
