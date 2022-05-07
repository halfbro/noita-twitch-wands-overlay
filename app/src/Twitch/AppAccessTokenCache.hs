module Twitch.AppAccessTokenCache (AppAccessToken, getAppAccessToken) where

import Data.Aeson (FromJSON)
import Data.Cache ( insert', lookup, newCache, Cache )
import Data.Int (Int64)
import GHC.Generics (Generic)
import Network.HTTP.Simple
    ( parseRequest,
      addToRequestQueryString,
      getResponseBody,
      httpJSON,
      setRequestMethod )
import System.Clock (TimeSpec (TimeSpec))
import System.IO.Unsafe (unsafePerformIO)
import Twitch.Secrets (twitchClientId, twitchClientSecret)
import Twitch.Types (AppAccessToken)
import Util (toBytestring)

{-# NOINLINE cache #-}
cache :: Cache () AppAccessToken
cache = unsafePerformIO $ newCache (Just $ 4_500_000 * 10 ^ 9)

data TokenResponse = TokenResponse
  { access_token :: String,
    expires_in :: Int64,
    token_type :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON TokenResponse

fetchNewAppAccessToken :: IO (AppAccessToken, Int64)
fetchNewAppAccessToken = do
  req <-
    setRequestMethod "POST"
      . addToRequestQueryString [("client_id", Just $ toBytestring twitchClientId)]
      . addToRequestQueryString [("client_secret", Just $ toBytestring twitchClientSecret)]
      . addToRequestQueryString [("grant_type", Just "client_credentials")]
      <$> parseRequest "https://id.twitch.tv/oauth2/token"
  res <- getResponseBody <$> httpJSON req :: IO TokenResponse
  return (access_token res, expires_in res)

getAppAccessToken :: IO AppAccessToken
getAppAccessToken = do
  v <- Data.Cache.lookup cache ()
  case v of
    Just token -> return token
    _ -> do
      (newToken, expiry) <- fetchNewAppAccessToken
      insert' cache (Just $ TimeSpec expiry 0) () newToken
      return newToken
