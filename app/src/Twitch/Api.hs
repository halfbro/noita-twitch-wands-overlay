{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Twitch.Api where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Servant (Get, JSON, NoContent, PostNoContent, Proxy (..), QueryParam', ReqBody, Required, Strict, type (:<|>) ((:<|>)), type (:>))
import Servant.Auth.Server (JWTSettings)
import Servant.Client (client)
import Twitch.Auth
  ( TwitchJwt (..),
    WithTwitchClientJwt,
    runWithTwitchAuth,
  )
import Twitch.Types
import Util (testDate)

data ChannelInformation = ChannelInformation
  { broadcaster_id :: String,
    broadcaster_login :: String,
    broadcaster_name :: String,
    game_name :: String,
    game_id :: String,
    title :: String,
    delay :: Integer
  }
  deriving (Eq, Show, Generic)

instance FromJSON ChannelInformation

type SendPubSubMessage =
  "helix" :> "extensions" :> "pubsub" :> WithTwitchClientJwt (ReqBody '[JSON] PubSubMessage :> PostNoContent)

type GetChannelInformation =
  "helix" :> "channels" :> WithTwitchClientJwt (QueryParam' '[Required, Strict] "broadcaster_id" String :> Get '[JSON] ChannelInformation)

type TwitchApi = SendPubSubMessage :<|> GetChannelInformation

twitchApi :: Proxy TwitchApi
twitchApi = Proxy

sendPubSubMessage :: TwitchJwt -> PubSubMessage -> IO (Either String NoContent)
getChannelInformation :: TwitchJwt -> String -> IO (Either String ChannelInformation)
sendPubSubMessage :<|> getChannelInformation =
  runWithTwitchAuth sendPubSubMessage'
    :<|> runWithTwitchAuth getChannelInformation'
  where
    sendPubSubMessage'
      :<|> getChannelInformation' = client twitchApi

_testJwt =
  TwitchJwt
    { exp = testDate,
      user_id = "21194124", -- owner of extension
      opaque_user_id = "21194124",
      role = "external",
      channel_id = Just $ Channel "21194124", -- target channel
      pubsub_perms =
        PubSubPerms
          { send = [Broadcast]
          }
    }

_testData =
  PubSubMessage
    { target = [Broadcast],
      broadcaster_id = "21194124", -- target channel
      is_global_broadcast = False,
      message = "test broadcast"
    }
