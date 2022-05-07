{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Twitch.Api
  ( sendPubSubMessage,
    getChannelInformation,
    getActiveStreamById,
    getActiveStreamByName,
    TwitchResponse (..),
    StreamInformation (..),
    ChannelInformation(..),
  )
where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:), (.:?))
import GHC.Generics (Generic)
import Servant
  ( Get,
    JSON,
    NoContent,
    PostNoContent,
    Proxy (..),
    QueryParam',
    ReqBody,
    Required,
    Strict,
    type (:<|>) ((:<|>)),
    type (:>),
  )
import Servant.Client (client)
import Twitch.Auth
  ( TwitchJwt (..),
    WithTwitchClientAppToken,
    WithTwitchClientJwt,
    runWithTwitchAppTokenAuth,
    runWithTwitchJwtAuth,
  )
import Twitch.Types (PubSubMessage)

newtype PaginationInfo = PaginationInfo {cursor :: String}
  deriving (Eq, Show, Generic)

instance FromJSON PaginationInfo

data TwitchResponse a = TwitchResponse
  { _data :: [a],
    pagination :: Maybe PaginationInfo
  }
  deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (TwitchResponse a) where
  parseJSON (Object v) =
    TwitchResponse
      <$> v .: "data"
      <*> (v .:? "pagination" <|> pure Nothing)
  parseJSON _ = mzero

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

data StreamInformation = StreamInformation
  { user_id :: String,
    user_login :: String,
    user_name :: String,
    game_id :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON StreamInformation

type SendPubSubMessage =
  "helix" :> "extensions" :> "pubsub" :> WithTwitchClientJwt (ReqBody '[JSON] PubSubMessage :> PostNoContent)

type GetChannelInformation =
  "helix" :> "channels" :> WithTwitchClientAppToken (QueryParam' '[Required, Strict] "broadcaster_id" String :> Get '[JSON] (TwitchResponse ChannelInformation))

type GetActiveStreamById =
  "helix" :> "streams" :> WithTwitchClientAppToken (QueryParam' '[Required, Strict] "user_id" String :> Get '[JSON] (TwitchResponse StreamInformation))

type GetActiveStreamByName =
  "helix" :> "streams" :> WithTwitchClientAppToken (QueryParam' '[Required, Strict] "user_login" String :> Get '[JSON] (TwitchResponse StreamInformation))

type TwitchApi =
  SendPubSubMessage
    :<|> GetChannelInformation
    :<|> GetActiveStreamById
    :<|> GetActiveStreamByName

twitchApi :: Proxy TwitchApi
twitchApi = Proxy

sendPubSubMessage :: TwitchJwt -> PubSubMessage -> IO (Either String NoContent)
getChannelInformation :: String -> IO (Either String (TwitchResponse ChannelInformation))
getActiveStreamById :: String -> IO (Either String (TwitchResponse StreamInformation))
getActiveStreamByName :: String -> IO (Either String (TwitchResponse StreamInformation))
( sendPubSubMessage,
  getChannelInformation,
  getActiveStreamById,
  getActiveStreamByName
  ) =
    ( runWithTwitchJwtAuth sendPubSubMessage',
      runWithTwitchAppTokenAuth getChannelInformation',
      runWithTwitchAppTokenAuth getActiveStreamById',
      runWithTwitchAppTokenAuth getActiveStreamByName'
    )
    where
      sendPubSubMessage'
        :<|> getChannelInformation'
        :<|> getActiveStreamById'
        :<|> getActiveStreamByName' =
          client twitchApi
