module Twitch.Types where

import Data.Aeson
import Data.Text
import GHC.Generics

type AppAccessToken = String

data PubSubMessage = PubSubMessage
  { target :: [PubSubTarget],
    broadcaster_id :: Text,
    is_global_broadcast :: Bool,
    message :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON PubSubMessage

data ChannelId
  = Channel Text
  | All
  deriving (Eq, Show)

instance ToJSON ChannelId where
  toJSON All = String "all"
  toJSON (Channel s) = String s

instance FromJSON ChannelId where
  parseJSON (String "all") = return All
  parseJSON (String s) = return $ Channel s
  parseJSON _ = fail "channel_id value is not a string"

data PubSubTarget
  = Broadcast
  | Global
  | Whisper Text
  deriving (Eq, Show, Generic)

instance ToJSON PubSubTarget where
  toJSON Broadcast = "broadcast"
  toJSON Global = "global"
  toJSON (Whisper t) = String t

instance FromJSON PubSubTarget where
  parseJSON (String "broadcast") = return Broadcast
  parseJSON (String "global") = return Global
  parseJSON (String s) = return $ Whisper s
  parseJSON _ = fail "pubsub permission is not a string"

newtype PubSubPerms = PubSubPerms
  { send :: [PubSubTarget]
  }
  deriving (Eq, Show, Generic)

instance ToJSON PubSubPerms

instance FromJSON PubSubPerms
