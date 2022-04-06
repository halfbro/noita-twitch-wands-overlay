{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Twitch (TwitchJwt, streamerStoppedStreaming) where

import Control.Lens.Getter (view)
import Crypto.JWT
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.Auth.JWT (FromJWT (decodeJWT), ToJWT)

data TwitchJwt = TwitchJwt
  { channel_id :: [Char],
    expires :: NumericDate,
    role :: String,
    opaque_user_id :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON TwitchJwt

instance FromJSON TwitchJwt

instance ToJWT TwitchJwt

instance FromJWT TwitchJwt where
  decodeJWT claims =
    let cs = view unregisteredClaims claims
        getClaim :: FromJSON a => Text -> Either Text a
        getClaim s =
          case HM.lookup s cs of
            Nothing -> Left $ "No claim '" <> s <> "' exists"
            Just v ->
              case fromJSON v of
                Success val -> Right val
                _ -> Left "Couldn't parse claim"
     in do
          channel_id <- getClaim "channel_id"
          expires <-
            case view claimExp claims of
              Just exp -> Right exp
              _ -> Left "Couldn't get expiry"
          role <- getClaim "role"
          opaque_user_id <- getClaim "opaque_user_id"

          return $
            TwitchJwt
              { channel_id = channel_id,
                expires = expires,
                role = role,
                opaque_user_id = opaque_user_id
              }

streamerStoppedStreaming :: String -> IO Bool
streamerStoppedStreaming streamerName =
  -- TODO
  return False
