{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Twitch.Auth where

import Control.Exception (Exception (toException), SomeException (SomeException), throw)
import Control.Lens
import Control.Lens.Getter (view)
import Control.Monad.Except (runExceptT)
import Crypto.JOSE
import Crypto.JOSE.Error
import Crypto.JWT (ClaimsSet, NumericDate (NumericDate), SignedJWT, addClaim, claimExp, emptyClaimsSet, signClaims, unregisteredClaims)
import Data.Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString as S
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy as L
import Data.Either (fromRight)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (NominalDiffTime, TimeOfDay (TimeOfDay), UTCTime (UTCTime), addUTCTime, fromGregorian, getCurrentTime, timeOfDayToTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import GHC.Exception (errorCallException)
import GHC.Generics (Generic)
import GHC.TopHandler (runIO)
import Network.HTTP.Client (domainMatches, managerModifyRequest, newManager, requestHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.WebSockets as S
import Servant
import Servant.API.ContentTypes (JSON, OctetStream)
import Servant.Auth
import Servant.Auth.Client
import Servant.Auth.JWT (FromJWT (decodeJWT), ToJWT)
import Servant.Client (ClientEnv, ClientError (ConnectionError), ClientM, client, mkClientEnv, parseBaseUrl, runClientM)
import Servant.Client.Streaming (ClientError)
import qualified System.Environment
import qualified System.IO.Unsafe as System.IO
import Prelude hiding (exp)
import Control.Monad (void)

toBytestring :: String -> L.ByteString
toBytestring = L.fromStrict . encodeUtf8 . T.pack

_secret = System.IO.unsafePerformIO $ System.Environment.getEnv "TWITCH_JWT_SECRET"

_clientId = System.IO.unsafePerformIO $ System.Environment.getEnv "TWITCH_API_CLIENT_ID"

twitchJwtSecret = fromOctets . B64.decodeLenient . toBytestring $ _secret

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

data TwitchJwt = TwitchJwt
  { exp :: NumericDate,
    user_id :: Text,
    opaque_user_id :: Text,
    role :: Text,
    channel_id :: Maybe ChannelId,
    pubsub_perms :: PubSubPerms
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
          channel_id <-
            case HM.lookup "channel_id" cs of
              Nothing -> Right Nothing
              Just v ->
                case fromJSON v of
                  Success val -> Right val
                  _ -> Left "Couldn't parse channel_id claim"

          expires <-
            case view claimExp claims of
              Just exp -> Right exp
              _ -> Left "Couldn't get expiry"

          role <- getClaim "role"

          opaque_user_id <- getClaim "opaque_user_id"
          user_id <- getClaim "user_id"
          pubsub_perms <- getClaim "pubsub_perms"

          return $
            TwitchJwt
              { exp = expires,
                user_id = user_id,
                opaque_user_id = opaque_user_id,
                role = role,
                channel_id = channel_id,
                pubsub_perms = pubsub_perms
              }

type WithTwitchClientSecurity a = Auth '[Bearer] Token :> Header "Client-Id" String :> a

data PubSubMessageData = PubSubMessageData
  { target :: [PubSubTarget],
    broadcaster_id :: Text,
    is_global_broadcast :: Bool,
    message :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON PubSubMessageData

type SendPubSubMessage =
  "helix" :> "extensions" :> "pubsub" :> WithTwitchClientSecurity (ReqBody '[JSON] PubSubMessageData :> PostNoContent)

twitchApi :: Proxy SendPubSubMessage
twitchApi = Proxy

twitchEnv :: ClientEnv
twitchEnv =
  mkClientEnv manager url
  where
    manager =
      System.IO.unsafePerformIO $ newManager $ tlsManagerSettings {managerModifyRequest = \req -> do print (requestHeaders req); return req}
    url = System.IO.unsafePerformIO $ parseBaseUrl "https://api.twitch.tv"

sendPubSubMessage' :: Token -> Maybe String -> PubSubMessageData -> ClientM NoContent
sendPubSubMessage' = client twitchApi

testJwt =
  TwitchJwt
    { exp = NumericDate (UTCTime (fromGregorian 2 2 2) $ timeOfDayToTime (TimeOfDay 0 0 0)),
      user_id = "21194124",
      opaque_user_id = "21194124",
      role = "external",
      channel_id = Just $ Channel "21194124",
      pubsub_perms =
        PubSubPerms
          { send = [Broadcast]
          }
    }

testData =
  PubSubMessageData
    { target = [Broadcast],
      broadcaster_id = "21194124",
      is_global_broadcast = False,
      message = "test broadcast"
    }

roundUTCTime :: UTCTime -> UTCTime
roundUTCTime = posixSecondsToUTCTime . fromIntegral . truncate . utcTimeToPOSIXSeconds

mkClaims :: TwitchJwt -> IO ClaimsSet
mkClaims twitchJwt = do
  exp <- roundUTCTime . addUTCTime 30 <$> getCurrentTime
  let claimsBase = emptyClaimsSet & claimExp ?~ NumericDate exp
  pure $
    claimsBase
      & addClaim "user_id" (toJSON $ user_id twitchJwt)
      & addClaim "role" "external"
      & addClaim "channel_id" (toJSON $ channel_id twitchJwt)
      & addClaim "pubsub_perms" (toJSON $ pubsub_perms twitchJwt)

--enocdeTwitchJwt :: L.ByteString -> IO (Either Error SignedJWT)
encodeTwitchJwt :: TwitchJwt -> IO (Either Error SignedJWT)
encodeTwitchJwt jwt = do
  claims <- mkClaims jwt
  runExceptT $ makeJWSHeader twitchJwtSecret >>= \h -> signClaims twitchJwtSecret h claims

sendPubSubMessage :: TwitchJwt -> PubSubMessageData -> IO (Either String ())
sendPubSubMessage twitchJwt d = do
  jwt <- first show <$> encodeTwitchJwt twitchJwt
  case jwt of
    Left e -> return $ Left e
    Right signed -> do
      let token = Token . S.fromLazyByteString . encodeCompact $ signed
      res <- runClientM (sendPubSubMessage' token (Just _clientId) d) twitchEnv
      return $ void $ first show res
