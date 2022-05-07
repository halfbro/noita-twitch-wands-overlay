{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Twitch.Auth where

import Control.Lens (view, (&), (?~))
import Control.Monad.Except (runExceptT)
import Crypto.JOSE
  ( Error,
    encodeCompact,
    fromOctets,
    makeJWSHeader,
  )
import Crypto.JWT
  ( ClaimsSet,
    NumericDate (NumericDate),
    SignedJWT,
    addClaim,
    claimExp,
    emptyClaimsSet,
    signClaims,
    unregisteredClaims,
  )
import Data.Aeson
  ( FromJSON,
    Result (Success),
    ToJSON (toJSON),
    fromJSON,
  )
import Data.Bifunctor (first)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant
  ( Header,
    JSON,
    NoContent,
    Proxy (..),
    ReqBody,
    type (:>),
  )
import Servant.Auth.Client (Bearer, Token (Token))
import Servant.Auth.Server
import Servant.Client (ClientEnv, ClientM, mkClientEnv, parseBaseUrl, runClientM)
import Twitch.AppAccessTokenCache (getAppAccessToken)
import Twitch.Secrets (twitchClientId, twitchJwtSecret)
import Twitch.Types (ChannelId (Channel), PubSubPerms (..), PubSubTarget (Broadcast))
import Util (nowRounded, roundUTCTime, toBytestring, toLazyBytestring)

twitchJwk = fromOctets . B64.decodeLenient . toLazyBytestring $ twitchJwtSecret

data TwitchJwt = TwitchJwt
  { exp :: NumericDate,
    user_id :: T.Text,
    opaque_user_id :: T.Text,
    role :: T.Text,
    channel_id :: Maybe ChannelId,
    pubsub_perms :: PubSubPerms
  }
  deriving (Eq, Show, Generic)

instance ToJSON TwitchJwt

instance FromJSON TwitchJwt

makeTwitchJwt :: String -> IO TwitchJwt
makeTwitchJwt channelId = do
  expiryDate <- addUTCTime 10 <$> nowRounded
  return
    TwitchJwt
      { exp = NumericDate expiryDate,
        user_id = "21194124", -- owner of extension
        opaque_user_id = "21194124",
        role = "external",
        channel_id = Just $ Channel $ T.pack channelId, -- target channel
        pubsub_perms =
          PubSubPerms
            { send = [Broadcast],
              listen = []
            }
      }

instance ToJWT TwitchJwt

instance FromJWT TwitchJwt where
  decodeJWT claims =
    let cs = view unregisteredClaims claims
        getClaim :: FromJSON a => T.Text -> Either T.Text a
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
          pubsub_perms <- getClaim "pubsub_perms"

          return $
            TwitchJwt
              { exp = expires,
                user_id = "21194124",
                opaque_user_id = opaque_user_id,
                role = role,
                channel_id = channel_id,
                pubsub_perms = pubsub_perms
              }

type WithTwitchClientJwt a = Auth '[Bearer] Token :> Header "Client-Id" String :> a

type WithTwitchClientAppToken a = Auth '[Bearer] Token :> Header "Client-Id" String :> a

twitchApiEnv :: IO ClientEnv
twitchApiEnv =
  mkClientEnv <$> manager <*> url
  where
    manager = newManager tlsManagerSettings
    url = parseBaseUrl "https://api.twitch.tv"

mkClaims :: TwitchJwt -> IO ClaimsSet
mkClaims twitchJwt = do
  exp <- roundUTCTime . addUTCTime 30 <$> getCurrentTime
  pure $
    emptyClaimsSet
      & claimExp ?~ NumericDate exp
      & addClaim "user_id" (toJSON $ user_id twitchJwt)
      & addClaim "role" "external"
      & addClaim "channel_id" (toJSON $ channel_id twitchJwt)
      & addClaim "pubsub_perms" (toJSON $ pubsub_perms twitchJwt)

twitchJwtSettings :: JWTSettings
twitchJwtSettings = defaultJWTSettings twitchJwk

encodeTwitchJwt :: TwitchJwt -> IO SignedJWT
encodeTwitchJwt jwt = do
  claims <- mkClaims jwt
  maybeSigned <- runExceptT $ makeJWSHeader twitchJwk >>= \h -> signClaims twitchJwk h claims
  case maybeSigned of
    Left (_ :: Error) -> fail "Error signing JWT somehow???"
    Right signed -> return signed

runWithTwitchAppTokenAuth :: (Token -> Maybe String -> a -> ClientM b) -> a -> IO (Either String b)
runWithTwitchAppTokenAuth f args = do
  token <- Token . toBytestring <$> getAppAccessToken
  res <- runClientM (f token (Just twitchClientId) args) =<< twitchApiEnv
  return $ first show res

runWithTwitchJwtAuth :: (Token -> Maybe String -> a -> ClientM b) -> TwitchJwt -> a -> IO (Either String b)
runWithTwitchJwtAuth f jwt args = do
  token <- Token . BL.toStrict . encodeCompact <$> encodeTwitchJwt jwt
  res <- runClientM (f token (Just twitchClientId) args) =<< twitchApiEnv
  return $ first show res
