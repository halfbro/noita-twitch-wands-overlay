{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Twitch.Auth where

import Control.Exception (Exception (toException), SomeException (SomeException), throw)
import Control.Lens (view, (&), (?~))
import Control.Lens.Getter (view)
import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Crypto.JOSE
  ( Error,
    encodeCompact,
    fromOctets,
    makeJWSHeader,
  )
import Crypto.JOSE.Error ()
import Crypto.JWT (ClaimsSet, NumericDate (NumericDate), SignedJWT, addClaim, claimExp, emptyClaimsSet, signClaims, unregisteredClaims)
import Data.Aeson
  ( FromJSON (parseJSON),
    Result (Success),
    ToJSON (toJSON),
    Value (String),
    fromJSON,
  )
import Data.Bifunctor (first)
import qualified Data.ByteString as S
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy as L
import Data.Either (fromRight)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (addUTCTime, getCurrentTime)
import GHC.Exception (errorCallException)
import GHC.Generics (Generic)
import GHC.TopHandler (runIO)
import Network.HTTP.Client (domainMatches, managerModifyRequest, newManager, requestHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.WebSockets as WS
import Servant
  ( Header,
    JSON,
    NoContent,
    PostNoContent,
    Proxy (..),
    ReqBody,
    type (:>),
  )
import Servant.API.ContentTypes (JSON, OctetStream)
import Servant.Auth (Auth)
import Servant.Auth.Client (Bearer, Token (Token))
import Servant.Auth.JWT (FromJWT (decodeJWT), ToJWT)
import Servant.Auth.Server
import Servant.Client (ClientEnv, ClientError (ConnectionError), ClientM, client, mkClientEnv, parseBaseUrl, runClientM)
import Servant.Client.Streaming (ClientError)
import qualified System.Environment
import qualified System.IO.Unsafe as System.IO
import Twitch.Types
import Util (roundUTCTime)
import Prelude hiding (exp)

toBytestring :: String -> L.ByteString
toBytestring = L.fromStrict . encodeUtf8 . T.pack

_secret = System.IO.unsafePerformIO $ System.Environment.getEnv "TWITCH_JWT_SECRET"

_clientId = System.IO.unsafePerformIO $ System.Environment.getEnv "TWITCH_API_CLIENT_ID"

twitchJwtSecret = fromOctets . B64.decodeLenient . toBytestring $ _secret

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

{-
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
-}

type WithTwitchClientJwt a = Auth '[Bearer] Token :> Header "Client-Id" String :> a

type WithTwitchClientAppToken a = Auth '[Bearer] Token :> Header "Client-Id" String :> a

twitchEnv :: IO ClientEnv
twitchEnv =
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
twitchJwtSettings = defaultJWTSettings twitchJwtSecret

encodeTwitchJwt :: TwitchJwt -> IO SignedJWT
encodeTwitchJwt jwt = do
  claims <- mkClaims jwt
  maybeSigned <- runExceptT $ makeJWSHeader twitchJwtSecret >>= \h -> signClaims twitchJwtSecret h claims
  case maybeSigned of
    Left (_ :: Error) -> fail "Error signing JWT somehow???"
    Right signed -> return signed

runWithTwitchAuth :: (Token -> Maybe String -> a -> ClientM b) -> TwitchJwt -> a -> IO (Either String b)
runWithTwitchAuth f jwt args = do
  token <- Token . WS.fromLazyByteString . encodeCompact <$> encodeTwitchJwt jwt
  res <- runClientM (f token (Just _clientId) args) =<< twitchEnv
  return $ first show res
