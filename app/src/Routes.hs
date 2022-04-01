{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Routes where

import qualified Color
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens.Getter
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Crypto.JWT
import qualified Crypto.JWT as JWT
import Data.Aeson
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Text
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Network.HTTP.Types (hAccept, hAcceptLanguage, hAuthorization, hContentType)
import Network.HTTP.Types.Header (hContentLanguage)
import Network.HTTP.Types.Method
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Servant.API
import Servant.Auth
import Servant.Auth.Server
import qualified Servant.Auth.Server as AuthResult
import qualified System.IO.Unsafe as System.IO
import qualified System.Environment

toBytestring :: [Char] -> L.ByteString
toBytestring = L.fromStrict . encodeUtf8 . T.pack

_secret = System.IO.unsafePerformIO $ System.Environment.getEnv "TWITCH_JWT_SECRET"
secret = JWT.fromOctets . B64.decodeLenient . toBytestring $ _secret

data TwitchJwt = TwitchJwt
  { channel_id :: [Char],
    expires :: NumericDate,
    role :: Text,
    opaque_user_id :: Text
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
            Nothing -> Left $ Data.Text.concat ["No claim '", s, "' exists"]
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

newtype ColorInformation = ColorInformation
  { color :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON ColorInformation

instance ToJSON ColorInformation

type TwitchAuth = Auth '[JWT] TwitchJwt

type RawApi =
  "color" :> Get '[JSON] ColorInformation
    :<|> "color" :> ReqBody '[JSON] ColorInformation :> Post '[JSON] ColorInformation

getColor :: TwitchJwt -> Handler ColorInformation
getColor _jwt = do
  liftIO $ putStrLn "GET received"
  newColor <- liftIO Color.getColor
  return ColorInformation {color = newColor}

postColor :: TwitchJwt -> ColorInformation -> Handler ColorInformation
postColor _jwt ColorInformation {color = color} = do
  liftIO $ putStrLn "POST received"
  liftIO $ Color.setColor color
  newColor <- liftIO Color.getColor
  return ColorInformation {color = newColor}

type ColorApi = TwitchAuth :> RawApi

server :: Server ColorApi
server (Authenticated jwt) = getColor jwt :<|> postColor jwt
server _ = throwAll err401

colorApi :: Proxy ColorApi
colorApi = Proxy

runApi :: IO ()
runApi = do
  let corsConfig =
        const $
          Just $
            simpleCorsResourcePolicy
              { corsMethods = [methodOptions, methodGet, methodPost],
                corsRequestHeaders = [hAuthorization, hContentType, hAccept, hAcceptLanguage, hContentLanguage]
              }
  let app = serveWithContext colorApi (defaultCookieSettings :. defaultJWTSettings secret :. EmptyContext) server
  let withCors = cors corsConfig
  run 7999 (withCors app)
