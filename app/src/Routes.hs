{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Routes where

import Control.Lens.Getter ()
import Crypto.JOSE (fromOctets)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics ()
import Network.HTTP.Types (hAccept, hAcceptLanguage, hAuthorization, hContentType)
import Network.HTTP.Types.Header (hContentLanguage)
import Network.HTTP.Types.Method
  ( methodGet,
    methodOptions,
    methodPost,
  )
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (corsMethods, corsRequestHeaders),
    cors,
    simpleCorsResourcePolicy,
  )
import Servant
  ( Context (EmptyContext, (:.)),
    Get,
    Handler,
    JSON,
    Post,
    Proxy (..),
    ReqBody,
    Server,
    err401,
    serveWithContext,
    type (:<|>) (..),
    type (:>),
  )
import Servant.API ()
import Servant.Auth (Auth, JWT)
import Servant.Auth.Server
  ( AuthResult (Authenticated),
    ThrowAll (throwAll),
    defaultCookieSettings,
    defaultJWTSettings,
  )
import qualified Servant.Auth.Server as AuthResult
import qualified System.Environment
import qualified System.IO.Unsafe as System.IO
import Twitch (TwitchJwt)

toBytestring :: [Char] -> L.ByteString
toBytestring = L.fromStrict . encodeUtf8 . T.pack

_secret = System.IO.unsafePerformIO $ System.Environment.getEnv "TWITCH_JWT_SECRET"

secret = fromOctets . B64.decodeLenient . toBytestring $ _secret

type TwitchAuth = Auth '[JWT] TwitchJwt

type RawApi =
  "color" :> Get '[JSON] Integer
    :<|> "color" :> ReqBody '[JSON] Integer :> Post '[JSON] Integer

getColor :: TwitchJwt -> Handler Integer
getColor _jwt = do
  return 69

postColor :: TwitchJwt -> Integer -> Handler Integer
postColor _jwt color = do
  return 42

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
  let ctx = defaultCookieSettings :. defaultJWTSettings secret :. EmptyContext
  let app = serveWithContext colorApi ctx server
  let withCors = cors corsConfig
  run 7999 (withCors app)
