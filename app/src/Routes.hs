{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Routes where

import Control.Lens.Getter ()
import qualified Data.Map as M
import qualified Data.Text as T
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
import Twitch

{-
type RawApi =
  "color" :> Get '[JSON] Integer
    :<|> "color" :> ReqBody '[JSON] Integer :> Post '[JSON] Integer

getColor :: TwitchJwt -> Handler Integer
getColor _jwt = do
  return 69

postColor :: TwitchJwt -> Integer -> Handler Integer
postColor _jwt color = do
  return 42

type TwitchAuth = Auth '[JWT] TwitchJwt

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
  let ctx = defaultCookieSettings :. twitchJwtSettings :. EmptyContext
  let app = serveWithContext colorApi ctx server
  let withCors = cors corsConfig
  run 7999 (withCors app)
-}
