{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Routes where

import Control.Monad.Cont (liftIO)
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
import qualified OnlyWands
import Servant
  ( Context (EmptyContext, (:.)),
    Get,
    Handler,
    JSON,
    Proxy (..),
    Server,
    err401,
    serveWithContext,
    type (:>),
  )
import Servant.API (Capture)
import Servant.Auth (Auth, JWT)
import Servant.Auth.Server
  ( AuthResult (Authenticated),
    ThrowAll (throwAll),
    defaultCookieSettings,
  )
import Twitch.Auth
import Types (StreamerInformation)

type RawApi =
  "wand_info" :> Capture "twitchId" String :> Get '[JSON] StreamerInformation

getInfo :: TwitchJwt -> String -> Handler StreamerInformation
getInfo jwt channelId = do
  liftIO $ OnlyWands.getWandInfoForStreamer channelId

type TwitchAuth = Auth '[JWT] TwitchJwt

type AppApi = TwitchAuth :> RawApi

server :: Server AppApi
server (Authenticated jwt) = getInfo jwt
server _ = throwAll err401

api :: Proxy AppApi
api = Proxy

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
  let app = serveWithContext api ctx server
  let withCors = cors corsConfig
  run 7999 (withCors app)
