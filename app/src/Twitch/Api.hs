{-# LANGUAGE DataKinds #-}
module Twitch.Api (TwitchAuth, twitchJwtSettings) where

import Twitch.Auth
import Servant.Auth
import Servant.Auth.Server (defaultJWTSettings)
import Servant.Auth.Server.Internal.ConfigTypes (JWTSettings)

type TwitchAuth = Auth '[JWT] TwitchJwt

twitchJwtSettings :: JWTSettings
twitchJwtSettings = defaultJWTSettings twitchJwtSecret
