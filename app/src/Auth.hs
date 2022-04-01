module Auth ( verify ) where

import Control.Monad.Except
import qualified Crypto.JWT as JWT
import qualified Data.ByteString.Lazy as L

verify :: L.ByteString -> L.ByteString -> IO (Either JWT.JWTError JWT.ClaimsSet)
verify secret token = runExceptT $ do
  let secret' = JWT.fromOctets secret -- turn raw secret into symmetric JWK
      audCheck = const True -- should be a proper audience check
  token' <- JWT.decodeCompact token -- decode JWT
  JWT.verifyClaims (JWT.defaultJWTValidationSettings audCheck) secret' token'
