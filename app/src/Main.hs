module Main where

import Routes
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  runApi
