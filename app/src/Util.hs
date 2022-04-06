{-# LANGUAGE NumericUnderscores #-}

module Util (sleepSeconds) where

import Control.Concurrent (threadDelay)

sleepSeconds :: Int -> IO ()
sleepSeconds secs =
  threadDelay $ secs * 1_000_000
