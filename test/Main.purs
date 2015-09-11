module Test.Main where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.Maybe (maybe)
import qualified Platform as P

main :: Eff (platform :: P.PLATFORM, console :: CONSOLE) Unit
main = do
  platform <- P.getPlatform
  maybe (log "Error") (log <<< show) platform
