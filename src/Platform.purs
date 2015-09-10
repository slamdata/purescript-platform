module Platform
  ( Platform()
  , PlatformInfo()
  , PLATFORM()
  , getPlatform
  , platformInfo
  , parse
  ) where

import Prelude
import Control.Monad.Eff

foreign import data PLATFORM :: !
foreign import data Platform :: *

type PlatformInfo =
  { description :: String
  , layout :: String
  , manufacturer :: String
  , name :: String
  , prerelease :: String
  , product :: String
  , ua :: String
  , os :: String
  , version :: String
  }

foreign import parse :: String -> Platform
foreign import getPlatform :: forall e. Eff (platform :: PLATFORM | e) Platform
foreign import platformInfo :: Platform -> PlatformInfo
foreign import _toString :: Platform -> String

instance showPlatform :: Show Platform where
  show = _toString

