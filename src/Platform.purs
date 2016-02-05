module Platform
  ( Platform(..)
  , PLATFORM()
  , Os(..)
  , Prerelease(..)
  , OsRec()
  , PlatformRec()
  , runPlatform
  , runOs
  , getPlatform
  , parse
  ) where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (MonadEff, liftEff)
import Data.Either (either, Either(..))
import Data.Foreign (readString, Foreign(), ForeignError(TypeMismatch), F())
import Data.Foreign.Class (IsForeign, read, readProp)
import Data.Foreign.Null (runNull)
import Data.Maybe (Maybe(..))

foreign import data PLATFORM :: !

type OsRec =
  { architecture :: Maybe Int
  , family :: Maybe String
  , version :: Maybe String
  }

newtype Os = Os OsRec
runOs :: Os -> OsRec
runOs (Os r) = r

instance isForeignOs :: IsForeign Os where
  read f = do
    r <-
      { architecture: _
      , family: _
      , version: _
      } <$> readMaybeNull "architecture" f
        <*> readMaybeNull "family" f
        <*> readMaybeNull "version" f
    pure $ Os r

instance showOs :: Show Os where
  show (Os r) =
    "(Os \n"
      <> "{ architecture = " <> show r.architecture <> ",\n"
      <> "  family = " <> show r.family <> ",\n"
      <> "  version = " <> show r.version <> "})"

instance eqOs :: Eq Os where
  eq (Os r1) (Os r2) =
    r1.architecture == r2.architecture
      && r1.family == r2.family
      && r1.version == r2.version

data Prerelease = Alpha | Beta

instance showPrerelease :: Show Prerelease where
  show Alpha = "Alpha"
  show Beta = "Beta"

instance eqPrerelease :: Eq Prerelease where
  eq Alpha Alpha = true
  eq Beta Beta = true
  eq _ _  = false

instance isForeignPrerelease :: IsForeign Prerelease where
  read f = do
    str <- readString f
    case str of
      "alpha" -> pure Alpha
      "beta" -> pure Beta
      _ -> Left $ TypeMismatch "prerelease" "string"

type PlatformRec =
  { description :: Maybe String
  , layout :: Maybe String
  , manufacturer :: Maybe String
  , name :: Maybe String
  , prerelease :: Maybe Prerelease
  , product :: Maybe String
  , ua :: Maybe String
  , version :: Maybe String
  , os :: Os
  }

newtype Platform = Platform PlatformRec
runPlatform :: Platform -> PlatformRec
runPlatform (Platform r) = r

instance showPlatform :: Show Platform where
  show (Platform r) =
    "(Platform \n"
      <> " {description = " <> show r.description <> ",\n"
      <> "  layout = " <> show r.layout <> ",\n"
      <> "  manufacturer = " <> show r.manufacturer <> ",\n"
      <> "  name = " <> show r.name <> ",\n"
      <> "  prerelease = " <> show r.prerelease <> ",\n"
      <> "  product = " <> show r.product <> ",\n"
      <> "  ua = " <> show r.ua <> ",\n"
      <> "  version = " <> show r.version <> ",\n"
      <> "  os = " <> show r.os <> "})"

instance isForeignPlatform :: IsForeign Platform where
  read f = do
    r <-
      { description: _
      , layout: _
      , manufacturer: _
      , name: _
      , prerelease: _
      , product: _
      , ua: _
      , version: _
      , os: _
      } <$> readMaybeNull "description" f
        <*> readMaybeNull "layout" f
        <*> readMaybeNull "manufacturer" f
        <*> readMaybeNull "name" f
        <*> readMaybeNull "prerelease" f
        <*> readMaybeNull "product" f
        <*> readMaybeNull "ua" f
        <*> readMaybeNull "version" f
        <*> readProp "os" f
    pure $ Platform r

readMaybeNull :: forall a. (IsForeign a) => String -> Foreign -> F (Maybe a)
readMaybeNull key f = map runNull $ readProp key f

foreign import getPlatform_ :: forall e. Eff (platform :: PLATFORM|e) Foreign

getPlatform :: forall m e. (Monad m, MonadEff (platform :: PLATFORM|e) m) => m (Maybe Platform)
getPlatform = do
  f <- liftEff getPlatform_
  pure case read f of
    Left _ -> Nothing
    Right p -> Just p

foreign import parse_ :: String -> Foreign

parse :: String -> Maybe Platform
parse str = either (const Nothing) Just $ read $ parse_ str
