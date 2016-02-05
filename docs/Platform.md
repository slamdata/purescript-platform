## Module Platform

#### `PLATFORM`

``` purescript
data PLATFORM :: !
```

#### `OsRec`

``` purescript
type OsRec = { architecture :: Maybe Int, family :: Maybe String, version :: Maybe String }
```

#### `Os`

``` purescript
newtype Os
  = Os OsRec
```

##### Instances
``` purescript
IsForeign Os
Show Os
Eq Os
```

#### `runOs`

``` purescript
runOs :: Os -> OsRec
```

#### `Prerelease`

``` purescript
data Prerelease
  = Alpha
  | Beta
```

##### Instances
``` purescript
Show Prerelease
Eq Prerelease
IsForeign Prerelease
```

#### `PlatformRec`

``` purescript
type PlatformRec = { description :: Maybe String, layout :: Maybe String, manufacturer :: Maybe String, name :: Maybe String, prerelease :: Maybe Prerelease, product :: Maybe String, ua :: Maybe String, version :: Maybe String, os :: Os }
```

#### `Platform`

``` purescript
newtype Platform
  = Platform PlatformRec
```

##### Instances
``` purescript
Show Platform
IsForeign Platform
```

#### `runPlatform`

``` purescript
runPlatform :: Platform -> PlatformRec
```

#### `getPlatform`

``` purescript
getPlatform :: forall m e. (Monad m, MonadEff (platform :: PLATFORM | e) m) => m (Maybe Platform)
```

#### `parse`

``` purescript
parse :: String -> Maybe Platform
```


