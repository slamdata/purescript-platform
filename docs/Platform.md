## Module Platform

#### `PLATFORM`

``` purescript
data PLATFORM :: !
```

#### `Platform`

``` purescript
data Platform :: *
```

##### Instances
``` purescript
instance showPlatform :: Show Platform
```

#### `PlatformInfo`

``` purescript
type PlatformInfo = { description :: String, layout :: String, manufacturer :: String, name :: String, prerelease :: String, product :: String, ua :: String, version :: String }
```

#### `parse`

``` purescript
parse :: String -> Platform
```

#### `getPlatform`

``` purescript
getPlatform :: forall e. Eff (platform :: PLATFORM | e) Platform
```

#### `platformInfo`

``` purescript
platformInfo :: Platform -> PlatformInfo
```


