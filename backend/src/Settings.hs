{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Runtime application configuration
--
-- @since 0.1.0
module Settings
  ( AppSettings(..)
  , appDetailedRequestLogging
  , appHost
  , appIpFromHeader
  , appMutableStatic
  , appPort
  , appStaticDir
  , configSettingsYmlValue
  ) where

import Control.Exception        (throw)
import Control.Lens             (makeLenses)
import Data.Aeson               (FromJSON (parseJSON), withObject, (.!=), (.:),
                                 (.:?))
import Data.ByteString          (ByteString)
import Data.FileEmbed           (embedFile)
import Data.String              (fromString)
import Data.Yaml                (Value, decodeEither')
import Network.Wai.Handler.Warp (HostPreference)
import Yesod.Default.Config2    (configSettingsYml)

-- | The main application settings
--
-- @since 0.1.0
data AppSettings = AppSettings
    { _appStaticDir              :: String
    , _appHost                   :: HostPreference
    , _appPort                   :: Int
    , _appIpFromHeader           :: Bool
    , _appDetailedRequestLogging :: Bool
    , _appMutableStatic          :: Bool
    }

makeLenses ''AppSettings

-- | Yaml parsing instance for the AppSettings
--
-- @since 0.1.0
instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#ifdef DEVELOPMENT
                True
#else
                False
#endif
        staticDir              <- o .: "static-dir"
        host                   <- fromString <$> o .: "host"
        port                   <- o .: "port"
        ipFromHeader           <- o .: "ip-from-header"
        dev                    <- o .:? "development"      .!= defaultDev
        detailedRequestLogging <- o .:? "detailed-logging" .!= dev
        mutableStatic          <- o .:? "mutable-static"   .!= dev
        return AppSettings { _appStaticDir = staticDir
                           , _appHost = host
                           , _appPort = port
                           , _appIpFromHeader = ipFromHeader
                           , _appDetailedRequestLogging = detailedRequestLogging
                           , _appMutableStatic = mutableStatic
                           }

-- | `config/settings.yml`, parsed to a `Value`.
--
-- @since 0.1.0
configSettingsYmlValue :: Value
configSettingsYmlValue = either throw id $ decodeEither' configSettingsYmlBS

-- | Raw bytes at compile time of `config/settings.yml`
--
-- @since 0.1.0
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)
