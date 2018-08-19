{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Runtime application configuration
--
-- @since 0.1.0
module Settings
  ( AppSettings(..)
  , configSettingsYmlValue
  ) where

import Control.Exception        (throw)
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
    { appStaticDir              :: String
    , appHost                   :: HostPreference
    , appPort                   :: Int
    , appIpFromHeader           :: Bool
    , appDetailedRequestLogging :: Bool
    , appMutableStatic          :: Bool
    }

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
        appStaticDir              <- o .: "static-dir"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"
        dev                       <- o .:? "development"      .!= defaultDev
        appDetailedRequestLogging <- o .:? "detailed-logging" .!= dev
        appMutableStatic          <- o .:? "mutable-static"   .!= dev
        return AppSettings {..}

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
