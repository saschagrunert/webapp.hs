{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Settings
  ( appStaticDir
  , compileTimeAppSettings
  , AppSettings
  , widgetFile
  , appDetailedRequestLogging
  , appHost
  , appMutableStatic
  , appPort
  , appIpFromHeader
  , configSettingsYml
  , configSettingsYmlValue
  ) where

import           ClassyPrelude.Yesod
import qualified Control.Exception          as Exception
import           Control.Lens               (makeLenses, (^.))
import           Data.Aeson                 (Result (..), fromJSON, withObject,
                                             (.!=), (.:?))
import           Data.FileEmbed             (embedFile)
import           Data.Yaml                  (decodeEither')
import           Language.Haskell.TH.Syntax (Exp, Name, Q)
import           Network.Wai.Handler.Warp   (HostPreference)
import           Yesod.Default.Config2      (applyEnvValue, configSettingsYml)
import           Yesod.Default.Util         (WidgetFileSettings,
                                             widgetFileReload)

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
  { _appStaticDir              :: String
  , _appHost                   :: HostPreference
  , _appPort                   :: Int
  , _appIpFromHeader           :: Bool
  , _appDetailedRequestLogging :: Bool
  , _appMutableStatic          :: Bool
  , _appSkipCombining          :: Bool
  }

makeLenses ''AppSettings

instance FromJSON AppSettings where
  parseJSON =
    withObject "AppSettings" $ \o -> do
      let defaultDev =
#ifdef DEVELOPMENT
            True
#else
            False
#endif
      _appStaticDir <- o .: "static-dir"
      _appHost <- fromString <$> o .: "host"
      _appPort <- o .: "port"
      _appIpFromHeader <- o .: "ip-from-header"
      dev <- o .:? "development" .!= defaultDev
      _appDetailedRequestLogging <- o .:? "detailed-logging" .!= dev
      _appMutableStatic <- o .:? "mutable-static" .!= dev
      _appSkipCombining <- o .:? "skip-combining" .!= dev
      return
        AppSettings
        { _appStaticDir = _appStaticDir
        , _appHost = _appHost
        , _appPort = _appPort
        , _appIpFromHeader = _appIpFromHeader
        , _appDetailedRequestLogging = _appDetailedRequestLogging
        , _appMutableStatic = _appMutableStatic
        , _appSkipCombining = _appSkipCombining
        }

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- @since 0.1.0
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.
--
-- @since 0.1.0
widgetFile :: String -> Q Exp
widgetFile = widgetFileReload widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@
--
-- @since 0.1.0
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
--
-- @since 0.1.0
configSettingsYmlValue :: Value
configSettingsYmlValue =
  either Exception.throw id $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
--
-- @since 0.1.0
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
  case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
    Error e          -> error e
    Success settings -> settings
