{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
  ( appMain
  , develMain
  ) where

import Control.Lens                         ((^.))
import Control.Monad                        (when)
import Control.Monad.Logger                 (LogLevel (LevelError), liftLoc,
                                             toLogStr)
import Data.Default                         (def)
import Foundation                           (App (App, appHttpManager, appLogger, appSettings, appStatic),
                                             Route (HomeR, StaticR),
                                             resourcesApp)
import Handler.Home                         (getHomeR)
import Language.Haskell.TH.Syntax           (qLocation)
import Network.HTTP.Client.TLS              (getGlobalManager)
import Network.Wai                          (Application, Middleware)
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             runSettings, setHost,
                                             setOnException, setPort)
import Network.Wai.Middleware.RequestLogger (Destination (Logger), IPAddrSource (FromFallback, FromSocket),
                                             OutputFormat (Apache, Detailed),
                                             destination, mkRequestLogger,
                                             outputFormat)
import Settings                             (AppSettings,
                                             appDetailedRequestLogging, appHost,
                                             appIpFromHeader, appMutableStatic,
                                             appPort, appStaticDir,
                                             configSettingsYmlValue)
import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet)
import Yesod.Core                           (defaultMiddlewaresNoLogging,
                                             messageLoggerSource,
                                             mkYesodDispatch, toWaiAppPlain)
import Yesod.Core.Types                     (loggerSet)
import Yesod.Default.Config2                (configSettingsYml, develMainHelper,
                                             getDevSettings, loadYamlSettings,
                                             loadYamlSettingsArgs,
                                             makeYesodLogger, useEnv)
import Yesod.Static                         (static, staticDevel)

mkYesodDispatch "App" resourcesApp

-- | Main function for production environments
--
-- @since 0.1.0
appMain :: IO ()
appMain = do
  settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
  foundation <- makeFoundation settings
  app <- makeApplication foundation
  runSettings (warpSettings foundation) app

-- | Main function for yesod devel
--
-- @since 0.1.0
develMain :: IO ()
develMain = do
  settings <- loadYamlSettings [configSettingsYml] [] useEnv
  foundation <- makeFoundation settings
  devSettings <- getDevSettings $ warpSettings foundation
  app <- makeApplication foundation
  develMainHelper $ return (devSettings, app)

-- | Create the applications foundation
--
-- @since 0.1.0
makeFoundation :: AppSettings -> IO App
makeFoundation settings = do
  httpManager <- getGlobalManager
  logger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  stat <-
    (if settings ^. appMutableStatic
       then staticDevel
       else static)
      (settings ^. appStaticDir)
  return
    App
    { appSettings = settings
    , appStatic = stat
    , appHttpManager = httpManager
    , appLogger = logger
    }

-- | Warp settings for the given foundation value
--
-- @since 0.1.0
warpSettings :: App -> Settings
warpSettings foundation =
  setPort (appSettings foundation ^. appPort) $
  setHost (appSettings foundation ^. appHost) $
  setOnException
    (\_req e ->
       when (defaultShouldDisplayException e) $
       messageLoggerSource
         foundation
         (appLogger foundation)
         $(qLocation >>= liftLoc)
         "yesod"
         LevelError
         (toLogStr $ "Exception from Warp: " ++ show e))
    defaultSettings

-- | Convert our foundation to a WAI Application by calling `toWaiAppPlain` and
-- applying additional middlewares.
--
-- @since 0.1.0
makeApplication :: App -> IO Application
makeApplication foundation = do
  logWare <- makeLogWare foundation
  appPlain <- toWaiAppPlain foundation
  return . logWare $ defaultMiddlewaresNoLogging appPlain

-- | Create the logger middleware
--
-- @since 0.1.0
makeLogWare :: App -> IO Middleware
makeLogWare foundation =
  mkRequestLogger
    def
    { outputFormat =
        if appSettings foundation ^. appDetailedRequestLogging
          then Detailed True
          else Apache
                 (if appSettings foundation ^. appIpFromHeader
                    then FromFallback
                    else FromSocket)
    , destination = Logger . loggerSet $ appLogger foundation
    }
