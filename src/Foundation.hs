{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE TemplateHaskell #-}

module Foundation
  ( App(App, appSettings, appStatic, appHttpManager, appLogger)
  , Handler
  , Route(..)
  , Widget
  , resourcesApp
  ) where

import Control.Monad.Logger (LogLevel, LogSource)
import Network.HTTP.Client  (Manager)
import Settings             (AppSettings)
import Yesod.Core           (Route, Yesod (makeLogger, shouldLogIO),
                             mkYesodData, parseRoutesFile, renderRoute)
import Yesod.Core.Types     (Logger)
import Yesod.Static         (Static)

-- | The main application
--
-- @since 0.1.0
data App = App
  { appSettings    :: AppSettings
  , appStatic      :: Static
  , appHttpManager :: Manager
  , appLogger      :: Logger
  }

mkYesodData "App" $(parseRoutesFile "config/routes")

-- | The yesod instance of the application
--
-- @since 0.1.0
instance Yesod App where
  makeLogger :: App -> IO Logger
  makeLogger = return . appLogger
  shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
  shouldLogIO _ _ _ = return True
