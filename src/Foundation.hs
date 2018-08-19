{-# LANGUAGE TemplateHaskell #-}

module Foundation
  ( App(App, appSettings, appStatic, appHttpManager, appLogger)
  , Handler
  , Route(..)
  , Widget
  , resourcesApp
  ) where

import Network.HTTP.Client (Manager)
import Settings            (AppSettings)
import Yesod.Core          (Route, Yesod, mkYesodData, parseRoutesFile,
                            renderRoute)
import Yesod.Core.Types    (Logger)
import Yesod.Static        (Static)

data App = App
  { appSettings    :: AppSettings
  , appStatic      :: Static
  , appHttpManager :: Manager
  , appLogger      :: Logger
  }

mkYesodData "App" $(parseRoutesFile "config/routes")

instance Yesod App
