{-# LANGUAGE TemplateHaskell #-}

module Foundation
  ( App(App)
  , Handler
  , Route(..)
  , Widget
  , resourcesApp
  ) where

import Yesod.Core (Route, Yesod, mkYesodData, parseRoutesFile, renderRoute)

data App =
  App

mkYesodData "App" $(parseRoutesFile "config/routes")

instance Yesod App
