{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
  (
  ) where

import Foundation (App, Route (HomeR), resourcesApp)
import Home       (getHomeR)
import Yesod.Core (mkYesodDispatch)

mkYesodDispatch "App" resourcesApp
