module Home
  ( getHomeR
  ) where

import Foundation (Handler)
import Yesod.Core (Html, defaultLayout, setTitle)

getHomeR :: Handler Html
getHomeR = defaultLayout $ setTitle "Home"
