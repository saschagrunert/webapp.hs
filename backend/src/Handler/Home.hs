module Handler.Home
  ( getHomeR
  ) where

import Foundation (Handler)
import Yesod.Core (sendFile, typeHtml)

-- | The default home handler, which simply serves the static index file
--
-- @since 0.1.0
getHomeR :: Handler ()
getHomeR = sendFile typeHtml "static/index.html"
