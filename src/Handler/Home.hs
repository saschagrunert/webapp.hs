{-# LANGUAGE TemplateHaskell #-}

module Handler.Home
  ( getHomeR
  , postHomeR
  ) where

import Import

-- | The Home GET request handler
--
-- @since 0.1.0
getHomeR :: Handler Html
getHomeR = do
  (widget, enctype) <- generateFormPost $ renderDivs $ pure ()
  defaultLayout $ do
    setTitle "Home"
    $(widgetFile "home")

-- | The Home POST request handler
--
-- @since 0.1.0
postHomeR :: Handler Html
postHomeR = getHomeR
