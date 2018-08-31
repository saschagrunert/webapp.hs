{-# LANGUAGE TemplateHaskell #-}

module Handler.Home
  ( getHomeR
  ) where

import Import

-- | The Home get request handler
--
-- @since 0.1.0
getHomeR :: Handler Html
getHomeR =
  defaultLayout $ do
    setTitle "Home"
    $(widgetFile "home")
