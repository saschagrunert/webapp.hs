{-# LANGUAGE TemplateHaskell #-}

-- | Common handler functions
module Handler.Common
  ( getFaviconR
  ) where

import Data.FileEmbed (embedFile)
import Import

-- | Retrieve the favicon
--
-- @since 0.1.0
getFaviconR :: Handler TypedContent
getFaviconR = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
  return $
    TypedContent "image/x-icon" $ toContent $(embedFile "config/favicon.ico")
