module Handler.Api
  ( getApiR
  , postApiR
  ) where

import Import

getApiR :: Handler Value
getApiR = return $ String "Hello world"

postApiR :: Handler ()
postApiR = do
  post <- requireJsonBody :: Handler Value
  sendResponse post
