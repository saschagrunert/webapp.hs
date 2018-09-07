module Handler.CommonSpec
  ( spec_common
  ) where

import TestImport

spec_common :: Spec
spec_common =
  withApp $
  describe "favicon.ico" $
  it "should return 200 status" $ do
    get FaviconR
    statusIs 200
