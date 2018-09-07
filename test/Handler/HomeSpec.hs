{-# LANGUAGE NoImplicitPrelude #-}

module Handler.HomeSpec
  ( spec_home
  ) where

import TestImport

spec_home :: Spec
spec_home =
  withApp $
  describe "Homepage" $
  it "should load the index" $ do
    get HomeR
    statusIs 200
    htmlAnyContain "h1" "Welcome"
