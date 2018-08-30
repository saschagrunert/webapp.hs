-- | Running the application within GHCi
--
-- This option provides significantly faster code reload compared to
-- @yesod devel@. However, you do not get automatic code reload
-- (which may be a benefit, depending on your perspective).
--
-- 1. Start up GHCi
--    $ stack ghci webapp:lib --no-load --work-dir .stack-work-devel
-- 2. Load this module
--    > :l app/DevelMain.hs
-- 3. Run @update@
--    >  Ghci.update
-- 4. Your app should now be running, you can connect at http://localhost:3000
-- 5. Make changes to your code
-- 6. After saving your changes, reload by running:
--    > :r
--    > Ghci.update
--
-- You can also call @DevelMain.shutdown@ to stop the app
module Ghci
  ( update
  , shutdown
  ) where

import Application              (getRepl)
import Control.Concurrent       (MVar, ThreadId, forkFinally, killThread,
                                 newEmptyMVar, putMVar, takeMVar)
import Control.Monad            ((>=>))
import Data.IORef               (IORef, newIORef, readIORef, writeIORef)
import Foreign.Store            (Store (Store), lookupStore, readStore,
                                 storeAction, withStore)
import GHC.Word                 (Word32)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)

-- | Start or restart the server
update :: IO ()
update = do
  mtidStore <- lookupStore tidStoreNum
  case mtidStore
      -- no server running
        of
    Nothing -> do
      done <- storeAction doneStore newEmptyMVar
      tid <- start done
      _ <- storeAction (Store tidStoreNum) (newIORef tid)
      return ()
      -- server is already running
    Just tidStore -> restartAppInNewThread tidStore
  where
    doneStore :: Store (MVar ())
    doneStore = Store 0
    -- shut the server down with killThread and wait for the done signal
    restartAppInNewThread :: Store (IORef ThreadId) -> IO ()
    restartAppInNewThread tidStore =
      modifyStoredIORef tidStore $ \tid -> do
        killThread tid
        withStore doneStore takeMVar
        readStore doneStore >>= start
    -- | Start the server in a separate thread.
    start ::
         MVar () -- ^ Written to when the thread is killed.
      -> IO ThreadId
    start done = do
      (port, site, app) <- getRepl
      forkFinally
        (runSettings (setPort port defaultSettings) app)
        (\_ -> putMVar done () >> (\_ -> return ()) site)

-- | Shutdown the server
shutdown :: IO ()
shutdown = do
  mtidStore <- lookupStore tidStoreNum
  case mtidStore of
    Nothing -> putStrLn "no Yesod app running"
    Just tidStore -> do
      withStore tidStore $ readIORef >=> killThread
      putStrLn "Yesod app is shutdown"

tidStoreNum :: Word32
tidStoreNum = 1

modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f =
  withStore store $ \ref -> do
    v <- readIORef ref
    f v >>= writeIORef ref
