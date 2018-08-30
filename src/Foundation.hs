{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Foundation where

import           Control.Lens         ((^.))
import           Control.Monad.Logger (LogSource)
import qualified Data.CaseInsensitive as CI (foldedCase)
import qualified Data.Text.Encoding   as TE (decodeUtf8)
import           Import.NoFoundation
import           Text.Hamlet          (hamletFile)
import           Text.Jasmine         (minifym)
import           Yesod.Core.Types     (Logger)
import           Yesod.Default.Util   (addStaticContentExternal)

-- | The main application
--
-- @since 0.1.0
data App = App
  { appSettings    :: AppSettings
  , appStatic      :: Static
  , appHttpManager :: Manager
  , appLogger      :: Logger
  }

mkYesodData "App" $(parseRoutesFile "config/routes")

data MenuItem = MenuItem
  { menuItemLabel          :: Text
  , menuItemRoute          :: Route App
  , menuItemAccessCallback :: Bool
  }

data MenuTypes
  = NavbarLeft MenuItem
  | NavbarRight MenuItem

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  makeLogger :: App -> IO Logger
  makeLogger = return . appLogger
  makeSessionBackend :: App -> IO (Maybe SessionBackend)
  makeSessionBackend _ = return Nothing
  shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
  shouldLogIO _ _ _ = return True
  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    mcurrentRoute <- getCurrentRoute
        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
    (title, parents) <- breadcrumbs
        -- Define the menu items of the header.
    let menuItems =
          [ NavbarLeft $
            MenuItem
            { menuItemLabel = "Home"
            , menuItemRoute = HomeR
            , menuItemAccessCallback = True
            }
          ]
    let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
    let navbarRightMenuItems = [x | NavbarRight x <- menuItems]
    let navbarLeftFilteredMenuItems =
          [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
    let navbarRightFilteredMenuItems =
          [x | x <- navbarRightMenuItems, menuItemAccessCallback x]
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.
    pc <-
      widgetToPageContent $ do
        addStylesheet $ StaticR css_bootstrap_css
        $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
  isAuthorized ::
       Route App -- ^ The route the user is visiting.
    -> Bool -- ^ Whether or not this is a "write" request.
    -> Handler AuthResult
  isAuthorized FaviconR _ = return Authorized
  isAuthorized _ _        = return Authorized
  addStaticContent ::
       Text -- ^ The file extension
    -> Text -- ^ The MIME content type
    -> LByteString -- ^ The contents of the file
    -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appSettings master ^. appStaticDir
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
    where
      genFileName lbs = "autogen-" ++ base64md5 lbs

-- Define the breadcrumbs
instance YesodBreadcrumbs App where
  breadcrumb :: Route App -> Handler (Text, Maybe (Route App))
  breadcrumb HomeR = return ("Home", Nothing)
  breadcrumb _     = return ("home", Nothing)

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage :: App -> [Lang] -> FormMessage -> Text
  renderMessage _ _ = defaultFormMessage

-- | Useful when writing code that is re-usable outside of the Handler context.
instance HasHttpManager App where
  getHttpManager :: App -> Manager
  getHttpManager = appHttpManager
