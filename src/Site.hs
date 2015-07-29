{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid
import           Data.Maybe
import qualified Data.Text as T
import           Database.Redis hiding (String, auth)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.RedisDB
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application

------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
    where
        errs = maybe mempty splice authError
        splice err = "loginError" ## I.textSplice err

------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
   where
        err = Just "Unknown user or password"

------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"

------------------------------------------------------------------------------
-- | Handle new user form submit
--handleNewUser :: Handler App (AuthManager App) ()
--handleNewUser = method GET handleForm <|> method POST handleFormSubmit
--    where
--        handleForm = render "new_user"
--        handleFormSubmit = registerUser "login" "password" >> redirect "/"

------------------------------------------------------------------------------
-- | Handle redis add data
handleAddData :: Handler App App ()
handleAddData = method GET handleForm <|> method POST handleFormSubmit
    where
        handleForm = render "add_data"
        handleFormSubmit = do
            runRedisDB redis $ set "key" "value"
            redirect "/"

------------------------------------------------------------------------------
-- | Handle redis show data
handleShowData :: Handler App App ()
handleShowData = method GET handleFormSubmit
    where
        -- Assuming if there's key, there's value, whatever it is.
        right = either undefined id
        handleFormSubmit = do
            (allkeys, alldata) <- runRedisDB redis $ do
                allkeys <- keys "*"
                alldata <- mget $ right allkeys
                return (right allkeys, map (fromMaybe "NULL") $ right alldata)
            writeBS $ BS.append ("Data I have:\n" :: ByteString) $ BS.pack $ show $ zip allkeys alldata

------------------------------------------------------------------------------
-- | Handle when not logged in
handleNotLoggedIn :: Handler App App ()
handleNotLoggedIn = writeBS "No User"

------------------------------------------------------------------------------
-- | Helper function
require :: SnapletLens App (AuthManager App)
           -> Handler App App ()
           -> Handler App App ()
require authe = requireUser authe handleNotLoggedIn

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
--         , ("/new_user", with auth handleNewUser)
         , ("/add_data", require auth handleAddData)
         , ("/show_data", require auth handleShowData)
         , ("",          serveDirectory "static")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"

    redisdb <- nestSnaplet "redis" redis redisDBInitConf

    addRoutes routes
    addAuthSplices h auth
    return $ App h s a redisdb

