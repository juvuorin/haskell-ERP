{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Foundation where


import Auth.JWT
import Control.Monad.Logger (LogSource)
import Data.Aeson (Result (Success), fromJSON)
import qualified Data.CaseInsensitive as CI
import Data.Text.Encoding (decodeLatin1)
import qualified Data.Text.Encoding as TE
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Database.Persist.Sql (ConnectionPool, fromSqlKey, runSqlPool, toSqlKey)
import Import.NoFoundation

import Network.Wai (pathInfo)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import WaiAppStatic.Types
import Web.JWT
import Yesod.Auth.Dummy
import Yesod.Auth.Email
import qualified Yesod.Auth.Message as AuthMsg
import Yesod.Auth.OpenId (IdentifierType (Claimed), authOpenId)
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Default.Util (addStaticContentExternal)
import Prelude (read, (!!))
import qualified GHC.Conc as T
import Types (DefaultAccountType, Rolename (Admin))
import Control.Monad.Reader

-- The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
  { appSettings :: AppSettings,
    -- | Settings for static file serving.
    appStatic :: Static,
    -- | Database connection pool.
    appConnPool :: ConnectionPool,
    appHttpManager :: Manager,
    appLogger :: Logger,
    appAccountMap :: Map DefaultAccountType Int,
    appAccountSet :: Set Int
  }

data MenuItem = MenuItem
  { menuItemLabel :: Text,
    menuItemRoute :: Route App,
    menuItemAccessCallback :: Bool
  }

data MenuTypes
  = NavbarLeft MenuItem
  | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

-- A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- A convenient synonym for database access functions.
{- type DB a =
  forall (m :: * -> *).
  (MonadIO m) =>
  ReaderT SqlBackend m a
 -}
{- type DB a =
  forall (m :: * -> *).
  (MonadIO m) =>
  ReaderT SqlBackend m a
 -}
--type DB = ReaderT SqlBackend Handler
type DB = ReaderT SqlBackend Handler


{- instance PathPiece CompanyId where
    toPathPiece (CompanyId i) = pack $ show i
    fromPathPiece s =
        case reads $ unpack s of
            (i, ""):_
                | i < 1 -> Nothing
                | otherwise -> Just $ CompanyId i
            [] -> Nothing
 -}

{- isAdmin = do
    mu <- maybeAuthId
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized
        Just _ -> Unauthorized "You must be an admin"
 -}

isAdmin :: Handler AuthResult
isAdmin = do
  user <- getAuthenticatedUser
  let userId = entityKey user
  adminEntity <- runDB $ selectFirst [RoleName ==. Admin] []
  case adminEntity of
    Just x -> do
      isAdmin <- runDB $ exists [UserRoleRoleId ==. entityKey x, UserRoleUserId ==. userId]
      if isAdmin then return Authorized else return $ Unauthorized "User must have admin rights to add new users"
    Nothing -> return $ Unauthorized "Admin role does not exist"

getAuthenticatedUser = do
  user <- maybeAuth
  case user of
    Just x -> return x
    Nothing -> sendResponseStatus status403 ("User is not authenticated" :: Text)

accessRightMiddleware :: Handler res -> Handler res
accessRightMiddleware handler = do
  e <- getYesod
  let set = appSettings e
  req <- waiRequest
  let path = pathInfo req

  -- | Check if we need to check the company id in order to assess access rights
  maybeCompanyId <- case path of
    -- | If user is requesting a list of companies, companyId is not needed
    (company : companies : _)
      | company == "company" && companies == "companies" ->
        return $ Nothing

    -- | If user is requesting company or filestore related services the companyId is needed
    (x : _)
      | x == "company" || x == "filestore" ->
        return $ Just $ toSqlKey $ (read $ unpack $ path !! 1 :: Int64)

    -- | If user is requesting payroll related services the companyId is needed
    (payroll : company : _)
      | payroll == "payroll" && company == "company" ->
        return $ Just $ toSqlKey (read $ unpack $ path !! 2 :: Int64)
    _ -> return Nothing --sendResponseStatus status404 ("This route does not exist" :: Text)
  
  
  -- | If companyId is needed we check if the user has access to the company with the companyId
  case maybeCompanyId of
    Just x -> do
      user <- getAuthenticatedUser    
      accessOk <- runDB $ exists [UserCompanyUserId ==. entityKey user, UserCompanyCompanyId ==. x]
      if accessOk then handler else sendResponseStatus status403 ("The user has no access to this company" :: Text)
    Nothing -> handler

dbAccessMiddleware :: Handler res -> Handler res
dbAccessMiddleware handler = do
  mauth <- maybeAuth
  case mauth of
    Nothing -> handler
    Just (Entity userId user) -> do
      runDB $
        liftIO getCurrentTime >>= \time ->
          ( insert $
              AccessLog
                { accessLogUserId = userId,
                  accessLogTime = time
                }
          )
            >> return ()
      handler

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  -- Controls the base of generated URLs. For more information on modifying,
  -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
  approot :: Approot App
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
      Nothing -> getApprootText guessApproot app req
      Just root -> root

  -- Store session data on the client in encrypted cookies,
  -- default session idle timeout is 120 minutes
  {-     makeSessionBackend :: App -> IO (Maybe SessionBackend)
      makeSessionBackend _ = Just <$> defaultClientSessionBackend
          120    -- timeout in minutes
          "config/client_session_key.aes"
   -}
  makeSessionBackend :: App -> IO (Maybe SessionBackend)
  makeSessionBackend _ =
    sslOnlySessions $
      fmap Just $ defaultClientSessionBackend 120 "mykey.aes"

  -- Yesod Middleware allows you to run code before and after each handler function.
  -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
  -- Some users may also want to add the defaultCsrfMiddleware, which:
  --   a) Sets a cookie with a CSRF token in it.
  --   b) Validates that incoming write requests include that token in either a header or POST parameter.
  -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
  -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
  yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
  yesodMiddleware site =
    dbAccessMiddleware $
    accessRightMiddleware $
    defaultYesodMiddleware $
    sslOnlyMiddleware 120 site

  isAuthorized ::
    -- | The route the user is visiting.
    Route App ->
    -- | Whether or not this is a "write" request.
    Bool ->
    Handler AuthResult
  -- Routes not requiring authentication.
  --  isAuthorized UsersRegisterR _ = return Authorized
  isAuthorized UsersLoginR _ = return Authorized
  isAuthorized UsersRegisterR _ = isAdmin
  -- isAuthorized (VatReportSendR _) _ = return Authorized

  isAuthorized _ _ = isAuthenticated
  maximumContentLength _ (Just (FilesMonthlyR _)) = Just (2 * 1024 * 1024 * 1024)
  --  maximumContentLength _  (FilesMonthlyR )  = 1024
  maximumContentLength _ _ = Just (10 * 1024 * 1024)

  --    isAuthorized _ _ = return Authorized

  --  isAuthorized _ _ _ = isAuthenticated

  -- the profile route requires that the user is authenticated, so we
  -- delegate to that function

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent ::
    -- | The file extension
    Text ->
    -- | The MIME content type
    Text ->
    -- | The contents of the file
    LByteString ->
    Handler (Maybe (Either Text (Route App, [(Text, Text)])))
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
    where
      -- Generate a unique filename based on the content itself
      genFileName lbs = "autogen-" ++ base64md5 lbs

  -- What messages should be logged. The following includes all messages when
  -- in development, and warnings and errors in production.
  shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
  shouldLogIO app _source level =
    return $
      appShouldLogAll (appSettings app)
        || level == LevelWarn
        || level == LevelError

  makeLogger :: App -> IO Logger
  makeLogger = return . appLogger

  {- NotFound
  The requested resource was not found. Examples of when this occurs include when an incorrect URL is used, or yesod-persistent's get404 doesn't find a value. HTTP status: 404.

  InternalError !Text
  Some sort of unexpected exception. If your application uses throwIO or error to throw an exception, this is the form it would take. HTTP status: 500.

  InvalidArgs ![Text]
  Indicates some sort of invalid or missing argument, like a missing query parameter or malformed JSON body. Examples Yesod functions that send this include requireCheckJsonBody and Yesod.Auth.GoogleEmail2. HTTP status: 400.

  NotAuthenticated
  Indicates the user is not logged in. This is thrown when isAuthorized returns AuthenticationRequired. HTTP code: 401.

  PermissionDenied !Text
  Indicates the user doesn't have permission to access the requested resource. This is thrown when isAuthorized returns Unauthorized. HTTP code: 403.

  BadMethod !Method
  Indicates the URL would have been valid if used with a different HTTP method (e.g. a GET was used, but only POST is handled.) HTTP code: 405.
  -}
  errorHandler err = case err of
    PermissionDenied message ->
      return $
        toTypedContent $ object ["error" .= ("Permission denied" :: Text)]
    NotFound ->
      return $
        toTypedContent $ object ["error" .= ("Document not found" :: Text)]
    _ ->
      return $
        toTypedContent $ object ["error" .= ("Unspecified error" :: Text)]

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB :: SqlPersistT Handler a -> Handler a
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner :: Handler (DBRunner App, Handler ())
  getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
  type AuthId App = UserId

  redirectToReferer _ = True

  authPlugins _ = []

  authHttpManager = error "Doesn't need an HTTP manager"

  authenticate _ =
    maybe (UserError AuthMsg.InvalidLogin) Authenticated <$> maybeAuthId



  maybeAuthId = do
    mToken <- Auth.JWT.lookupToken
    userId <- case mToken of
      Just token -> do
        validUser <- liftHandler $ tokenToUserId token

        -- | We need a session cookie for banking activities (for redirecting)
        case validUser of
          Just valid -> do
            setSession "auth_token" token
          Nothing -> return ()
        liftHandler $ tokenToUserId token
      Nothing -> do
        token <- liftHandler $ lookupSession "auth_token"
        case token of
          Just token' -> do
            liftHandler $ tokenToUserId token'
          _ -> return Nothing

    token <- liftHandler $ lookupSession "auth_token"
    session <- liftHandler getSession

    liftHandler $ return userId

-- Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
  muid <- maybeAuthId
  case muid of
    Nothing -> return (Unauthorized "You must login to access this page")
    Just _ -> return Authorized

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage :: App -> [Lang] -> FormMessage -> Text
  renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
  getHttpManager :: App -> Manager
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
userIdToToken :: UserId -> HandlerFor App Text
userIdToToken userId = do
  jwtSecret <- getJwtSecret
  expTime <- getExpirationTime
  let token = Auth.JWT.jsonToToken jwtSecret (toJSON userId) expTime
  -- Set session token for redirects
  setSession "auth_token" token
  return token

getExpirationTime :: HandlerFor App NominalDiffTime
getExpirationTime = do
  currentTime <- liftIO getCurrentTime
  let expirationTime = addUTCTime (77 * nominalDay) currentTime
  let posixSeconds = utcTimeToPOSIXSeconds expirationTime
  return posixSeconds

{- validateTokenInCookie::Text -> Text -> Handler (Maybe Text)
validateTokenInCookie = token jwtSecret
  let jwtCookie = lookupSession "jwt-cookie"
    case jwtCookie of
      Just cookie ->
      Nothing ->
 -}

tokenToUserId :: Text -> Handler (Maybe UserId)
tokenToUserId token = do
  jwtSecret <- getJwtSecret
  maybeValidated <- validateToken token jwtSecret
  case maybeValidated of
    Just token' -> do
      let mUserId = fromJSON <$> Auth.JWT.tokenToJson jwtSecret token'
      case mUserId of
        Just (Success userId) -> return $ Just userId
        _ -> return Nothing
    Nothing -> return Nothing

getJwtSecret :: Handler Text
getJwtSecret =
  getsYesod $ appJwtSecret . appSettings

isDateExpired :: Maybe Web.JWT.NumericDate -> Maybe Web.JWT.NumericDate -> Maybe Bool
isDateExpired exptime currtime = (<) <$> exptime <*> currtime

-- Notice that IntDate and NumericDate are type synonyms
validateToken :: MonadIO m => Text -> Text -> m (Maybe Text)
validateToken bearerToken secret = do
  let decodedAndVerified = Web.JWT.decodeAndVerifySignature (Web.JWT.hmacSecret secret) bearerToken
      claimset = Web.JWT.claims <$> decodedAndVerified
      expiration = Web.JWT.exp =<< claimset

  posixTime <- liftIO getPOSIXTime
  let expired = isDateExpired expiration (Web.JWT.numericDate posixTime)

  case expired of
    Nothing -> return Nothing
    Just expired -> do
      if expired then return Nothing else return $ Just bearerToken
