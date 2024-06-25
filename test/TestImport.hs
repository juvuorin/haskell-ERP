--{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation, makeLogWare)
import ClassyPrelude         as X hiding (delete, deleteBy, Handler)
import Database.Persist      as X hiding (get)


import Database.Persist.Sql  (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, rawSql, unSingle)
import Foundation            as X
import Model                 as X
import Test.Hspec            as X
import Yesod.Default.Config2 (useEnv, loadAppSettings, loadYamlSettings, ignoreEnv)
import Yesod.Auth            as X
import Yesod.Test            as X
import Yesod.Core.Unsafe     (fakeHandlerGetLogger)
import           System.Environment         (setEnv)
import Database.Persist.Types
-- Wiping the database
--import Database.Persist.Sqlite              (sqlDatabase, mkSqliteConnectionInfo, fkEnabled, createSqlitePoolFromInfo)
--import Database.Persist.Postgresql--              (sqlDatabase, mkSqliteConnectionInfo, fkEnabled, createSqlitePoolFromInfo)
import Database.Persist.Postgresql
  ( createPostgresqlPool,
    pgConnStr,
    pgPoolSize,
    runSqlPool,
  )

import Control.Monad.Logger                 (runLoggingT)
import Lens.Micro                           (set)
import Settings (appDatabaseConf)
import Yesod.Core (messageLoggerSource)
import Text.Shakespeare.Text (st)

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

<<<<<<< HEAD

-- | Get response from JSON body.
{- getJsonResponse :: FromJSON a => YesodExample App a
getJsonResponse =
  withResponse $ \SResponse {..} ->
    case fromJSON <$> decode simpleBody of
      Just (Success a) -> return a
      _ -> liftIO $ assertFailure $ "cannot decode JSON: " ++ unpack simpleBody
 -}

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)

=======
runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)


>>>>>>> 99263dae65b86dda80a57b9f5a63bcbd6092e9c2
-- | Spec runner that sets up a test environment with DB.
withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    setEnv "JWT_SECRET" "test"
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        useEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask
    print tables
    let escapedTables = map (\name->"\""++name++"\"") tables
    let query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
    rawExecute query []

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do

    tables <- rawSql "SELECT table_name FROM information_schema.tables WHERE table_schema='public' AND table_type='BASE TABLE';"[]

    return $ map unSingle tables
