{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
--{-# LANGUAGE DeriveDataTypeable #-}

--{-# LANGUAGE QuasiQuotes #-}
-- {---# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
--{-# OPTIONS_GHC -fno-warn-orphans #-}
--{-# OPTIONS_GHC -fno-warn-orphans #-}
--{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Application
  ( getApplicationDev,
    appMain,
    develMain,
    makeFoundation,
    makeLogWare,

    -- * for DevelMain
    getApplicationRepl,
    shutdownApp,

    -- * for GHCI
    handler,
    db,
  ) where

-------------------------------
--  Haskell stack with ghc version 8.10.7 LTS lts-18.19  

-------------------------------
--  libxml2-dev

-- must install libxml2-dev version
-- "sudo apt-get install libxml2-dev" (poppler may be needed, too???)
-- "sudo apt-get install libgd-dev"
-- "sudo apt-get install libssl-dev"
-- "sudo apt-get install libpq-dev"
-- use "stack build --extra-include-dirs=/usr/include/libxml2" fo building the app (may need to copy a c++ header file
-- to the right directory "libxml2.h" )

-----------------------------------
-- sudo apt-get install libssl-dev
--
--


-------------------------------
--  Set linux timezone

-- Change default local time zone according to your location like: "sudo timedatectl set-timezone Europe/Helsinki"

-------------------------------
--  xsdtohaskell

-- Use xsdtohaskell sw to transform dtd definition to haskell data types (HaXml command line utility), the tools need to be downloaded
-- https://hackage.haskell.org/package/HaXml
-- add "- HaXml" to package.yaml file "- HaXml" (this will download package when project is build)
-- 1) run in ghci with stack ghci xsdToHaskell
-- 2) "xsdToHaskell :set args "hakemisto_mistä_löytyy xsd" "output filename" ja sitten "main" (käsittelee kaikki xsd-päätteiset)
-------------------------------
--  LINUX OS

-- Distributor ID: Ubuntu
-- Description:    Ubuntu 18.04.5 LTS
-- Release:        18.04
-- Codename:       bionic
-------------------------------
--  (PostgreSQL) 10.19

-- psql (PostgreSQL) 10.19 (Ubuntu 10.19-0ubuntu0.18.04.1)
-- sudo apt update
-- sudo apt install postgresql postgresql-contrib
-- Ensure that the server is running using the systemctl start command:
-- sudo systemctl start postgresql.service

-------------------------------
--  NGINX nginx version: nginx/1.14.0 (Ubuntu)

-- sudo apt update
-- sudo apt install nginx
-- start and stop nginx "sudo systemctl start nginx" "sudo systemctl stop nginx"  
-- conf in "/etc/nginx/nginx.conf"
-- need to create key and certificate for nginx https  (see Servec section in conf file and also see certificate and key for conf file)

-------------------------------
--  wkhtmltopdf 0.12.6

-- Need to install wkhtmltopdf: 
-- wget https://github.com/wkhtmltopdf/packaging/releases/download/0.12.6-1/wkhtmltox_0.12.6-1.bionic_amd64.deb
-- sudo apt install ./wkhtmltox_0.12.6-1.bionic_amd64.deb

-------------------------------
-- puppeteer  ^14.2.0

-- Need to install nodejs v14.17.6
-- Need to install npm 6.14.15

-------------------------------
-- Debugging 
-- :l DevelMain
-- DevelMain.update



import Control.Monad.Logger (liftLoc, runLoggingT,runNoLoggingT)
import Data.List ( (!!), head )
import Database.Persist.Postgresql
  ( createPostgresqlPool,
    pgConnStr,
    pgPoolSize,
    runSqlPool,
  )
import Database.Persist.Sql (ConnectionPool, Single, fromSqlKey, rawQuery, rawSql, runSqlPool, toSqlKey)
import Handler.BankStatement.BankStatement
import Handler.BankStatement.BankStatements
import Handler.Banking.Authenticate
import Handler.Bookkeeping.CrudEndpoints.Account
import Handler.Bookkeeping.CrudEndpoints.Accounts
import Handler.Bookkeeping.CrudEndpoints.Transaction
import Handler.Bookkeeping.CrudEndpoints.Transactions
import Handler.Bookkeeping.CrudEndpoints.Chart

import Handler.Bookkeeping.InternalReporting.BalanceRecalc
import Handler.Bookkeeping.InternalReporting.BalanceSheet
import Handler.Bookkeeping.InternalReporting.GeneralLedger
import Handler.Bookkeeping.InternalReporting.IncomeStatement
import Handler.Bookkeeping.InternalReporting.Journal
import Handler.Bookkeeping.InternalReporting.ReportTrees (hierarchy {- , createTree -})
import Handler.Bookkeeping.InternalReporting.VatReport
import Handler.Certificates.Cert
import Handler.Company.Companies
import Handler.Company.Company
import Handler.Event.Event
import Handler.Files.Files
import Handler.Invoice.Purchase.CrudEndpoints.PurchaseInvoice
import Handler.Invoice.Purchase.CrudEndpoints.PurchaseInvoices
import Handler.Invoice.Sales.CrudEndpoints.SalesInvoice
import Handler.Invoice.Sales.CrudEndpoints.SalesInvoices
import Handler.Partner.Partner
import Handler.Partner.Partners
import Handler.Payroll.CrudEndpoints.Employee
import Handler.Payroll.CrudEndpoints.Employees
import Handler.Payroll.CrudEndpoints.Payadjustments
import Handler.Payroll.CrudEndpoints.Payadjustmenttypes
import Handler.Payroll.CrudEndpoints.Payevent
import Handler.Payroll.CrudEndpoints.Payevents
import Handler.Payroll.ExternalReporting.ReportToIR
import Handler.Users.User

import Import
import Language.Haskell.TH.Syntax (qLocation)
import Middlewares (allowCsrf, corsified)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
  ( Settings,
    defaultSettings,
    defaultShouldDisplayException,
    getPort,
    runSettings,
    setHost,
    setOnException,
    setPort,
  )
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger
  ( Destination (Logger),
    IPAddrSource (..),
    OutputFormat (..),
    destination,
    mkRequestLogger,
    outputFormat,
  )
import System.Log.FastLogger
  ( defaultBufSize,
    newStdoutLoggerSet,
    toLogStr,
  )
import System.Random
import Types (ConfigurationItem (AllSet), DefaultAccountType (DefaultAccountTypeInterestNonTax))
import Common (account)
import qualified Control.Concurrent        as CC
import qualified Control.Concurrent.STM    as T
import qualified Control.Monad             as CM
import Data.Map (fromList)
import Data.Set (fromList)
import ClassyPrelude (fromGregorian)
import Data.Data
import Text.Read
-- import Handler.Home
-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- type AccountMap = T.TMVar (Map DefaultAccountType (Entity Account)) 

createAccountMap :: Map DefaultAccountType (Entity Account) -> T.STM (T.TMVar (Map DefaultAccountType (Entity Account)))

--openAccount :: Float -> T.STM (Account)
--openAccount balance = T.newTMVar balance    
createAccountMap entities = T.newTMVar entities




{- insertHaskellEntity :: MonadIO m =>(a,b)-> ReaderT SqlBackend m AccessRight
insertHaskellEntity (a,b) = do
  insertEntity (toSqlKey 1) {- $ snd (Data.List.head r)) -} AccessRight {accessRightRight=a, accessRightDescription="description"} 
 -}
-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
  -- Some basic initializations: HTTP connection manager, logger, and static
  -- subsite.
    --openAccount :: Float -> T.STM (Account)

    --openAccount balance = T.newTMVar balance
  let appAccountMap = Data.Map.fromList [
                                      (DefaultAccountTypeInterestNonTax, 7270),
                                      (DefaultAccountTypeInterest, 7280)]


  appHttpManager <- getGlobalManager
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  appStatic <-
    (if appMutableStatic appSettings then staticDevel else static)
      (appStaticDir appSettings)

  -- We need a log function to create a connection pool. We need a connection
  -- pool to create our foundation. And we need our foundation to get a
  -- logging function. To get out of this loop, we initially create a
  -- temporary foundation without a real connection pool, get a log function
  -- from there, and then create the real foundation.
  let mkFoundation appConnPool = App {..}
      -- The App {..} syntax is an example of record wild cards. For more
      -- information, see:
      -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
      tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
      logFunc = messageLoggerSource tempFoundation appLogger

  -- Create the database connection pool
  {-     pool <- flip runLoggingT logFunc $ createSqlitePool
          (sqlDatabase $ appDatabaseConf appSettings)
          (sqlPoolSize $ appDatabaseConf appSettings)
   -}
  pool <-
    runNoLoggingT $
      createPostgresqlPool
        (pgConnStr $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)


{-   pool <-
    flip runLoggingT logFunc $
      createPostgresqlPool
        (pgConnStr $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)
 -}
  -- Perform database migration using our application's logging settings.
  runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc
  flip runSqlPool pool $ do
    --let doc1= defTransaction {transactionMemo = Just "Azure kulut", transactionCompanyId = toSqlKey }


{-     let d1 = fromGregorian 2023 9 1
    let d2 = fromGregorian 2023 10 1
    let d3 = fromGregorian 2023 11 1
    let d4 = fromGregorian 2023 12 1

    let doc1= defTransaction {transactionMemo = Just "Azure kulut", transactionCompanyId=toSqlKey 1}
    let doc2= defTransaction {transactionMemo = Just "Puhelin",transactionCompanyId=toSqlKey 1}
    let doc3= defTransaction {transactionMemo = Just "Zoom",transactionCompanyId=toSqlKey 1}

    mapM_ (\day-> do
                      _ <-insert doc1 {transactionDate=day}
                      _ <-insert doc2 {transactionDate=day}
                      _ <-insert doc3 {transactionDate=day}
                      return ()       
          ) [d1,d2,d3,d4]

 

 -}
--    accounts <- selectList [DefaultAccountDefaultAccountType !=. Nothing][]  
 {-    let companyId = toSqlKey 5
    accounts <- selectList [AccountDefaultAccountType ==. Just DefaultAccountTypeInterestNonTax][]  
    case accounts of
      (x:xs) -> do        
        print "DefaultAccountTypeInterestNonTax already added"
      _ -> do
        value <- selectFirst [AccountCompanyId ==. companyId,AccountCode ==. 7270][]
        case value of
          Just x -> update (entityKey x) [AccountDefaultAccountType =. Just DefaultAccountTypeInterestNonTax]
          Nothing -> print "Account with code 7280 not found"
 
    accounts <- selectList [AccountDefaultAccountType ==. Just DefaultAccountTypeInterestNonTax][]  
    case accounts of
      (x:xs) -> do
          let typeToAccountMap = Data.Map.fromList [(DefaultAccountTypeInterestNonTax, x)] 
          let accountMap = T.atomically (createAccountMap typeToAccountMap)        
          --T.putTMVar accountMap (appAccountMap appSettings)
          print "DefaultAccountTypeInterestNonTax successsfully added to STM"        
      _ -> print "Accounts with default type do not exist"
   

 -}

    info <- selectFirst [] []



{-     allAccounts <- selectList [AccountCompanyId ==. toSqlKey 1][]
    mapM_ (\account-> do
        let account = entityVal account
        let code = accountCode account
        case code of
          if(code>0 && code<1000) then EquityAccount 
          else if (code>999 && code<200) LiabilityAccount          

      ) allAccounts
 -}
    --colorStrings :: [String]
--    AccessRight json
--    right               AccessRightType
--    description         Text

{-     deleteWhere ([] :: [Filter UserRole])
    deleteWhere ([] :: [Filter AccessRightRole])

    let values = zip [1..] $ map (read . show) $ dataTypeConstrs (dataTypeOf VerifyPurchaseInvoice :: DataType) 
    deleteWhere ([] :: [Filter AccessRight])
    mapM (\(index,value)->insertKey (toSqlKey index) (AccessRight value "description") ) values 

    let values = zip [1..] $ map (read . show) $ dataTypeConstrs (dataTypeOf Admin :: DataType) 
    deleteWhere ([] :: [Filter Role])
    mapM (\(index,value)->insertKey (toSqlKey index) (Role value "description") ) values 

    key <- insert (UserRole (toSqlKey 7) (toSqlKey 1))
    key2 <- insert $ AccessRightRole (toSqlKey 1) (toSqlKey 1) (toSqlKey 1) 

 -}

    case info of
      Just (Entity _ x) ->
        if configurationInfoConfigurationitem x == AllSet
          then print "System already AllSet configured!"
          else print "System already configured, but not AllSet"
      Nothing -> do
        -- Attach payer summary property to the property set
--        result <- insertUnique savaDebtProperty
  --      case result of
  --        Just x -> print "PayerSummaryAccount property successfully added!"
  --        Nothing -> print "PayerSummaryAccount property is already added!"

        -- Add VAT types
{-         let vatTypePurchaseItems = map (\i -> (i, listPurchaseVat !! (i -1))) [1 .. (length listPurchaseVat)]
        mapM_ (\(key, value) -> insertKey (toSqlKey $ fromIntegral key) value) vatTypePurchaseItems


        -- Add VAT types
        let vatTypeSaleItems = map (\i -> (i, listSaleVat !! (i -1))) [1 .. (length listSaleVat)]
        mapM_ (\(key, value) -> insertKey (toSqlKey $ fromIntegral key) value) vatTypeSaleItems
 -}


        --  Add VAT percentages
        --let entities = map (\i -> (i, listVatPct !! (i -1))) [1 .. (length listVatPct)]
        --mapM_ (\(key, value) -> insertKey (toSqlKey $ fromIntegral key) (value)) entities

        -- Add report hierarchies
--        mapM_ (\x -> insertKey (entityKey x) (entityVal x)) hierarchy
--        print "Report hierarchies inserted successfully!"
--        _ <- insert (ConfigurationInfo AllSet)
        print "ConfigurationInfo AllSet successfully added!"



        --let accountMap = appAccountMap appSettings
        return ()






--  insertDefaultProperties pool
  return $ mkFoundation pool

--insertDefaultProperties :: ConnectionPool -> IO ()
--insertDefaultProperties pool = do
  -- These properties are for the test company with companyId 5
--  let savaDebtProperty =
--        AccountProperty
--          { accountPropertyProperty = HealthInsurancePayerSummary,
--            accountPropertyValue = Nothing,
--            accountPropertyAccountId = toSqlKey 367
--          }

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- | applying some additional middlewares.

makeApplication :: App -> IO Application
makeApplication foundation = do
  logWare <- makeLogWare foundation
  -- Create the WAI application and apply middlewares
  appPlain <- toWaiAppPlain foundation
  return $ logWare $ defaultMiddlewaresNoLogging $ {- simpleCors -} {- allowCsrf  -} corsified appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
  mkRequestLogger
    def
      { outputFormat =
          if appDetailedRequestLogging $ appSettings foundation
            then Detailed True
            else
              Apache
                ( if appIpFromHeader $ appSettings foundation
                    then FromFallback
                    else FromSocket
                ),
        destination = Logger $ loggerSet $ appLogger foundation
      }

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
  setPort (appPort $ appSettings foundation) $
    setHost (appHost $ appSettings foundation) $
      setOnException
        ( \_req e ->
            when (defaultShouldDisplayException e) $
              messageLoggerSource
                foundation
                (appLogger foundation)
                $(qLocation >>= liftLoc)
                "yesod"
                LevelError
                (toLogStr $ "Exception from Warp: " ++ show e)
        )
        defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings $ warpSettings foundation
  app <- makeApplication foundation
  return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
  -- Get the settings from all relevant sources
  settings <-
    loadYamlSettings
      -- fall back to compile-time values, set to [] to require values at runtime
      ["config/settings.yml"]
      []
      -- allow environment variables to override
      useEnv

  -- Generate the foundation from the settings
  foundation <- makeFoundation settings

  -- Generate a WAI Application from the foundation
  app <- makeApplication foundation

  -- Run the application with Warp
  runSettings (warpSettings foundation) app

--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings $ warpSettings foundation
  app1 <- makeApplication foundation
  return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()

---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: DB a -> IO a
db = handler . runDB
