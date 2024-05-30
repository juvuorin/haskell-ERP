{-# LANGUAGE CPP             ***REMOVED***-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards ***REMOVED***-}
{-# LANGUAGE TemplateHaskell ***REMOVED***-}
{-# LANGUAGE DeriveGeneric***REMOVED***-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod
import qualified Control.Exception as Exception
import Data.Aeson                  (Result (..), fromJSON, withObject, (.!=),
                                    (.:?))
import Data.FileEmbed              (embedFile)
import Data.Yaml                   (decodeEither')
--import Database.Persist.Sqlite     (SqliteConf)
import Database.Persist.Postgresql (PostgresConf)
import Language.Haskell.TH.Syntax  (Exp, Name, Q)
import Network.Wai.Handler.Warp    (HostPreference)
import Yesod.Default.Config2       (applyEnvValue, configSettingsYml)
import Yesod.Default.Util          (WidgetFileSettings, widgetFileNoReload,
                                    widgetFileReload)
import Types
import qualified Data.Map
import Model (Entry, AccountId, Account)
import Database.Persist.Sql (toSqlKey)

import qualified Control.Concurrent        as CC
import qualified Control.Concurrent.STM    as T
import qualified Control.Monad             as CM


type AccountMapMutable = T.TVar (Map DefaultAccountType (Entity Account))


data AccountConstant = AccountConstant 
                     {grossPay::AccountId, --6000
                      taxPrepaymentDebt::AccountId,
                      healthInsuranceDebt::AccountId, --2362
                      healthInsuranceExpense::AccountId,--6003
                      shareholderDebt::AccountId, -- 2381
                      pensionInsuranceExpense::AccountId, --6006
                      employerUnemploymentExpense::AccountId, --6008  
                      bankingFees::AccountId, -- 6940 
                      shortTermPayable::AccountId, --2330   
                      shortTermReceivable::AccountId, --1500
                      accruedExpenses::AccountId,
                      interestIncome::AccountId --7280
                      }
  deriving (Eq, Ord)

predefinedAccounts = AccountConstant {
              grossPay=toSqlKey 6000,
              healthInsuranceDebt=toSqlKey 2362,
              healthInsuranceExpense=toSqlKey 6003,
              shareholderDebt=toSqlKey 2381,
              taxPrepaymentDebt=toSqlKey 2361,
              pensionInsuranceExpense=toSqlKey 6006,
              employerUnemploymentExpense=toSqlKey 6008,  
              bankingFees=toSqlKey 6940 ,
              shortTermPayable=toSqlKey 2330,   
              shortTermReceivable=toSqlKey 1500,
              accruedExpenses=toSqlKey 2350,
              interestIncome=toSqlKey 7280
              }



-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appStaticDir              :: String
    -- ^ Directory from which to serve static files.
    --, appDatabaseConf           :: SqliteConf
    , appDatabaseConf           :: PostgresConf
    -- ^ Configuration settings for accessing the database.
    , appRoot                   :: Maybe Text
    -- ^ Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.
    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , appReloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , appMutableStatic          :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining

    -- Example app-specific configuration values.
    , appCopyright              :: Text
    -- ^ Copyright text to appear in the footer of the page
    , appAnalytics              :: Maybe Text
    -- ^ Google Analytics code
--    , appAuthDummyLogin         :: Bool
    -- ^ Indicate if auth dummy login should be enabled.
    , appJwtSecret              :: Text

    -- ^ Banking client id
    , appClientId :: ClientId

    , appXApiKey :: XApiKey

    -- ^ Banking token endpoint
    , appTokenEndpoint::TokenEndpoint

    -- ^ Banking authorization endpoint
    , appAuthEndpoint::AuthEndpoint

    -- ^ Redirect URI for redirects
    , appRedirectUriAuth::RedirectUriAuth

    -- ^ Banking consents endpoint
    , appAccountAccessConsentsEndpoint::AccountAccessConsentsEndpoint

    -- ^ Banking API endoint for accessing account data
    , appApiEndpoint::ApiEndpoint

    , appPredefinedAccounts::AccountConstant
    
    , appMySetting :: String

    , appMmaventa_client_id :: String
    , appMaventa_client_secret :: String 
    , appMaventa_vendor_api_key :: String


    
    
    }


{- type AccountCode = Int
predefinedAccounts :: Map AccountConstant AccountCode
predefinedAccounts=Data.Map.fromList 
             [(GrossPayment, 6000),
              (HealthInsuranceDebt,2362),
              (HealthInsuranceExpense,6003),
              (ShareholderDebt,2381),
              (TaxPrepaymentDebt,2361),
              (PensionInsuranceExpense,6006),
              (EmployerUnemploymentExpense,6008),  
              (BankingFees,6940) ,
              (ShortTermPayable,2330),   
              (ShortTermReceivable,1500),
              (AccruedExpenses,2350)]
 -}

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#ifdef DEVELOPMENT
                True
#else
                False
#endif
        appStaticDir              <- o .: "static-dir"
        appDatabaseConf           <- o .: "database"
        appRoot                   <- o .:? "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        dev                       <- o .:? "development"      .!= defaultDev

        appDetailedRequestLogging <- o .:? "detailed-logging" .!= dev
        appShouldLogAll           <- o .:? "should-log-all"   .!= dev
        appReloadTemplates        <- o .:? "reload-templates" .!= dev
        appMutableStatic          <- o .:? "mutable-static"   .!= dev
        appSkipCombining          <- o .:? "skip-combining"   .!= dev

        appCopyright              <- o .:  "copyright"
        appAnalytics              <- o .:? "analytics"
        appJwtSecret              <- o .:  "jwt-secret"
 
        appClientId               <- fromString <$> o .:  "clientId"
        appXApiKey                <- fromString <$> o .:  "xApiKey"
        appTokenEndpoint          <- fromString <$> o .:  "tokenEndpoint"
        appAuthEndpoint           <- fromString <$> o .:  "authEndpoint"
        appRedirectUriAuth        <- fromString <$> o .:  "redirectUriAuth"
        appAccountAccessConsentsEndpoint <- fromString <$> o .:  "accountAccessConsentsEndpoint"
        appApiEndpoint            <- fromString <$> o .:  "apiEndpoint" 
        appMaventa_client_id <- fromString <$> o .: "maventa_client_id" 
        appMaventa_client_secret <- fromString <$> o .: "maventa_client_secret"
        appMaventa_vendor_api_key <- fromString <$> o .: "maventa_vendor_api_key"


        let appPredefinedAccounts = predefinedAccounts
        let appMySetting = "moro"



{-         authorityCompanyId        <- o .:  "authorityCompanyId"
        authorityCustomerId       <- o .:  "authorityCustomerId"
        authorityCustomerName     <- o .:  "authorityCustomerName"
        authorityTransferId       <- o .:  "authorityTransferId"
        authorityTransferPassword <- o .:  "authorityTransferPassword"
 -}     return AppSettings {..}

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if appReloadTemplates compileTimeAppSettings
                then widgetFileReload
                else widgetFileNoReload)
              widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either Exception.throw id
                       $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings
