{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}

module Common where

import qualified ClassyPrelude as Data.Map
--import Prelude hiding ((++))
-- import Import.NoFoundation hiding (map, filter)

-- (getAccountEntities)
--import Import

--import Prelude hiding ((++))
-- import Import.NoFoundation hiding (map, filter)

-- (getAccountEntities)
--import Import
--import Prelude hiding ((++))
-- import Import.NoFoundation hiding (map, filter)

-- (getAccountEntities)
--import Import

--import Prelude hiding ((++))
-- import Import.NoFoundation hiding (map, filter)

-- (getAccountEntities)
--import Import
--import Prelude hiding ((++))
-- import Import.NoFoundation hiding (map, filter)

-- (getAccountEntities)
--import Import

--import Prelude hiding ((++))
-- import Import.NoFoundation hiding (map, filter)

-- (getAccountEntities)
--import Import
--import Prelude hiding ((++))
-- import Import.NoFoundation hiding (map, filter)

-- (getAccountEntities)
--import Import

--import Prelude hiding ((++))
-- import Import.NoFoundation hiding (map, filter)

-- (getAccountEntities)
--import Import
import ClassyPrelude.Yesod hiding (Update, lookup, sortBy)
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Types
import Data.Fixed
import qualified Data.HashMap.Strict as HM
import Data.List (deleteFirstsBy, foldl, head, intersectBy, lookup, nub, sortBy, unionBy, (!!), iterate)
import Data.Text ()
import Data.Time
  ( TimeOfDay (TimeOfDay),
    diffDays,
    gregorianMonthLength,
    timeOfDayToTime,
  )
import qualified Data.Vector as V
import Database.Persist.Postgresql (fromSqlKey)
import Database.Persist.Sql (ConnectionPool, fromSqlKey, runSqlPool, toSqlKey, Update, SqlWriteBackend)
import Foundation
import GHC.Exts
import qualified GHC.Generics
import Import.NoFoundation hiding (Update, filter, lookup, map, mapM_, null, print, sortBy, zip, (.))
import System.Directory (removeDirectoryRecursive, removeFile)
import System.Random
import Text.Casing (camel)
import Types
  ( Code,
    DefaultAccountType (DefaultAccountTypeInterestNonTax),
    TransactionType (TypePurchaseInvoice, TypeSalesInvoice),
    round', currentDay, DocumentStatus,
  )
import Data.Map (elems)
import Network.Wai
import Prelude (read)
import Data.Time.Calendar
import Database.Persist.Compatible (Compatible)
import Data.Kind (Type)
import qualified GHC.OverloadedLabels


now :: IO Day
now = liftIO currentDay

instance SymbolToField "company_id" AccessRightRole CompanyId where symbolToField :: EntityField AccessRightRole CompanyId
                                                                    symbolToField = AccessRightRoleCompanyId
instance SymbolToField "company_id" Account CompanyId where symbolToField = AccountCompanyId
instance SymbolToField "company_id" AccountingYear CompanyId where symbolToField = AccountingYearCompanyId
instance SymbolToField "company_id" BankStatement CompanyId where symbolToField = BankStatementCompanyId
instance SymbolToField "company_id" Transaction CompanyId where symbolToField = TransactionCompanyId
instance SymbolToField "company_id" Employee CompanyId where symbolToField = EmployeeCompanyId
instance SymbolToField "company_id" Employment CompanyId where symbolToField = EmploymentCompanyId
instance SymbolToField "company_id" FiscalYear CompanyId where symbolToField = FiscalYearCompanyId
instance SymbolToField "company_id" MonthlyBalance CompanyId where symbolToField = MonthlyBalanceCompanyId
instance SymbolToField "company_id" Notification CompanyId where symbolToField = NotificationCompanyId
instance SymbolToField "company_id" Payevent CompanyId where symbolToField = PayeventCompanyId
instance SymbolToField "company_id" Product CompanyId where symbolToField = ProductCompanyId
instance SymbolToField "company_id" PurchaseInvoice CompanyId where symbolToField = PurchaseInvoiceCompanyId
instance SymbolToField "company_id" SalesInvoice CompanyId where symbolToField = SalesInvoiceCompanyId
instance SymbolToField "company_id" SecurityInfo CompanyId where symbolToField = SecurityInfoCompanyId
instance SymbolToField "company_id" UserCompany CompanyId where symbolToField = UserCompanyCompanyId
instance SymbolToField "company_id" UserRole CompanyId where symbolToField = UserRoleCompanyId
instance SymbolToField "company_id" VatReport CompanyId where symbolToField = VatReportCompanyId
instance SymbolToField "company_id" ProcessedVatReport CompanyId where symbolToField = ProcessedVatReportCompanyId



{- SymbolToField "status" record DocumentStatus,
instance GHC.OverloadedLabels.IsLabel "status" (record -> DocumentStatus),
    fromLabel = partnerInfoStatus 
 -}


instance SymbolToField "document_status" PurchaseInvoice DocumentStatus where symbolToField = PurchaseInvoiceDocumentStatus

--instance SymbolToField "status" PartnerInfo DocumentStatus where symbolToField = PartnerInfoStatus

instance GHC.OverloadedLabels.IsLabel "document_status" (PurchaseInvoice -> DocumentStatus) where
    fromLabel = purchaseInvoiceDocumentStatus
instance GHC.OverloadedLabels.IsLabel "document_status" (PartnerInfo -> DocumentStatus) where 
    fromLabel = partnerInfoStatus 

instance GHC.OverloadedLabels.IsLabel "id" (Entity record -> Key record) where 
    fromLabel = entityKey



instance GHC.OverloadedLabels.IsLabel "company_id" (AccessRightRole -> Key Company) where 
    fromLabel = accessRightRoleCompanyId 

instance GHC.OverloadedLabels.IsLabel "company_id" (Account -> Key Company) where 
    fromLabel = accountCompanyId 

instance GHC.OverloadedLabels.IsLabel "company_id" (AccountingYear -> Key Company) where 
    fromLabel = accountingYearCompanyId 

instance GHC.OverloadedLabels.IsLabel "company_id" (BankStatement -> Key Company) where 
    fromLabel = bankStatementCompanyId 

instance GHC.OverloadedLabels.IsLabel "company_id" (Transaction -> Key Company) where 
    fromLabel = transactionCompanyId 

instance GHC.OverloadedLabels.IsLabel "company_id" (Employee -> Key Company) where 
    fromLabel = employeeCompanyId 

instance GHC.OverloadedLabels.IsLabel "company_id" (Employment -> Key Company) where 
    fromLabel = employmentCompanyId 

instance GHC.OverloadedLabels.IsLabel "company_id" (FiscalYear -> Key Company) where 
    fromLabel = fiscalYearCompanyId 

instance GHC.OverloadedLabels.IsLabel "company_id" (MonthlyBalance -> Key Company) where 
    fromLabel = monthlyBalanceCompanyId 

instance GHC.OverloadedLabels.IsLabel "company_id" (Notification -> Key Company) where 
    fromLabel = notificationCompanyId 

instance GHC.OverloadedLabels.IsLabel "company_id" (Payevent -> Key Company) where 
    fromLabel = payeventCompanyId 

instance GHC.OverloadedLabels.IsLabel "company_id" (Product -> Key Company) where 
    fromLabel = productCompanyId 

instance GHC.OverloadedLabels.IsLabel "company_id" (PurchaseInvoice -> Key Company) where 
    fromLabel = purchaseInvoiceCompanyId 

instance GHC.OverloadedLabels.IsLabel "company_id" (SalesInvoice -> Key Company) where 
    fromLabel = salesInvoiceCompanyId 

instance GHC.OverloadedLabels.IsLabel "company_id" (SecurityInfo -> Key Company) where 
    fromLabel = securityInfoCompanyId 

instance GHC.OverloadedLabels.IsLabel "company_id" (UserCompany -> Key Company) where 
    fromLabel = userCompanyCompanyId 

instance GHC.OverloadedLabels.IsLabel "company_id" (UserRole -> Key Company) where 
    fromLabel = userRoleCompanyId 

instance GHC.OverloadedLabels.IsLabel "company_id" (VatReport -> Key Company) where 
    fromLabel = vatReportCompanyId 

instance GHC.OverloadedLabels.IsLabel "company_id" (ProcessedVatReport -> Key Company) where 
    fromLabel = processedVatReportCompanyId 



hasAccess :: ReaderT SqlBackend Handler CompanyId 
hasAccess = do
  companyId <- liftHandler $ getCompanyId
  user <- liftHandler $ getAuthenticatedUser  
  access <- exists [UserCompanyUserId ==. entityKey user, UserCompanyCompanyId ==. companyId]
  if access then return companyId else sendResponseStatus status403 ("The user has no access to this company" :: Text)


test3 :: ReaderT SqlBackend Handler ()
test3 = do
  
  i <- selectList' [][] :: ReaderT SqlBackend Handler [Entity Transaction]
  --_ <- delete' i 
  ent <- case i of
    (x:xs) -> do
        --e<-getEntity' (entityKey x)
        --case e of
          --Just x -> do
            _ <- delete' (entityKey x)
            update' (entityKey x) [TransactionType =. TypeSalesInvoice]
    _ -> sendResponseStatus status404 ("The record does not exist" :: Text)
  return ()

selectList' :: (PersistQueryRead SqlBackend, PersistRecordBackend record SqlBackend, SymbolToField "company_id" record (Key Company))
    =>  [Filter record] 
    -> [SelectOpt record]
    -> ReaderT SqlBackend Handler [Entity record]

selectList' filters options = do
    companyId <- hasAccess
    selectList ((#company_id ==. companyId) : filters) options

selectFirst' :: (PersistQueryRead SqlBackend, PersistRecordBackend record SqlBackend, SymbolToField "company_id" record (Key Company))
    =>  [Filter record] 
    -> [SelectOpt record]
    -> ReaderT SqlBackend Handler (Maybe (Entity record))

selectFirst' filters options = do
    companyId <- hasAccess
    selectFirst ((#company_id ==. companyId) : filters) options

getEntity' :: (PersistQueryRead SqlBackend, PersistRecordBackend record SqlBackend, SymbolToField "company_id" record (Key Company))
  => Key record -> ReaderT SqlBackend Handler (Maybe (Entity record))
getEntity' key = do
  getEntity key
    
getEntity404' :: (PersistQueryRead SqlBackend, PersistRecordBackend record SqlBackend, SymbolToField "company_id" record (Key Company))
  => Key record -> ReaderT SqlBackend Handler (Entity record)
getEntity404' key = do
  entity <- getEntity key
  case entity of
    Just x-> return x
    Nothing ->sendResponseStatus status404 ("Entity cannot be found"::Text)

get404' :: (PersistQueryRead SqlBackend, PersistRecordBackend record SqlBackend, SymbolToField "company_id" record (Key Company))
  => Key record -> ReaderT SqlBackend Handler record
get404' key = do
  get404 key

insertEntity' :: (PersistQueryRead SqlBackend, PersistRecordBackend record SqlBackend, GHC.OverloadedLabels.IsLabel "company_id" (record -> Key Company), SymbolToField "company_id" record (Key Company))
  => record -> ReaderT SqlBackend Handler (Entity record)
insertEntity' record = do
  companyId <- hasAccess
  if #company_id record == companyId then 
    insertEntity record        
  else sendResponseStatus status404 ("The record does not belong to the company in context" :: Text)


delete' :: (PersistQueryRead SqlBackend, PersistRecordBackend record SqlBackend, GHC.OverloadedLabels.IsLabel "company_id" (record -> Key Company), SymbolToField "company_id" record (Key Company))
  => Key record -> ReaderT SqlBackend Handler ()
delete' key = do
  companyId <- hasAccess
  r <- get404' key
  if #company_id r == companyId then 
    delete key        
  else sendResponseStatus status404 ("The record does not belong to the company in context" :: Text)

update' :: (PersistQueryRead SqlBackend, PersistRecordBackend record SqlBackend,GHC.OverloadedLabels.IsLabel "company_id" (record -> Key Company), SymbolToField "company_id" record (Key Company)) 
  => Key record 
  -> [Update record] 
  -> ReaderT SqlBackend Handler ()
update' key updates = do
  companyId <- hasAccess
  r <- get404' key
  if #company_id r == companyId then 
      update key updates
  else sendResponseStatus status404 ("The record does not belong to the company in context" :: Text)


-- If the application has several clients lodging in one database we need to be able to access
-- the data securely over different tables -> need to add company_id to SQL queries




data HeterogenousListEntry
  = WithId (Entity Entry)
  | WithoutId Entry
  deriving (Generic, Show)

instance FromJSON HeterogenousListEntry where
  parseJSON = genericParseJSON defaultOptions {sumEncoding = UntaggedValue}

data NewOrExistingItem a = Existing (Entity a) | New a
  deriving (Generic)

instance (FromJSON a, FromJSON (Entity a)) => FromJSON (NewOrExistingItem a) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camel, sumEncoding = UntaggedValue}

data DocumentWithNewAndExistingItems a b = DocumentWithNewAndExistingItems
  { documentFromClient :: a,
    documentDetailsFromClient :: [NewOrExistingItem b]
  }
  deriving stock (Generic)

instance (FromJSON a, FromJSON (NewOrExistingItem b)) => FromJSON (DocumentWithNewAndExistingItems a b) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camel, sumEncoding = UntaggedValue}

class UpdateBatch a b where
  batchUpdate ::
    ( PersistEntity a,
      PersistRecordBackend b SqlBackend,
      PersistRecordBackend a SqlBackend
    ) =>Key Company->
    Key a ->
    (DocumentWithNewAndExistingItems a b) ->
    DB [Key b]

genericUpdate companyId documentId documentWithDetails = do
  let entriesFromClient = documentDetailsFromClient documentWithDetails
  let (entriesToInsert, entriesToUpdate) =
        foldl
          ( \acc item -> do
              case item of
                Existing (Entity key x) -> (fst acc, snd acc ++ [Entity key x])
                New x -> (fst acc ++ [x], snd acc)
          )
          ([], [])
          entriesFromClient

  replace documentId $ documentFromClient documentWithDetails
  mapM_ (\item -> replace (entityKey item) (entityVal item)) entriesToUpdate
  insertMany entriesToInsert

instance UpdateBatch Transaction Entry where
  batchUpdate companyId transactionId documentWithDetails = do
    let entriesFromClient = documentDetailsFromClient documentWithDetails
    --entriesFromClient' <- addproperties entriesFromClient
    let (entriesToInsert, entriesToUpdate) =
          foldl
            ( \acc item -> do
                case item of
                  Existing (Entity key x) -> (fst acc, snd acc ++ [Entity key x])
                  New x -> (fst acc ++ [x], snd acc)
            )
            ([], [])
            entriesFromClient

    let postedDoc = documentFromClient documentWithDetails
    storedDoc <- get404 transactionId
    -- Document needs to be updated
    replace transactionId postedDoc
    -- Update balances of new entries and insert entries
    ids <- insertEntries companyId entriesToInsert (transactionDate postedDoc)
    -- Update balances of existing entries and update entries
    updateEntries companyId entriesToUpdate (transactionDate postedDoc) (transactionDate storedDoc)

    return ids










-- Function to calculate the number of months between two dates
monthsBetween :: Day -> Day -> Int
monthsBetween startDate endDate =
  let startMonth = fromGregorian year month 1
      months = takeWhile (<= endDate) $ iterate (addGregorianMonthsClip 1) startMonth
  in length months
  where
    (year, month, _) = toGregorian startDate

data GenericAccount = DeferredExpenses  
                    | AdvancePayment
                    | AccrualsAndDeferredIncome
                    |PrepaymentsAndAccruedIncome


to :: GenericAccount -> Handler AccountId
to a = do
  id <- getCompanyId
  account <- do
      case a of
        DeferredExpenses -> return 1860  
  
        _ -> sendResponseStatus status404 ("Account not found!"::Text) 
  
  toId account id


updateParentAndInsertOrUpdateChildren ::
  ( PersistEntity a,
    PersistRecordBackend b SqlBackend,
    PersistRecordBackend a SqlBackend
  ) =>
  Key a ->
  (DocumentWithNewAndExistingItems a b) ->
  DB [Key b]
updateParentAndInsertOrUpdateChildren documentId documentWithDetails = do
  let entriesFromClient = documentDetailsFromClient documentWithDetails
  let (entriesToInsert, entriesToUpdate) =
        foldl 
          ( \acc item -> do
              case item of
                Existing (Entity key x) -> (fst acc, snd acc ++ [Entity key x])
                New x -> (fst acc ++ [x], snd acc)
          )
          ([], [])
          entriesFromClient

  replace documentId $ documentFromClient documentWithDetails
  
  mapM_ (\item -> replace (entityKey item) (entityVal item)) entriesToUpdate
  insertMany entriesToInsert

data PostResult = Ok | Error

class Postable p where
  post :: p -> Handler ()

data Account' index where
  IdAccount :: AccountId -> Account' AccountId
  CodeAccount :: Int -> Account' Int

-- A side-effect of using GADTs is that we need to use standalone deriving
-- for our instances.
deriving instance Show (Account' i)

deriving instance Eq (Account' i)

account :: Account' i -> Handler (Maybe Account)
account (IdAccount account) = do
  account <- runDB $ get account
  case account of
    Just x -> return (Just x)
    Nothing -> return Nothing
account (CodeAccount account) = do
  account <- runDB $ selectList [AccountCode ==. account] []
  case account of
    (x : []) -> return (Just (entityVal x))
    [] -> return Nothing

fromId :: AccountId -> CompanyId -> Handler Int
fromId accountId companyId = do
  account <- runDB $ selectList [AccountId ==. accountId, AccountCompanyId ==. companyId] []
  case account of
    (x : []) -> return $ accountCode $ entityVal x
    _ -> sendResponseStatus status404 ("Cannot find account code for account id " ++ (show accountId))

toId :: Int -> CompanyId -> Handler AccountId
toId accountCode companyId = do
  account <- runDB $ selectList [AccountCode ==. accountCode, AccountCompanyId ==. companyId] []
  case account of
    (x : []) -> return $ entityKey x
    _ -> sendResponseStatus status404 ("Cannot find account id for account code" ++ (show accountCode))

toId'' :: Int -> CompanyId -> DB AccountId
toId'' accountCode companyId = do
  account <- selectList [AccountCode ==. accountCode, AccountCompanyId ==. companyId] []
  case account of
    (x : []) -> return $ entityKey x
    _ -> sendResponseStatus status404 ("Cannot find account id for account code" ++ (show accountCode))


fromId' :: AccountId -> CompanyId -> DB Int
fromId' accountId companyId = do
  account <- selectList [AccountId ==. accountId, AccountCompanyId ==. companyId] []
  case account of
    (x : []) -> return $ accountCode $ entityVal x
    _ -> sendResponseStatus status404 ("Cannot find account code for account id " ++ (show accountId))

accountCodeFromAccountId :: AccountId -> Handler Int
accountCodeFromAccountId accountId = do
  account <- runDB $ selectList [AccountId ==. accountId] []
  case account of
    (x : []) -> return $ accountCode $ entityVal x
    _ -> sendResponseStatus status404 ("Cannot find account code for account id " ++ (show accountId))

toId' :: Int -> CompanyId -> DB AccountId
toId' accountCode companyId = do
  account <- selectList [AccountCode ==. accountCode, AccountCompanyId ==. companyId] []
  case account of
    (x : []) -> return $ entityKey x
    _ -> sendResponseStatus status404 ("Cannot find account id for account code" ++ (show accountCode))

transformEntryAccountCodeToAccountId accounts entries' = do
  map
    ( \item -> do
        let accountId = getIdByCode (fromIntegral (fromSqlKey (entryAccountId item))) accounts
        item {entryAccountId = accountId}
    )
    entries'

newtype CachedAccountList list = CachedAccountList {unCachedAccountList :: list}
  deriving (Typeable)

getAccountMap :: Handler (Map DefaultAccountType Int)
getAccountMap = do
  app <- getYesod
  return (appAccountMap app)

getAccountSet :: Handler (Set Int)
getAccountSet = do
  app <- getYesod
  return (appAccountSet app)

getDefaultAccountIdByEnum :: DefaultAccountType -> Handler AccountId
getDefaultAccountIdByEnum defaultAccountType = do
  accountMap <- getAccountMap
  let accountCode = Data.Map.lookup defaultAccountType accountMap
  case accountCode of
    Nothing -> sendResponseStatus status404 (pack (show defaultAccountType ++ " AccountCode not set") :: Text)
    Just x -> do
      account <- runDB $ selectList [{- AccountCompanyId==.companyId,  -} AccountCode ==. x] []
      case account of
        (x : xs) -> return (entityKey x)
        _ -> sendResponseStatus status404 (pack (show defaultAccountType ++ "not found") :: Text)

accountDeletableOrError :: AccountId -> Handler Bool
accountDeletableOrError accountId = do
  accountMap <- getAccountMap
  accountCode <- accountCodeFromAccountId accountId
  let accountMapElems = elems accountMap
  let fixed = accountCode `elem` accountMapElems
  case fixed of
    False -> sendResponseStatus status404 ("Account cannot be deleted as it has default action set to it"::Text) 
    True -> return True  

accountInterestNonTax :: Handler AccountId
accountInterestNonTax = getDefaultAccountIdByEnum DefaultAccountTypeInterestNonTax

daysInYear = 365

calculateInterest :: Double -> Double -> Int -> Double
calculateInterest rate capital days = capital * rate * (fromIntegral days) / daysInYear

newtype StartDay = StartDay {unStartDay :: Day}
newtype EndDay = EndDay {unEndDay :: Day}
newtype InterestRate = InterestRate {unInterestRate :: Double}
newtype Capital = Capital {unCapital :: Double}

calculateInterestDays :: InterestRate -> Capital -> StartDay -> EndDay -> Double
calculateInterestDays rate capital startDay endDay =
  calculateInterest (unInterestRate rate) (unCapital capital) (fromIntegral (diffDays (unEndDay endDay) (unStartDay startDay)))

getAccountEntities_ :: CompanyId -> Handler [Entity Account]
getAccountEntities_ companyId = do
  runDB $ selectList [AccountCompanyId ==. companyId] []


getCompanyId :: Handler CompanyId
getCompanyId = do
    e <- getYesod
    let set = appSettings e
    req <- waiRequest
    let path = pathInfo req

    -- | Check if we need to check the company id in order to assess access rights
    case path of
      -- | If user is requesting a list of companies, companyId is not needed
      (company : companies : _)
        | company == "company" && companies == "companies" -> sendResponseStatus status404 ("Company id is not needed!"::Text)

      -- | If user is requesting company or filestore related services the companyId is needed
      (x : _)
        | x == "company" || x == "filestore" ->
          return $ toSqlKey $ (read $ unpack $ path !! 1 :: Int64)

      -- | If user is requesting payroll related services the companyId is needed
      (payroll : company : _)
        | payroll == "payroll" && company == "company" ->
          return $ toSqlKey (read $ unpack $ path !! 2 :: Int64)
      
      _ -> sendResponseStatus status404 ("Company id is not provided by the client!"::Text)
    
    

newtype CachedCompanyId id = CachedCompanyId {unCachedCompanyId :: id}
  deriving (Typeable)

getAccountEntities :: CompanyId -> Handler [Entity Account]
getAccountEntities companyId = unCachedAccountList <$> cached (CachedAccountList <$> getAccountEntities_ companyId)

getAccountEntities' :: CompanyId -> DB [Entity Account]
getAccountEntities' companyId = unCachedAccountList <$> cached (CachedAccountList <$> getAccountEntities' companyId)

getDefaultAccountEntities_ :: CompanyId -> Handler [Entity Account]
getDefaultAccountEntities_ companyId = do
  runDB $ selectList [AccountDefaultAccountType !=. Nothing] []

getDefaultAccountTypeMap :: CompanyId -> Handler [Entity Account]
getDefaultAccountTypeMap companyId = unCachedAccountList <$> cached (CachedAccountList <$> getAccountEntities_ companyId)

addDocIds docId l =
  map
    ( \item -> do
        item {entryTransactionId = docId}
    )
    l

getIdByCode :: Code -> [Entity Account] -> Key Account
getIdByCode code accounts = do
  let filteredAccount = filter (\account -> (accountCode (entityVal account)) == code) accounts
  case filteredAccount of
    [x] -> entityKey x

logEvent :: String -> Maybe CompanyId -> ReaderT SqlBackend (HandlerFor App) ()
logEvent message companyId = do
  time <- liftIO $ getCurrentTime
  insert $ Event (pack message) time companyId
  return ()

logEvent' :: String -> Handler ()
logEvent' message = do
  time <- liftIO $ getCurrentTime
  runDB $ insert $ Event (pack message) time Nothing
  return ()

entryWithDocId :: Key Transaction -> Entry
entryWithDocId docId = defEntry {entryTransactionId = docId}

postMakeTestTransactionsAndEntries :: CompanyId -> Handler ()
postMakeTestTransactionsAndEntries companyId = do
  allAccounts <- liftHandler $ getAccountEntities (toSqlKey 5)

  docs' <- liftIO $
    forM [0 .. 999] $ \_ -> do
      randDay <- randomRIO (1 :: Int, 27)
      randType <- randomRIO (1 :: Int, 2)
      randMonth <- randomRIO (7 :: Int, 12)

      let day = fromGregorian 2022 randMonth randDay

      let type' = case randType of
            1 -> TypeSalesInvoice
            2 -> TypePurchaseInvoice
            _ -> TypeSalesInvoice

      let memo' = case randType of
            1 -> "Myyntilasku"
            2 -> "Ostolasku"
            _ -> "jotain vaan"

      let newDoc =
            defTransaction
              { transactionDate = (day),
                transactionMemo = (Just memo'),
                transactionCreatedautomatically = Nothing,
                transactionValid = Nothing,
                transactionCompanyId = (toSqlKey 1),
                transactionDebt = Nothing,
                transactionReceivable = Nothing,
                transactionSalary = Nothing,
                transactionPayable = Nothing,
                transactionAuto = Nothing,
                transactionType = type'
              }

      return newDoc
  let toAccountId code = Data.List.head $ map (\(Entity key item) -> key) $ filter (\(Entity key item) -> accountCode item == code) allAccounts

  entrySets' <- forM [0 .. 999] $ \index -> do
    z' <- randomRIO (20 :: Double, 5000)
    let z = round' z'
    let preparedEntry = entryWithDocId (toSqlKey 0)
    case transactionType (docs' !! index) of
      TypeSalesInvoice -> do
        return
          [ preparedEntry {entryAccountId = toAccountId 3010, entryAmount = - z},
            preparedEntry {entryAccountId = toAccountId 1500, entryAmount = round' $ z * 1.24},
            preparedEntry {entryAccountId = toAccountId 2366, entryAmount = round' (round' (- z * 1.24) + z)}
          ]
      TypePurchaseInvoice -> do
        return
          [ preparedEntry {entryAccountId = toAccountId 4010, entryAmount = z},
            preparedEntry {entryAccountId = toAccountId 2330, entryAmount = round' (- z * 1.24)},
            preparedEntry {entryAccountId = toAccountId 1538, entryAmount = round' ((z * 1.24) - z)}
          ]
      _ -> sendResponseStatus status404 ("Unknown transaction type" :: Text)

  print ("Phase 1 ok")
  runDB $ do
    ids <- insertMany docs'
    let tuples = zip ids entrySets'
    let newEntries' = join $ map (\(id, entrySet) -> map (\entry -> entry {entryTransactionId = id}) entrySet) tuples
    ids' <- insertMany newEntries'
    return ()

  return ()

sortByKey :: Ord (Key record) => [Entity record] -> [Entity record]
sortByKey entities = do
  let sorted = Data.List.sortBy (\a b -> compare (entityKey a) (entityKey b)) entities
  sorted

updateAccountBalancesForNewEntries ::CompanyId-> [Entry] -> Day -> DB ()
updateAccountBalancesForNewEntries companyId entriesToInsert' dateOfPostedTransaction = do
  let endDateOfPostedTransaction = getEndDate dateOfPostedTransaction
  let entriesToInsert = sortBy (\a b -> compare (entryAccountId a) (entryAccountId b)) entriesToInsert'

  -- 1. Create a list of unique accounts found in the incoming entries
  let accountIds = nub $ map entryAccountId entriesToInsert

  let groupedEntries = groupBy (\a b -> entryAccountId a == entryAccountId b) entriesToInsert
  let balancesOfAccountsNew = map (\item -> foldl (\acc entry -> (entryAccountId entry, snd acc + entryAmount entry)) (toSqlKey 0, 0) item) groupedEntries

  -- Get current balances from the MonthlyBalance table for each account
  currentBalances <- selectList ([MonthlyBalanceAccountId <-. accountIds, MonthlyBalanceDate ==. endDateOfPostedTransaction]) [Asc MonthlyBalanceAccountId]
  let balancesOfAccountsCurrent = map (\(Entity key value) -> (monthlyBalanceAccountId value, monthlyBalanceBalance value)) currentBalances

  toBeInsertedOrReplaced <-
    mapM
      ( \(account, balance) -> do
          let balance' = case lookup account balancesOfAccountsCurrent of
                Just oldBalance -> balance + oldBalance
                Nothing -> balance
          liftHandler $ monthlyBalanceEntity account endDateOfPostedTransaction balance'
      )
      balancesOfAccountsNew
  repsertMany $ map (\(Entity key val) -> (key, val)) toBeInsertedOrReplaced

  return ()


updateMonthlyBalancesForEntriesToBeDeleted ::CompanyId-> [EntryId] -> Day -> DB ()
updateMonthlyBalancesForEntriesToBeDeleted companyId ids date =
  mapM_
    ( \entryId -> do
        entry <- get404 entryId
        key <- liftHandler $ monthlyBalanceEntityKey companyId (entryAccountId entry) (getEndDate date)
        balance <- get key
        case balance of
          Just x -> replace key x {monthlyBalanceBalance = round' (monthlyBalanceBalance x - entryAmount entry)}
          _ -> sendResponseStatus status404 ("No balance to be cleared, this should never happen" :: Text)
    )
    ids


updateAccountBalancesForExistingEntries :: CompanyId -> [Entity Entry] -> Day -> Day -> DB ()
updateAccountBalancesForExistingEntries companyId entriesToUpdate' dateOfPostedTransaction dateOfStoredTransaction = do
  let endDateOfPostedTransaction = getEndDate dateOfPostedTransaction
  let endDateOfStoredTransaction = getEndDate dateOfStoredTransaction

  -- 1. Create a list with unique accounts found in the list of entries with identities

  let entriesToUpdate = Data.List.sortBy (\a b -> compare (entityKey a) (entityKey b)) entriesToUpdate'

  let keys = map (\(Entity key value) -> key) entriesToUpdate

  currentEntriesToBeUpdated <- selectList [EntryId <-. keys] [Asc EntryId]

  let newAndCurrent = zip entriesToUpdate currentEntriesToBeUpdated

  mapM_
    ( \((Entity newKey newEntry), (Entity currentKey currentEntry)) -> do
        -- If nothing changes there is nothing to be done
        let amountHasChanged = (round' (entryAmount newEntry) /= round' (entryAmount currentEntry))
        let accountIdHasChanged = entryAccountId newEntry /= entryAccountId currentEntry
        let transactionDateHasChanged = endDateOfPostedTransaction /= endDateOfStoredTransaction

        if (not amountHasChanged && not accountIdHasChanged && not transactionDateHasChanged)
          then return ()
          else do
            currentBalanceKey <- liftHandler $ monthlyBalanceEntityKey companyId (entryAccountId currentEntry) endDateOfStoredTransaction

            -- Check if current balance exists for old date and old id
            currentBalance' <- get currentBalanceKey
            currentBalance <- case currentBalance' of
              Just currentBalance -> return currentBalance
              Nothing -> sendResponseStatus status404 ("Cannot find current balance record, should never happen though :)" :: Text)

            if (not accountIdHasChanged && not transactionDateHasChanged)
              then do
                -- Subtract the balance of the old balance record
                let diff = entryAmount newEntry - entryAmount currentEntry
                let recalculatedOldBalance = currentBalance {monthlyBalanceBalance = round' (monthlyBalanceBalance currentBalance + diff)}
                replace currentBalanceKey recalculatedOldBalance
              else do
                -- Subtract the balance of the old balance record
                let removedOldBalance = currentBalance {monthlyBalanceBalance = round' (monthlyBalanceBalance currentBalance - entryAmount currentEntry)}
                replace currentBalanceKey removedOldBalance

                -- Get new key with the new date
                newBalanceKey <- liftHandler $ monthlyBalanceEntityKey companyId (entryAccountId newEntry) endDateOfPostedTransaction
                newBalance' <- get newBalanceKey

                -- Check if the balance is found in the table
                case newBalance' of
                  Just newBalance -> do
                    -- Increase the balance of the the new balance record by the amount of the new entry
                    let addedNewBalance = newBalance {monthlyBalanceBalance = round' (monthlyBalanceBalance newBalance + entryAmount newEntry)}
                    replace newBalanceKey addedNewBalance
                  Nothing -> do
                    newBalance <- liftHandler $ monthlyBalanceVal (entryAccountId newEntry) endDateOfPostedTransaction (round' (entryAmount newEntry))
                    insert_ newBalance
    )
    newAndCurrent

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

removeDirectoryIfExists :: FilePath -> Handler ()
removeDirectoryIfExists fileName =
  liftIO $ removeDirectoryRecursive fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

deleteTransaction :: TransactionId -> DB ()
deleteTransaction transactionId = do
  keys <- selectKeysList ([EntryTransactionId ==. transactionId]) []
  transaction <- get404 transactionId
  let companyId = transactionCompanyId transaction
  let date = transactionDate transaction
  print keys
  print date
  mapM_ (\item-> delete item) keys
  --_ <- deleteEntries keys date
  delete transactionId
  print "OKOKOK"

  let filePath =
        ( "companies" </> (show $ fromSqlKey companyId) </> "documents"
            </> (show $ fromSqlKey transactionId)
        )
  liftHandler $ removeDirectoryIfExists filePath
 
deleteEntries :: CompanyId -> [EntryId] -> Day -> DB ()
deleteEntries companyId entryIds transactionDate = do
  updateMonthlyBalancesForEntriesToBeDeleted companyId entryIds transactionDate
  deleteWhere [EntryId <-. entryIds]

monthlyBalanceEntityKey :: CompanyId -> AccountId -> Day -> Handler (Key MonthlyBalance)
monthlyBalanceEntityKey companyId accountId endDate = do
  case keyFromValues [PersistInt64 $ fromSqlKey companyId, PersistInt64 $ fromSqlKey accountId, PersistDay $ endDate] of
    Right key -> return key
    Left _ -> sendResponseStatus status404 ("Cannot create balance key" :: Text)


updateEntries companyId entriesToUpdate transactionDatePostedDoc transactionDateStoredDoc = do
  updateAccountBalancesForExistingEntries companyId 
    entriesToUpdate
    (transactionDatePostedDoc)
    (transactionDateStoredDoc)
  -- Update existing entries
  mapM_ (\item -> replace (entityKey item) (entityVal item)) entriesToUpdate

insertEntries :: CompanyId -> [Entry] -> Day -> DB [Key Entry]
insertEntries companyId newEntries transactionDate = do
  updateAccountBalancesForNewEntries companyId newEntries transactionDate
  ids <- insertMany newEntries
  return ids

monthlyBalanceEntity :: Key Account -> Day -> Double -> Handler (Entity MonthlyBalance)
monthlyBalanceEntity accountId endDateOfPostedTransaction balance = do
  companyId <- getCompanyId 
  case keyFromValues [PersistInt64 $ fromSqlKey accountId, PersistDay $ endDateOfPostedTransaction] of
    Left s -> sendResponseStatus status404 ("Cannot create balance record and key" :: Text)
    Right x -> do
      let (Entity key value) =
            Entity
              (x)
              MonthlyBalance
                { monthlyBalanceAccountId = accountId,
                  monthlyBalanceDate = endDateOfPostedTransaction,
                  monthlyBalanceBalance = balance,
                  monthlyBalanceCompanyId  = companyId
                }
      return $ Entity key value

monthlyBalanceVal :: Key Account -> Day -> Double -> Handler MonthlyBalance
monthlyBalanceVal accountId endDateOfPostedTransaction balance = do
  value <- monthlyBalanceEntity accountId endDateOfPostedTransaction balance
  return $ entityVal value

mkUTCTime ::
  (Integer, Int, Int) ->
  (Int, Int, Pico) ->
  UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
  UTCTime
    (fromGregorian year mon day)
    (timeOfDayToTime (TimeOfDay hour min sec))

getCurrentYear :: Handler Integer
getCurrentYear = do
  let date = liftIO $ getCurrentTime >>= return . toGregorian . utctDay
  (year, month, day) <- date
  return year

getOneMonthDates :: Integer -> Int -> (UTCTime, UTCTime)
getOneMonthDates year month = do
  let days = gregorianMonthLength year month
  let timeStart = mkUTCTime (year, month, 1) (0, 0, 0)
  let timeEnd = mkUTCTime (year, month, days) (0, 0, 0)
  (timeStart, timeEnd)

getOneMonthDays :: Integer -> Int -> (Day, Day)
getOneMonthDays year month = do
  let days = gregorianMonthLength year month
  let timeStart = fromGregorian year month 1
  let timeEnd = fromGregorian year month days
  (timeStart, timeEnd)

getStartDate :: Day -> Day
getStartDate day = do
  let (year, monthOfYear, _) = toGregorian day
  fromGregorian year monthOfYear 1

getEndDate :: Day -> Day
getEndDate day = do
  let (year, month, _) = toGregorian day
  let days = gregorianMonthLength year month
  fromGregorian year month days

class CoherentObject a where
  requireCheckJsonBodyForceCoherence :: Handler a

-- | The rule for any invoice is that customer specific payment terms must be defined
-- | cash discount days should always be less than regular payment term days (until due)
-- | Therefore invoices should enforce this rule, too
instance CoherentObject SalesInvoice where
  requireCheckJsonBodyForceCoherence = do
    invoice <- (requireCheckJsonBody :: Handler SalesInvoice)
    when (salesInvoicePaymenttermdays invoice < salesInvoiceCashdiscountdays invoice) $ sendResponseStatus status404 ("The due date of payment can not be less than the due date of payment with cash discount" :: Text)

    return invoice


instance CoherentObject Partner where
  requireCheckJsonBodyForceCoherence = do
    partner <- (requireCheckJsonBody :: Handler Partner)
    when (partnerPaymenttermdays partner < partnerCashdiscountdays partner) $ sendResponseStatus status404 ("Due date of payment can not be less than the due date of payment with cash discount" :: Text)
    return partner

class Extension a b where
  extend :: a b -> Handler Value

