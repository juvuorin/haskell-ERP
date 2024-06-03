{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Handler.Bookkeeping.CrudEndpoints.Transaction where

import Data.Aeson
import Data.List (foldl, length)
import qualified Data.Text.IO
import Database.Persist.Sql (fromSqlKey)
import External.IncomeRegister.Types ()
import qualified GHC.Generics
import Import hiding (Transaction, Html, check, elem, filter, head, id, length, post, toHtml)
import Model
import System.Directory
  ( doesDirectoryExist,
    listDirectory,
  )
import System.Process (callCommand)
import Data.Time
import Data.Text.IO.Utf8



-- A helper to package transaction and related entries
-- Entries without id are considered new ones
data TransactionWithNewAndExistingEntries = TransactionWithHeterogenousEntries
  { transaction :: Transaction,
    entries :: [HeterogenousListEntry]
  }
  deriving (Generic, Show)
instance FromJSON TransactionWithNewAndExistingEntries


addproperties :: [HeterogenousListEntry] -> DB [HeterogenousListEntry] 
addproperties =
  mapM
    ( \heterogenousEntry -> do
        case heterogenousEntry of
          WithId (Entity key entry) -> do
            account <- get404 $ entryAccountId entry
            case accountPropertyId account of
              Just x -> do
                let newEntry = entry {entryPropertyId = Just x}
                return $ WithId (Entity key newEntry)
              Nothing -> return heterogenousEntry
          WithoutId entry -> do
            account <- get404 $ entryAccountId entry
            case accountPropertyId account of
              Just x -> do
                return $ WithoutId entry {entryPropertyId = Just x}
              Nothing -> return heterogenousEntry
    )

updateBookkeepingTransactionAndInsertOrUpdateEntries ::
  CompanyId -> TransactionId ->
  TransactionWithNewAndExistingEntries ->
  DB [Key Entry] 

updateBookkeepingTransactionAndInsertOrUpdateEntries companyId transactionId transactionWithEntries = do
  let entriesFromClient' = entries transactionWithEntries
  mapM_ print entriesFromClient'
  let (entriesToInsert, entriesToUpdate) =
        foldl
          ( \acc item -> do
              case item of
                WithId (Entity key x) -> (fst acc, snd acc ++ [Entity key x])
                WithoutId x -> (fst acc ++ [x], snd acc)
          )
          ([], [])
          entriesFromClient'

  let postedDoc = transaction transactionWithEntries
  storedDoc <- get404 transactionId

  -- Transaction needs to be updated
  replace transactionId postedDoc

  -- Update balances of new entries and insert entries
  ids <- insertEntries companyId entriesToInsert (transactionDate postedDoc)

  -- Update balances of existing entries and update entries
  updateEntries companyId entriesToUpdate (transactionDate postedDoc) (transactionDate storedDoc)

  return ids

data HtmlContent = HtmlContent {html :: Text} deriving (Show, Generic)

instance FromJSON HtmlContent

instance ToJSON HtmlContent

data PdfReportType = Journal | GeneralLedger
  deriving (Show, Generic, Eq)

instance FromJSON PdfReportType

instance ToJSON PdfReportType

postHtmlToPdfR :: CompanyId -> String -> Handler ()
postHtmlToPdfR companyId reportType = do
  htmlDoc <- requireCheckJsonBody :: Handler HtmlContent
  print (show reportType)
  liftIO $ Data.Text.IO.Utf8.writeFile (reportType ++ ".html") (html htmlDoc)
  let command = "node /home/azureuser/pdf/html_to_pdf/runner.js " ++ reportType ++ ".html"
  r <- liftIO $ callCommand command
  return ()

data Id' index where
  AccountId' :: AccountId -> Id' AccountId
  TransactionId' :: TransactionId -> Id' TransactionId
  CompanyId' :: CompanyId -> Id' CompanyId
  TransactionIdList' :: [TransactionId] -> Id' [TransactionId]
  EntryIdList' :: [EntryId] -> Id' [EntryId]

-- A side-effect of using GADTs is that we need to use standalone deriving
-- for our instances.
deriving instance Show (Id' i)

deriving instance Eq (Id' i)

data CheckKeys a where
  CheckKeyItem ::
    ( PersistQueryRead (YesodPersistBackend App),
      YesodPersist App,
      ToBackendKey SqlBackend record1,
      ToBackendKey SqlBackend record3,
      PersistEntity record2,
      PersistEntityBackend record2 ~ BaseBackend (YesodPersistBackend App)
    ) =>
    (((EntityField record2 (Key record1), Key record1), (EntityField record2 (Key record3), Key record3))) ->
    CheckKeys ((EntityField record2 (Key record1), Key record1), (EntityField record2 (Key record3), Key record3))
  CheckKeyList ::
    ( PersistQueryRead (YesodPersistBackend App),
      YesodPersist App,
      ToBackendKey SqlBackend record1,
      ToBackendKey SqlBackend record3,
      PersistEntity record2,
      PersistEntityBackend record2 ~ BaseBackend (YesodPersistBackend App)
    ) =>
    (((EntityField record2 (Key record1), Key record1), (EntityField record2 (Key record3), [Key record3]))) ->
    CheckKeys ((EntityField record2 (Key record1), Key record1), (EntityField record2 (Key record3), [Key record3]))

checkConsistency :: CheckKeys a -> Handler ()
checkConsistency (CheckKeyItem ((parentIdType, parentId), (childIdType, childId))) = do
  transaction <- runDB $ selectFirst [parentIdType ==. parentId, childIdType ==. childId] []
  checkMaybe transaction
checkConsistency (CheckKeyList ((parentIdType, parentId), (childIdType, childKeys))) = do
  keys <- runDB $ selectKeysList [parentIdType ==. parentId, childIdType <-. childKeys] []
  checkLength childKeys keys
  return ()

securityError :: (MonadHandler m) => m ()
securityError = do
  sendResponseStatus status500 ("Security error!" :: Text)

checkLength :: (Foldable t1, Foldable t2, MonadHandler m) => t1 a1 -> t2 a2 -> m ()
checkLength a b = do
  case (Data.List.length a == Data.List.length b) of
    False -> securityError
    True -> return ()

checkMaybe :: Maybe a -> HandlerFor site ()
checkMaybe a = do
  case a of
    Just x -> return ()
    Nothing -> securityError

checkTransactionBelongsToCompany transactionId companyId = do
  checkConsistency $ CheckKeyItem ((TransactionCompanyId, companyId), (TransactionId, transactionId))

checkEntriesBelongToTransaction :: [Key Entry] -> Key Transaction -> Handler ()
checkEntriesBelongToTransaction entryIds transactionId = do
  checkConsistency $ CheckKeyList ((EntryTransactionId, transactionId), (EntryId, entryIds))

test ::
  ( PersistQueryRead (YesodPersistBackend site),
    YesodPersist site,
    PersistEntity record,
    PersistField typ1,
    PersistField typ2,
    PersistEntityBackend record
      ~ BaseBackend (YesodPersistBackend site)
  ) =>
  EntityField record typ1 ->
  typ1 ->
  EntityField record typ2 ->
  typ2 ->
  HandlerFor site ()
test parentIdType parentId childIdType childId = do
  transaction <- runDB $ selectFirst [parentIdType ==. parentId, childIdType ==. childId] []
  return ()

putTransactionUpdateR :: CompanyId -> TransactionId -> Handler ()
putTransactionUpdateR companyId transactionId = do
  $(logInfo) "here we go... again!"
  transactionWithEntries <- requireCheckJsonBody :: Handler (DocumentWithNewAndExistingItems Transaction Entry)
  entryIds <- runDB $ updateParentAndInsertOrUpdateChildren transactionId transactionWithEntries
  sendResponseStatus status201 $ show $ map fromSqlKey entryIds

getTransactionR :: CompanyId -> TransactionId -> Handler Value
getTransactionR companyId transactionId = do

  checkTransactionBelongsToCompany transactionId companyId
  transaction <- runDB $ selectFirst [TransactionCompanyId ==. companyId, TransactionId ==. transactionId] []
  case transaction of
    Just (Entity _ value) -> return $ object ["transaction" .= Entity transactionId value]
    _ -> sendResponseStatus status404 ("Transaction not found" :: Text)

getTransactionEntriesR :: Key Company -> Key Transaction -> HandlerFor App Value
getTransactionEntriesR companyId transactionId = do
  checkConsistency $ CheckKeyItem ((TransactionCompanyId, companyId), (TransactionId, transactionId))

  let filePath =
        "companies" </> show (fromSqlKey companyId) </> "documents"
          </> show (fromSqlKey transactionId)
  putStrLn (pack filePath)
  isDirectoryPresent <- liftIO $ doesDirectoryExist filePath
  files <-
    if isDirectoryPresent then liftIO $ listDirectory filePath else return []
  runDB $ do
    entries <- selectList [EntryTransactionId ==. transactionId] [Asc EntryId]
    transaction <- getEntity transactionId
    case transaction of
      Just x -> return $ object ["rows" .= entries, "files" .= files, "transaction" .= transaction]
      Nothing -> sendResponseStatus status404 ("Cannot find transaction! Should never happen!" :: Text)

data AutomationMasterKey = AutomationMasterKey
  { automationmasterkey :: String
  }
  deriving (Show, Generic)

instance FromJSON AutomationMasterKey

deleteTransactionR :: CompanyId -> TransactionId -> Handler ()
deleteTransactionR companyId transactionId = do
  runDB $ deleteTransaction transactionId
  sendResponseStatus status200 ("DELETED" :: Text)
