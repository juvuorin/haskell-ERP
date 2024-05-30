{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}

module Handler.Bookkeeping.CrudEndpoints.Transactions where


import qualified Crypto.Hash.SHA1 as SHA1
import Data.Aeson
  ( FromJSON,
    KeyValue ((.=)),
    ToJSON (toJSON),
    Value,
    object,
  )
import qualified Data.ByteString
import Data.ByteString.Base16
import qualified Data.ByteString.UTF8 as BSU
import Data.List (find, foldl, last, iterate)
import Data.Time
import Database.Persist.Sql (fromSqlKey, toSqlKey, SqlBackendCanRead)
import External.IncomeRegister.Types ()
import Import hiding (last)
import System.Directory (doesDirectoryExist, listDirectory)
import Data.Time.Calendar.MonthDay
import Data.List (groupBy)
--import Data.Time.Calendar.MonthDay
import Data.List (groupBy)
import Data.Time.Calendar
import qualified Data.Conduit.List as CL
import Data.Acquire

type Month = Int
type Year = Integer

sumOfInsuranceExpenses :: Key Transaction -> DB Double
sumOfInsuranceExpenses transactionId = do
  transaction <- getJust transactionId
  let companyId = transactionCompanyId transaction
  idInsurance <- toId' 6504 companyId
  entries <- selectList [EntryTransactionId ==. transactionId, EntryAccountId ==. idInsurance] []
  let sum = foldl (\acc item -> acc + round' (entryAmount $ entityVal item)) 0 entries
  return sum

sumOfExpenseOfTransaction ::
  ( MonadIO m,
    PersistQueryRead backend,
    BaseBackend backend ~ SqlBackend
  ) =>
  Key Transaction ->
  ReaderT backend m [Entry]
sumOfExpenseOfTransaction transactionId = do
  transaction <- getJust transactionId
  let companyId = transactionCompanyId transaction
  allExpenseAccounts <-
    selectList
      [ AccountCode >=. 4000,
        AccountCode <=. 9999,
        AccountCompanyId ==. companyId
      ]
      []
  let allExpenseAccountKeys = map entityKey allExpenseAccounts

  allExpenseEntries <-
    selectList
      [ EntryTransactionId ==. transactionId,
        EntryAccountId <-. allExpenseAccountKeys,
        EntryAmount >. 0
      ]
      []

  let allPairEntryPercentage =
        map
          ( \item -> do
              let accountId = entryAccountId (entityVal item)
              let (Just account) = Data.List.find (\item -> accountId == entityKey item) allExpenseAccounts

              let taxType = accountTaxtype (entityVal account)
              let vatPercentage = accountVat (entityVal account)
              if Just True == fmap (> 0) vatPercentage && taxType == Just "gross"
                then (entityVal item, vatPercentage)
                else (entityVal item, Nothing)
          )
          allExpenseEntries

  return $
    map
      ( \(entry, vatPercentage) -> do
          case vatPercentage of
            Just x -> do
              let grossValue = entryAmount entry
              let netValue = round' $ grossValue * (1 / (1 + fromIntegral x / 100))
              defEntry {entryAmount = netValue, entryAccountId = entryAccountId entry}
            Nothing -> defEntry {entryAmount = entryAmount entry, entryAccountId = entryAccountId entry}
      )
      allPairEntryPercentage


postExpenseDeferralR :: CompanyId -> TransactionId -> Handler ()
postExpenseDeferralR companyId transactionId = do
  _ <- runDB $ do
    transaction <- get404 transactionId
    let transactionDate' = transactionDate transaction
    $(logInfo) $ "transaction date:" ++ pack (show transactionDate')

    accountingYearOfTheTransaction' <-
      selectFirst
        [ AccountingYearCompanyId ==. companyId,
          AccountingYearStartDate <=. transactionDate',
          AccountingYearEndDate >=. transactionDate'
        ]
        []
    when (isNothing accountingYearOfTheTransaction') $ sendResponseStatus status404 ("accounting year does not exist" :: Text)

    let (Just (Entity _ accountingYearOfTheTransaction)) = accountingYearOfTheTransaction'
    let (Just (effectiveFrom, effectiveTo)) = (,) <$> transactionEffectivefrom transaction <*> transactionEffectiveto transaction

    when (effectiveFrom >= effectiveTo) $ sendResponseStatus status404 ("effective from date cannot be less or equal to effective to date" :: Text)
    when (effectiveTo <= accountingYearEndDate accountingYearOfTheTransaction) $ sendResponseStatus status404 ("effective from date cannot be less or equal to effective to date" :: Text)

    entries <- sumOfExpenseOfTransaction transactionId

    let totalDifference = diffDays effectiveFrom effectiveTo
    let daysCurrentYear = diffDays effectiveFrom (accountingYearEndDate accountingYearOfTheTransaction)
    let daysNextYear = totalDifference - daysCurrentYear
    let expenseFuturePeriod =
          if effectiveFrom <= accountingYearEndDate accountingYearOfTheTransaction
            && effectiveTo > accountingYearEndDate accountingYearOfTheTransaction
            then round' $ fromIntegral daysNextYear / fromIntegral totalDifference
            else 1

    memoTransactionId <-
      insert
        ( defTransaction
            { transactionCompanyId = companyId,
              transactionMemo = Just $ pack $ "Jaksotus seuraavalle tilikaudelle, jaksotettava tosite: " ++ show (fromSqlKey $ transactionId),
              transactionType = TypeMemo,
              transactionDate = accountingYearEndDate accountingYearOfTheTransaction
            }
        )

    mapM
      ( \entry -> do
          let reduction = round' $ expenseFuturePeriod * entryAmount entry
          let expenseReduction =
                defEntry
                  { entryMemo = Just "Jaksotus",
                    entryAmount = - reduction,
                    entryAccountId = entryAccountId entry,
                    entryTransactionId = memoTransactionId
                  }

          idCarryForwardsReceivable <- toId' 1515 companyId

          let expenseDeferral =
                defEntry
                  { entryMemo = Just "Jaksotus",
                    entryAmount = reduction,
                    entryAccountId = idCarryForwardsReceivable,
                    entryTransactionId = memoTransactionId
                  }

          insertEntries companyId [expenseReduction, expenseDeferral] (accountingYearEndDate accountingYearOfTheTransaction)
      )
      entries
  sendResponseStatus status201 ("EXPENSE DEFERRAL DONE" :: Text)

getTransactionsByMonthR :: CompanyId -> Month -> Year -> Handler Value
getTransactionsByMonthR companyId month year = do
  let (timeStart, timeEnd) = getOneMonthDays year month
  transactions <-
    runDB $
      selectList
        [ TransactionCompanyId ==. companyId,
          TransactionDate >=. timeStart,
          TransactionDate <=. timeEnd
        ]
        [Asc TransactionId] ::
      Handler [Entity Transaction]
  returnJson transactions

getTransactionsByMonthR' :: CompanyId -> Month -> Year -> Handler Value
getTransactionsByMonthR' companyId month year = do
  let (timeStart, timeEnd) = getOneMonthDays year month
  transactions <-
    runDB $
      selectList
        [ TransactionCompanyId ==. companyId,
          TransactionDate >=. timeStart,
          TransactionDate <=. timeEnd
        ]
        [Asc TransactionId] ::
      Handler [Entity Transaction]

  accountingYears <- runDB $ selectList [AccountingYearCompanyId ==. companyId] []

  (entriesOfLastTransaction, filesOfLastTransaction) <- case transactions of
    (x : xs) -> do
      let lastTransaction = Data.List.last transactions
      entriesOfLastTransaction <- runDB $ selectList [EntryTransactionId ==. entityKey lastTransaction] [Asc EntryId]

      let filePath = "companies" </> show (fromSqlKey companyId) </> "documents" </> show (fromSqlKey (entityKey lastTransaction))
      isDirectoryPresent <- liftIO $ doesDirectoryExist filePath
      filesOfLastTransaction <-
        if isDirectoryPresent then liftIO $ listDirectory filePath else return []
      return (entriesOfLastTransaction, filesOfLastTransaction)
    _ -> return ([], [])

  return
    ( object
        [ "transactions" .= transactions,
          "entriesOfLastTransaction" .= entriesOfLastTransaction,
          "accountingYears" .= accountingYears,
          "filesOfLastTransaction" .= filesOfLastTransaction
        ]
    )


data MonthInfo = MonthInfo
  { month :: Int
  }
  deriving (Show, Generic)

instance ToJSON MonthInfo

instance FromJSON MonthInfo

postTransactionsByMonthJsonR :: CompanyId -> Handler Value
postTransactionsByMonthJsonR id = do
  monthInfo <- requireCheckJsonBody :: Handler MonthInfo
  let m = month monthInfo
  year <- getCurrentYear
  let (timeStart, timeEnd) = getOneMonthDays year m
  transactions <-
    runDB $
      selectList
        [ TransactionCompanyId ==. id,
          TransactionDate >=. timeStart,
          TransactionDate <=. timeEnd
        ]
        [Asc TransactionId] ::
      Handler [Entity Transaction]
  returnJson transactions

postTransactionsR :: CompanyId -> Handler ()
postTransactionsR companyId = do
  transaction <- requireCheckJsonBody :: Handler Transaction
  id <- runDB $ insert transaction
  sendResponseStatus status201 (object ["id" .= fromSqlKey id])

data EntryList
  = EntryList [Entry]
  deriving (Generic, Show)

instance FromJSON EntryList

instance ToJSON EntryList

postRecalcBalancesR :: CompanyId -> Handler ()
postRecalcBalancesR companyId = do
  result <- runDB $ do
    accounts <- selectKeysList [AccountCompanyId ==. companyId] []
    $(logInfo) "Relevant accounts selected"
    deleteWhere [MonthlyBalanceAccountId <-. accounts]
    $(logInfo) "Old balances deleted"
    allDocs <- selectList [TransactionCompanyId ==. companyId] []
    let r =
          foldl
            ( \acc item -> do
                let date = transactionDate (entityVal item)
                let endDate = getEndDate date
                let dateFound = any (\item -> fst item == endDate) acc
                let newAcc =
                      if dateFound
                        then
                          map
                            ( \accItem ->
                                if endDate == fst accItem
                                  then
                                    ( fst accItem,
                                      snd accItem ++ [entityKey item]
                                    )
                                  else accItem
                            )
                            acc
                        else acc ++ [(endDate, [entityKey item])]
                newAcc
            )
            []
            allDocs

    mapM
      ( \(day, keys) -> do
          entries' <- selectList [EntryTransactionId <-. keys] []
          let entries = map entityVal entries'
          $(logInfo) "Transaction computed..."
          updateAccountBalancesForNewEntries companyId entries day
      )
      r

  sendResponseStatus status200 ("Balances Updated!" :: Text)

deleteEntriesR :: CompanyId -> EntryId -> Handler ()
deleteEntriesR companyId entryId = do
  runDB $ do
    entry <- get404 entryId
    doc <- get404 $ entryTransactionId entry
    $(logInfo) "Entry pahse 1 OK"

    deleteEntries companyId [entryId] $ transactionDate doc
    updateMonthlyBalancesForEntriesToBeDeleted companyId [entryId] $ transactionDate doc
    $(logInfo) "Entry deleted OK"
  sendResponseStatus status200 ("DELETED" :: Text)


postEntriesR :: CompanyId-> TransactionId -> Handler ()
postEntriesR companyId documentId =
  sendResponseStatus status500 ("Not implemented"::Text)

postDocumentEntryR :: CompanyId-> TransactionId -> Handler ()
postDocumentEntryR companyId documentId =
  sendResponseStatus status500 ("Not implemented"::Text)

putDocumentEntriesR3 :: CompanyId->EntryId->Handler ()
putDocumentEntriesR3 companyId entryId =
  sendResponseStatus status500 ("Not implemented"::Text)



