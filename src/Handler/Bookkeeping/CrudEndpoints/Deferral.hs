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

module Handler.Bookkeeping.CrudEndpoints.Deferral where

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
import Handler.Bookkeeping.CrudEndpoints.Account (getBalance)


type ExpenseAccountId = AccountId
type DeferralAccountId = AccountId
type Amount = Double

type DeferredExpensesAccountId = AccountId 
type ReceivablesCarriedForwardAccountId = AccountId





type AccrualsAndDeferredIncomeAccountId = AccountId
type AdvancePaymentAccountId = AccountId


type PrepaymentsAndAccruedIncomeAccountId = AccountId 
type AccruedExpenseAccountId = AccountId 



getExpenseAccountAndSum :: TransactionId -> Handler (AccountId, Double)
getExpenseAccountAndSum transactionId = do

  entries' <- runDB $ selectList [EntryTransactionId ==. transactionId][]
  let sumDebet = 
          foldl (\acc item -> do
            let value = entityVal item
            if  entryAmount value > 0 
            then  (entryAccountId value,snd acc+entryAmount value) 
            else acc) 
          (toSqlKey 0, 0) entries'
  return sumDebet

data TransactionWithEntries =  TransactionWithEntries {transaction :: Transaction, entries :: [Entry]}

 
storeNewTransactionsWithNewEntries :: (MonadIO m, PersistStoreWrite backend, Traversable t,
 BaseBackend backend ~ SqlBackend) =>
  t TransactionWithEntries -> ReaderT backend m (t (Key Transaction))
storeNewTransactionsWithNewEntries transactions = do
  traverse (\transaction'-> do
    transactionId <- insert (transaction transaction')
    entryKeys <- traverse (\entry-> insert entry {entryTransactionId = transactionId}) (entries transaction')
    return transactionId
    ) transactions

runDeferral :: CompanyId -> TransactionId ->  Day -> Day -> Handler [TransactionId]
runDeferral  companyId transactionId start end = do
  (expenseAccountId,sum) <- getExpenseAccountAndSum transactionId
  deferralAccountId <- to DeferredExpenses
  let deferrals = createDeferral expenseAccountId deferralAccountId sum start end
  case deferrals of
    Left s -> sendResponseStatus status404 s
    Right deferrals -> runDB $ storeNewTransactionsWithNewEntries deferrals

createDeferral :: ExpenseAccountId->DeferralAccountId-> Amount -> Day -> Day ->  Either Text [TransactionWithEntries]
createDeferral expenseAccountId deferralAccountId totalCost startDate endDate =

  case (splitCostOverMonths totalCost startDate endDate) of
    Left s -> Left s
    Right deferralList -> do

      let transactions = map (\(date,sum) -> do
            TransactionWithEntries {transaction = defTransaction {transactionDate=date, transactionType = TypeDeferral},
            entries = [ defEntry {entryAccountId = expenseAccountId, entryAmount=sum} ,
                        defEntry {entryAccountId = deferralAccountId, entryAmount= -sum}]}) deferralList

      Right $ transactions


-- Extract the last day of Day
lastDayOfMonth date = do
      let (year, month, day) = toGregorian date
      let daysInMonth = gregorianMonthLength year month
      fromGregorian year month daysInMonth


daysRemainingUntilEndOfMonth date = 1+diffDays (lastDayOfMonth date) date
daysPassedFromBeginningOfMonth date  = do
  let (_, _, days) = toGregorian date
  days

-- split cost over months so that first and last month deferral is based on exact number of effective days, first and last
-- month's amounts may differ from the rest 
splitCostOverMonthsExact :: Double -> Day -> Day -> Either Text [(Day, Double)]
splitCostOverMonthsExact totalCost startDate endDate  = do

    let months =  monthsBetween startDate endDate
    if months < 2 || totalCost < 1 then Left "Deferral should span at least two months and total cost should be at least 1" else do

      let totalDays = 1+diffDays endDate startDate

      -- Calculate cost for the first and last month
      let startMonthCost = totalCost * fromIntegral (daysRemainingUntilEndOfMonth startDate) / fromIntegral totalDays
      let endMonthCost = totalCost * fromIntegral (daysPassedFromBeginningOfMonth endDate) / fromIntegral totalDays

      -- create cost for each month
      let result =  map (\month -> do
            let nextMonthDay = addGregorianMonthsClip (fromIntegral month) startDate
            let (year, month, _) = toGregorian nextMonthDay
            let daysInMonth = gregorianMonthLength year month
            (nextMonthDay, round' $ totalCost * fromIntegral daysInMonth / fromIntegral totalDays)) [1..months-2]

      Right $ [(startDate,startMonthCost)]++result++[(endDate,endMonthCost)]


-- split cost over months based on how many months are involved in the effective period
-- The same expense is allocated for each month 
splitCostOverMonths :: Double -> Day -> Day -> Either Text [(Day, Double)]
splitCostOverMonths totalCost startDate endDate  = do

    let months =  monthsBetween startDate endDate
    if months < 2 || totalCost < 1 then 
      Left "Deferral should cover at least two months and total cost should be at least 1" 
      else do

      let (startYear, startMonth, _) = toGregorian startDate
      let startDate' = fromGregorian startYear startMonth 1
    
      let (endYear, endMonth, _) = toGregorian endDate
      let endDate' = fromGregorian endYear endMonth 1
  
      let splitCost = totalCost / fromIntegral months
  

      -- calculate monthly cost
      let result =  map (\month -> do
            let (year', month', _) = toGregorian $ addGregorianMonthsClip (fromIntegral month) startDate 
            let nextMonthDay = fromGregorian year' month' 1 
            (nextMonthDay, round' splitCost)) [1..months-2]
      
      Right $ [(startDate',splitCost)]++result++[(endDate',splitCost)]











