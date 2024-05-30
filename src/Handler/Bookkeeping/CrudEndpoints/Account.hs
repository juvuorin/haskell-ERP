{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Handler.Bookkeeping.CrudEndpoints.Account where
import Database.Persist.Sql (fromSqlKey)
import Import
import Data.Time
import qualified Data.List
import Data.Foldable
import Common (selectListSecure)


type Month = Integer

class HasBalance a where
  getBalance :: CompanyId -> a -> Handler Double
  getMonthlyBalance ::CompanyId -> a -> Day -> Handler Double
instance HasBalance AccountId where

  getMonthlyBalance companyId accountId endDate = do    
    key <- monthlyBalanceEntityKey companyId accountId endDate
    maybeBalance <- runDB $ get key
    case maybeBalance of 
      Nothing -> sendResponseStatus status404 ("Could not find balance"::Text)
      Just balance -> return $ monthlyBalanceBalance balance

  getBalance companyId accountId = do 
    account <- runDB $ get accountId
    case account of
      Nothing -> sendResponseStatus status404 ("Could not find account"::Text)
      Just account -> do 
        let companyId = accountCompanyId account
        accountingYears <- runDB $ selectList [AccountingYearCompanyId ==. companyId][] :: Handler [Entity AccountingYear]
        case accountingYears of
          [] -> sendResponseStatus status404 ("The company has not been specified any accounting years"::Text)
          x : xs -> do
            let currentAccountingYear = entityVal $ Data.List.last accountingYears 
            let accountingYearStartDay = accountingYearStartDate currentAccountingYear
            let accountingYearEndDay = accountingYearEndDate currentAccountingYear
            let startDate = getStartDate accountingYearStartDay
            let endDate = getEndDate accountingYearEndDay
            balances <- runDB $ selectList [MonthlyBalanceAccountId ==. accountId, MonthlyBalanceDate >=. startDate, MonthlyBalanceDate <=. endDate][] :: Handler [Entity MonthlyBalance] 
            let totalBalance = foldl (\acc balance -> acc+monthlyBalanceBalance (entityVal balance)) 0 balances
            return totalBalance


getAccountingYearsR ::CompanyId ->Handler Value
getAccountingYearsR companyId = do
  accountingYears <- runDB $ selectList [AccountingYearCompanyId ==. companyId][] :: Handler [Entity AccountingYear]
  returnJson accountingYears

getAccountR :: CompanyId -> AccountId -> Handler Value
getAccountR companyId accountId = do
  account <- runDB $ get404 accountId
  return $ object ["account" .= (Entity accountId account)]

putAccountR :: CompanyId -> AccountId -> Handler Value
putAccountR companyId accountId = do
  account <- (requireCheckJsonBody :: Handler Account)
  runDB $ replace accountId account
  sendResponseStatus status200 ("ACCOUNT UPDATED" :: Text)

deleteAccountR :: CompanyId -> AccountId -> Handler Value
deleteAccountR companyId accountId = do
  --account <- (requireCheckJsonBody :: Handler Account)
  accountDeletableOrError accountId
  runDB $ delete accountId
  sendResponseStatus status200 ("ACCOUNT DELETED" :: Text)
  
