{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.BankStatement.BankStatements where

import Import

getBankStatementsR :: CompanyId -> Handler Value
getBankStatementsR companyId = do
  bankstatements <- runDB $ selectList [BankStatementCompanyId ==. companyId] [Asc BankStatementId] :: Handler [Entity BankStatement]
  returnJson bankstatements

getBankStatementsByMonthR :: CompanyId -> Int -> Integer -> Handler Value
getBankStatementsByMonthR id m y = do
  let (timeStart, timeEnd) = getOneMonthDays y m
  $(logInfo) $ "timeEnd: " ++ (pack $ show timeEnd)

  transactions <-
    runDB $
      selectList
        [ BankStatementCompanyId ==. id,
          BankStatementDate ==. timeEnd
        ]
        [Asc BankStatementId] ::
      Handler [Entity BankStatement]

  returnJson transactions
