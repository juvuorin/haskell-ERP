{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Bookkeeping.CrudEndpoints.Accounts where

import Database.Persist.Sql (fromSqlKey)
import Import
    ( ($),
      Monad(return),
      object,
      status201,
      (==.),
      selectList,
      sendResponseStatus,
      requireCheckJsonBody,
      insert400,
      Value,
      (.=),
      ToJSON(toJSON),
      Entity,
      EntityField(AccountCode, AccountCompanyId),
      SelectOpt(Asc),
      YesodPersist(runDB),
      Account,
      CompanyId,
      Handler, returnJson, fmap )

getAccountsR :: CompanyId -> Handler Value
getAccountsR companyId = do
  accounts <- runDB $ selectList [AccountCompanyId ==. companyId] [Asc AccountCode] :: Handler [Entity Account]  
  returnJson accounts

postAccountsR :: CompanyId -> Handler Value
postAccountsR companyId = do
  account <- requireCheckJsonBody :: Handler Account
  id <- runDB $ insert400 account
  sendResponseStatus status201 (object ["id" .= (fromSqlKey id)])
