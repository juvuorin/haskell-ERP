{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Classes.Classes where

import Import

class BookkeepingTransaction transaction where
  add :: (BookkeepingTransaction transaction, PersistRecordBackend transaction SqlBackend) => transaction -> Handler (Key transaction)
  add x = runDB $ do
    addBookkeepingTransaction x
    insert x

  entries :: transaction -> [Entry]
  date :: transaction -> Day
  docType :: transaction -> TransactionType
  companyId :: transaction -> CompanyId

instance BookkeepingTransaction PurchaseInvoice where
  companyId transaction = purchaseInvoiceCompanyId transaction
  date transaction = purchaseInvoiceDate transaction
  docType _ = TypePurchaseInvoice

addBookkeepingTransaction :: (MonadIO m, PersistStoreWrite backend, BookkeepingTransaction transaction, BaseBackend backend ~ SqlBackend) => transaction -> ReaderT backend m (Key Transaction)
addBookkeepingTransaction transaction = do
  let bookkeepingTransaction =
        defTransaction
          { transactionCompanyId = companyId transaction,
            transactionDate = date transaction,
            transactionType = docType transaction,
            transactionMemo = Just $ pack $ show (docType transaction)
          }
  insert bookkeepingTransaction

addBookkeepingTransaction_ :: (MonadIO m, PersistStoreWrite backend, BookkeepingTransaction transaction, BaseBackend backend ~ SqlBackend) => transaction -> ReaderT backend m (Key Transaction)
addBookkeepingTransaction_ transaction = do
  let bookkeepingTransaction =
        defTransaction
          { transactionCompanyId = companyId transaction,
            transactionDate = date transaction,
            transactionType = docType transaction,
            transactionMemo = Just $ pack $ show (docType transaction)
          }
  insert bookkeepingTransaction


