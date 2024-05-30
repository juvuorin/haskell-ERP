{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Handler.Invoice.Purchase.CrudEndpoints.PurchaseInvoice where

import Import hiding (Tax)
import Common (CoherentObject (requireCheckJsonBodyForceCoherence), Extension, extend)
import Data.Aeson (GFromJSON, SumEncoding (UntaggedValue), Value (Object), Zero, defaultOptions, genericParseJSON, sumEncoding)
import qualified Data.HashMap.Lazy as HML
import Data.List (foldl)
import Data.List.Split.Internals (Chunk (Text))
import Data.Time
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import qualified GHC.Generics
import Handler.Bookkeeping.CrudEndpoints.Transaction (checkTransactionBelongsToCompany)
import System.Directory (
  doesDirectoryExist,
  listDirectory,
  removeDirectoryRecursive,
  removeFile,
 )
import Text.XML.HaXml (x)

putPurchaseInvoiceR :: CompanyId -> PurchaseInvoiceId -> Handler Value
putPurchaseInvoiceR companyId purchaseInvoiceId =
  do
    invoice <- (requireCheckJsonBody :: Handler PurchaseInvoice)
    runDB $ replace purchaseInvoiceId invoice
    sendResponseStatus status200 ("UPDATED" :: Text)

merge :: [Value] -> Value
merge = Object . HML.unions . Import.map (\(Object x) -> x)

class HasDueDate a where
  getDueDate :: a -> IO Day

instance HasDueDate PurchaseInvoice where
  getDueDate item = return $ purchaseInvoiceDuedate item

computeDaysDue :: (HasDueDate a) => a -> Handler Integer
computeDaysDue item = do
  dueDate <- liftIO $ getDueDate item
  time <- liftIO $ getZonedTime
  let currentDay = localDay (zonedTimeToLocalTime time)
  let diff = diffDays currentDay dueDate
  return diff

isOverdue :: (HasDueDate a) => a -> Handler Bool
isOverdue item = do
  days <- computeDaysDue item
  return $ if days > 0 then True else False
     
getPurchaseInvoiceR :: CompanyId -> PurchaseInvoiceId -> Handler Value
getPurchaseInvoiceR cid id = do
  invoice <- runDB $ get404 id
  overdue <- isOverdue invoice
  print overdue
  return $ object ["invoice" .= Entity id invoice]

getPurchaseInvoiceDetailsR :: CompanyId -> PurchaseInvoiceId -> Handler Value
getPurchaseInvoiceDetailsR companyId invoiceId = do
  let filePath =
        "companies" </> show (fromSqlKey companyId) </> "invoices"
          </> show (fromSqlKey invoiceId)
  entries <-
    runDB $
      selectList
        [PurchaseInvoiceDetailPurchaseInvoiceId ==. invoiceId]
        [Asc PurchaseInvoiceDetailId] ::
      Handler [Entity PurchaseInvoiceDetail]
  $(logInfo) (pack $ show entries)
  return $ object ["rows" .= entries]

deletePurchaseInvoiceR :: CompanyId -> PurchaseInvoiceId -> Handler Value
deletePurchaseInvoiceR companyId docId = do
  runDB $ delete docId
  let filePath =
        "companies" </> show companyId </> "invoices" </> show docId
  removeDirectoryIfExists filePath
  sendResponseStatus status200 ("DELETED" :: Text)

getPostToLedgerR :: CompanyId -> PurchaseInvoiceId -> Handler Value
getPostToLedgerR companyId invoiceId = do
  return "Not implemented"

updateAndReturnNewItemIds ::
  ( PersistRecordBackend b SqlBackend
  , PersistRecordBackend a SqlBackend
  , PersistEntity a
  , ToBackendKey SqlBackend b
  ) =>
  Key a ->
  DocumentWithNewAndExistingItems a b ->
  Handler ()
updateAndReturnNewItemIds key transactionWithEntries = do
  entryIds <- runDB $ updateParentAndInsertOrUpdateChildren key transactionWithEntries
  sendResponseStatus status201 $ show $ map fromSqlKey entryIds

putPurchaseUpdateR :: CompanyId -> PurchaseInvoiceId -> Handler ()
putPurchaseUpdateR companyId purchaseInvoiceId = do
  -- checkTransactionBelongsToCompany purchaseInvoiceId companyId
  $(logInfo) "here we go... again!"
  transactionWithEntries <- requireCheckJsonBody :: Handler (DocumentWithNewAndExistingItems PurchaseInvoice PurchaseInvoiceDetail)
  entryIds <- runDB $ updateParentAndInsertOrUpdateChildren purchaseInvoiceId transactionWithEntries
  sendResponseStatus status201 $ show $ map fromSqlKey entryIds

newtype Net = Net {unNet :: Double}
newtype Gross = Gross {unGross :: Double}
newtype Tax = Tax {unTax :: Double}

