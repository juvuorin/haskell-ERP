{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Invoice.Purchase.CrudEndpoints.PurchaseInvoices where

import Data.Aeson
import Database.Persist.Sql (
  fromSqlKey,
 )
import GHC.Exts
import Handler.Task.Task
import Import

getPurchaseInvoicesR :: CompanyId -> Handler Value
getPurchaseInvoicesR companyId = do
  invoices <- runDB $ selectList [] [] :: Handler [Entity PurchaseInvoice]
  returnJson invoices

getPurchaseInvoicesByMonthR :: CompanyId -> Int -> Integer -> Handler Value
getPurchaseInvoicesByMonthR companyId month year = do
  let (timeStart, timeEnd) = getOneMonthDays year month
  transtasks <-
    runDB $
      selectList
        [ PurchaseInvoiceCompanyId ==. companyId
        , PurchaseInvoiceDate >=. timeStart
        , PurchaseInvoiceDate <=. timeEnd
        ]
        [] ::
      Handler [Entity PurchaseInvoice]
  print transtasks
  returnJson transtasks

data MonthInfo = MonthInfo
  { month :: Int
  }
  deriving (Show, Generic)

instance ToJSON MonthInfo
instance FromJSON MonthInfo

type Created = GenericGADT 'PurchaseInvoiceStatusInvoiceCreated
type Verified = GenericGADT 'PurchaseInvoiceStatusInvoiceVerified
type Rejected = GenericGADT 'PurchaseInvoiceStatusInvoiceRejected
type Open = GenericGADT 'PurchaseInvoiceStatusInvoiceOpen
type Paid = GenericGADT 'PurchaseInvoiceStatusInvoicePaid


invoicesReadyForPayment :: DB [Open]
invoicesReadyForPayment = do
  companyId <- liftHandler $ getCompanyId
  openInvoices <- invoices companyId
  return openInvoices

class Monad m => MonadInvoice m a where
  invoices :: CompanyId -> m [a]
  invoice :: CompanyId -> Key PurchaseInvoice -> m a

instance MonadInvoice DB Created where
  invoice companyId invoiceId = do
    invoice <- selectFirst' [PurchaseInvoiceId ==. invoiceId, PurchaseInvoiceDocumentStatus ==. PurchaseInvoiceStatusInvoiceCreated] [] -- :: DB (Maybe (Entity PurchaseInvoice))
    case invoice of
      Just invoice -> return $ MkDocument invoice
      Nothing -> sendResponseStatus status404 ("Invoice not found" :: Text)

instance MonadInvoice DB Verified where
  invoice companyId invoiceId = do
    invoice <- selectFirst' [PurchaseInvoiceId ==. invoiceId, PurchaseInvoiceDocumentStatus ==. PurchaseInvoiceStatusInvoiceVerified] [] -- :: DB (Maybe (Entity PurchaseInvoice))
    case invoice of
      Just invoice -> return $ MkDocument invoice
      Nothing -> sendResponseStatus status404 ("Invoice not found" :: Text)

instance MonadInvoice DB Open where
  invoices companyId = do
    invoices <- selectList' [PurchaseInvoiceDocumentStatus ==. PurchaseInvoiceStatusInvoiceOpen] [] -- :: DB (Maybe (Entity PurchaseInvoice))
    return $ map MkDocument invoices

postVerifyInvoiceR :: CompanyId -> PurchaseInvoiceId -> Handler ()
postVerifyInvoiceR companyId invoiceId = do
  result <- runDB $ verify companyId invoiceId
  case result of
    Right _ -> sendResponseStatus status200 ("Invoice is successfully verified by all verifiers" :: Text)
    Left _ -> sendResponseStatus status200 ("Invoice is successfully verified by you" :: Text)

postApproveInvoiceR :: CompanyId -> PurchaseInvoiceId -> Handler ()
postApproveInvoiceR companyId invoiceId = do
  result <- runDB $ approve companyId invoiceId
  case result of
    Right _ -> sendResponseStatus status200 ("Invoice is successfully approved by all approvers" :: Text)
    Left _ -> sendResponseStatus status200 ("Invoice is successfully approved by you" :: Text)

postRejectInvoiceR :: CompanyId -> PurchaseInvoiceId -> Handler ()
postRejectInvoiceR companyId invoiceId = do
  result <- runDB $ reject companyId invoiceId
  case result of
    Right _ -> sendResponseStatus status200 ("Invoice is successfully rejected by all rejecters" :: Text)
    Left _ -> sendResponseStatus status200 ("Invoice is successfully rejected by you" :: Text)

verify :: CompanyId -> PurchaseInvoiceId -> DB (Either Created Verified)
verify companyId invoiceId = do
  invoiceCreated <- (invoice companyId invoiceId) :: DB Created
  --  let invoiceCreated = MkDocument (Entity (toSqlKey 3) defPurchaseInvoice)
  result <- processTasks invoiceCreated PurchaseInvoiceProcessingTaskVerify PurchaseInvoiceStatusInvoiceVerified
  case result of
    Nothing -> return $ Left invoiceCreated
    Just x -> return $ Right x

approve :: CompanyId -> PurchaseInvoiceId -> DB (Either Verified Open)
approve companyId invoiceId = do
  invoiceVerified <- (invoice companyId invoiceId) :: DB Verified
  --  let invoiceVerified = MkDocument (Entity (toSqlKey 3) defPurchaseInvoice)
  result <- processTasks invoiceVerified PurchaseInvoiceProcessingTaskApprove PurchaseInvoiceStatusInvoiceOpen
  case result of
    Nothing -> return $ Left invoiceVerified
    Just x -> return $ Right x

reject :: CompanyId -> PurchaseInvoiceId -> DB (Either Verified Rejected)
reject companyId invoiceId = do
  invoiceVerified <- (invoice companyId invoiceId) :: DB Verified
  -- let invoiceVerified = MkDocument (Entity (toSqlKey 3) defPurchaseInvoice)
  result <- processTasks invoiceVerified PurchaseInvoiceProcessingTaskReject PurchaseInvoiceStatusInvoiceRejected
  case result of
    Nothing -> return $ Left invoiceVerified
    Just x -> return $ Right x

postPurchaseInvoicesR :: CompanyId -> Handler Value
postPurchaseInvoicesR companyId = do
  invoice <- requireCheckJsonBody :: Handler PurchaseInvoice
  id <- runDB $ do
    entity <- insertEntity' invoice
    -- Let's check if someone has the right to verify or approve an invoice
    setTasks
      companyId
      entity
      [ PurchaseInvoiceProcessingTaskVerify
      , PurchaseInvoiceProcessingTaskApprove
      , PurchaseInvoiceProcessingTaskReject
      ]

    -- If verifier and approver are set we can change the status to Created
    update (entityKey entity) [PurchaseInvoiceDocumentStatus =. PurchaseInvoiceStatusInvoiceCreated]
    return (entityKey entity)

  return $ toJSON (fromSqlKey id)

postPurchaseInvoicesByMonthJsonR :: CompanyId -> Handler Value
postPurchaseInvoicesByMonthJsonR companyId = do
  monthInfo <- requireCheckJsonBody :: Handler MonthInfo
  let m = month monthInfo
  year <- getCurrentYear
  let (timeStart, timeEnd) = getOneMonthDays year m
  transtasks <-
    runDB $
      selectList
        [ PurchaseInvoiceCompanyId ==. companyId
        , PurchaseInvoiceDate >=. timeStart
        , PurchaseInvoiceDate <=. timeEnd
        ]
        [] ::
      Handler [Entity PurchaseInvoice]
  returnJson transtasks

data PurchaseInvoiceList
  = PurchaseInvoiceList [PurchaseInvoice]
  deriving (Generic, Show)

instance FromJSON PurchaseInvoiceList

instance ToJSON PurchaseInvoiceList

type DocumentId = Int64
