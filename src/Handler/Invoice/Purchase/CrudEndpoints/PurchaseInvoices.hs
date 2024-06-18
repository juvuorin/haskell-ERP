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
{-# LANGUAGE StandaloneKindSignatures #-}
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

--type Rejected = GenericGADT 'PurchaseInvoiceStatusInvoiceRejected
type Open = GenericGADT 'PurchaseInvoiceStatusInvoiceOpen
type Paid = GenericGADT 'PurchaseInvoiceStatusInvoicePaid

type family ResultOf a

-- A Created invoice can only stay Created or become Verified
type instance ResultOf Created = Either Created Verified

-- A Verified invoice can only stay Verified or become Open
type instance ResultOf Verified = Either Verified Open

-- An Open invoice can only stay Open or become Paid
type instance ResultOf Open = Either Open Paid

{- verify' :: Created -> DB (ResultOf Created)
verify' invoiceCreated = do
  result <- processTasks invoiceCreated PurchaseInvoiceProcessingTaskVerify PurchaseInvoiceStatusInvoiceVerified
  case result of
    Nothing -> return $ Left invoiceCreated
    Just x -> return $ Right x
 -}

{-
verify :: Created -> DB (Either Created Verified)
result <- processTasks invoiceCreated PurchaseInvoiceProcessingTaskVerify PurchaseInvoiceStatusInvoiceVerified

approve :: Verified -> DB (Either Verified Open)
result <- processTasks invoiceVerified PurchaseInvoiceProcessingTaskApprove PurchaseInvoiceStatusInvoiceOpen

reject :: Verified -> DB (Either Verified Rejected)
result <- processTasks invoiceVerified PurchaseInvoiceProcessingTaskReject PurchaseInvoiceStatusInvoiceRejected
 -}

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

result = PurchaseInvoiceProcessingTaskResultInvoiceVerified
postProcessInvoiceInvoiceR :: CompanyId -> PurchaseInvoiceId -> TaskResult -> Handler ()
postProcessInvoiceInvoiceR companyId invoiceId result = runDB $ do
  purchaseInvoice <- get404 invoiceId
  let currentStatus = purchaseInvoiceDocumentStatus purchaseInvoice

  x <- case currentStatus of
        PurchaseInvoiceStatusInvoiceCreated -> do 
            result <- invoice companyId invoiceId >>= flip verify result
            case result of
              Right _ -> return ("Invoice is successfully verified by all verifiers" :: Text)
              Left _ -> return ("Invoice is successfully verified by you" :: Text)
  
        PurchaseInvoiceStatusInvoiceVerified -> do 
            result <- invoice companyId invoiceId >>= flip approveOrReject result
            case result of
              Right _ -> return  ("Invoice is successfully approved by all approvers" :: Text)
              Left _ -> return ("Invoice is successfully approved by you" :: Text)
        
        _ -> sendResponseStatus status500 ("Illegal processing instruction" :: Text)
  sendResponseStatus status200 x

getInvoices :: DB [Entity PurchaseInvoice]
getInvoices = selectList' [] []

verify :: Created -> TaskResult -> DB (Either Created Verified)
verify invoiceCreated result = do
  result <- processTasks invoiceCreated PurchaseInvoiceProcessingTaskVerify result PurchaseInvoiceStatusInvoiceVerified
  case result of
    Nothing -> return $ Left invoiceCreated
    Just x -> return $ Right x

approveOrReject :: Verified -> TaskResult -> DB (Either Verified Open)
approveOrReject invoiceVerified result = do
  result <- processTasks invoiceVerified PurchaseInvoiceProcessingTaskApproveOrReject result PurchaseInvoiceStatusInvoiceOpen
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
      , PurchaseInvoiceProcessingTaskApproveOrReject
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
