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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Handler.Invoice.Purchase.CrudEndpoints.PurchaseInvoices where

import Data.Aeson
import Database.Persist.Sql (
  fromSqlKey, PersistFieldSql,
 )
import GHC.Exts
import Handler.Task.Task
import Import
import qualified Import as T
import qualified Data.Char as C
import Data.List.NonEmpty hiding (map)
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

type Created = GenericGADT ('PurchaseInvoiceStatus 'PurchaseInvoiceStatusInvoiceCreated)
type Verified = GenericGADT('PurchaseInvoiceStatus 'PurchaseInvoiceStatusInvoiceVerified)

--type Rejected = GenericGADT 'PurchaseInvoiceStatusInvoiceRejected
type Open = GenericGADT ('PurchaseInvoiceStatus 'PurchaseInvoiceStatusInvoiceOpen)
type Paid = GenericGADT ('PurchaseInvoiceStatus 'PurchaseInvoiceStatusInvoicePaid)

type family ResultOf a

-- A Created invoice can only stay Created or become Verified
type instance ResultOf Created = Either Created Verified

-- A Verified invoice can only stay Verified or become Open
type instance ResultOf Verified = Either Verified Open

-- An Open invoice can only stay Open or become Paid
type instance ResultOf Open = Either Open Paid

invoicesReadyForPayment :: DB [Open]
invoicesReadyForPayment = do
  companyId <- liftHandler $ getCompanyId
  openInvoices <- invoices companyId
  return openInvoices

class Monad m => MonadInvoice m a where
  invoices :: CompanyId -> m [a]
  invoice :: CompanyId -> Key PurchaseInvoice -> m a

class Monad m => ValidInvoice m a where
  --invoices :: CompanyId -> m [a]
  validate :: Entity PurchaseInvoice -> m a

instance ValidInvoice DB Created where
  validate invoice = do
      if purchaseInvoiceDocumentStatus (entityVal invoice) == PurchaseInvoiceStatus PurchaseInvoiceStatusInvoiceCreated
        then return $ MkDocument invoice
        else sendResponseStatus status404 ("Invoice is invalid" :: Text)

instance ValidInvoice DB Verified where
  validate invoice = do
      if purchaseInvoiceDocumentStatus (entityVal invoice) == PurchaseInvoiceStatus PurchaseInvoiceStatusInvoiceVerified
        then return $ MkDocument invoice
        else sendResponseStatus status404 ("Invoice is invalid" :: Text)


instance MonadInvoice DB Created where
  invoice companyId invoiceId = do
    invoice <- selectFirst' [PurchaseInvoiceId ==. invoiceId, PurchaseInvoiceDocumentStatus ==. PurchaseInvoiceStatus PurchaseInvoiceStatusInvoiceCreated] [] -- :: DB (Maybe (Entity PurchaseInvoice))
    case invoice of
      Just invoice -> return $ MkDocument invoice
      Nothing -> sendResponseStatus status404 ("Invoice not found" :: Text)

instance MonadInvoice DB Verified where
  invoice companyId invoiceId = do
    invoice <- selectFirst' [PurchaseInvoiceId ==. invoiceId, PurchaseInvoiceDocumentStatus ==. PurchaseInvoiceStatus PurchaseInvoiceStatusInvoiceVerified] [] -- :: DB (Maybe (Entity PurchaseInvoice))
    case invoice of
      Just invoice -> return $ MkDocument invoice
      Nothing -> sendResponseStatus status404 ("Invoice not found" :: Text)

instance MonadInvoice DB Open where
  invoices companyId = do
    invoices <- selectList' [PurchaseInvoiceDocumentStatus ==. PurchaseInvoiceStatus PurchaseInvoiceStatusInvoiceOpen] [] -- :: DB (Maybe (Entity PurchaseInvoice))
    return $ map MkDocument invoices


--purchaseInvoiceProcessingTasks :: [Task]
purchaseInvoiceProcessingTasks = [PurchaseInvoiceProcessingTaskVerify, PurchaseInvoiceProcessingTaskApproveOrReject]
   
processNextInvoiceState ::Entity PurchaseInvoice -> DocumentStatus -> PurchaseInvoiceTaskResult -> DB Text
processNextInvoiceState invoice (PurchaseInvoiceStatus status) result = do
            case status of
              PurchaseInvoiceStatusInvoiceCreated -> do
                result <- validate invoice >>= flip verify (toTaskResult result)
                case result of
                  -- Invoice is verified by all verifiers, let's move on to the next task
                  Right _ -> return ("Invoice is successfully verified by all verifiers" :: Text)
                  Left _ -> return ("Invoice is successfully verified by you" :: Text)

              PurchaseInvoiceStatusInvoiceVerified -> do
                result <- validate invoice >>= flip approveOrReject (toTaskResult result)
                case result of
                  Right _ -> return  ("Invoice is successfully approved by all approvers" :: Text)
                  Left _ -> return ("Invoice is successfully approved by you" :: Text)
              _ -> sendResponseStatus status500 ("Illegal processing instruction" :: Text)
   
postProcessInvoiceInvoiceR :: CompanyId -> PurchaseInvoiceId -> PurchaseInvoiceTaskResult -> Handler ()
postProcessInvoiceInvoiceR companyId invoiceId taskResult = 
  do
    x <- runDB $ do
      purchaseInvoice <- getEntity404' invoiceId
      processNextInvoiceState purchaseInvoice (purchaseInvoiceDocumentStatus (entityVal purchaseInvoice)) taskResult
    sendResponseStatus status200 x

class ToTaskResult a where
  toTaskResult :: a -> TaskResult 

instance ToTaskResult PurchaseInvoiceTaskResult where
  toTaskResult = PurchaseInvoiceTaskResult

getInvoices :: DB [Entity PurchaseInvoice]
getInvoices = selectList' [] []

verify :: Created -> TaskResult -> DB (Either Created Verified)
verify invoiceCreated result = do
  result <- processTasks invoiceCreated PurchaseInvoiceProcessingTaskVerify result $ PurchaseInvoiceStatus PurchaseInvoiceStatusInvoiceVerified
  case result of
    Nothing -> return $ Left invoiceCreated
    Just x -> return $ Right x

approveOrReject :: Verified -> TaskResult -> DB (Either Verified Open)
approveOrReject invoiceVerified result = do
  result <- processTasks invoiceVerified PurchaseInvoiceProcessingTaskApproveOrReject result $ PurchaseInvoiceStatus PurchaseInvoiceStatusInvoiceOpen
  case result of
    Nothing -> return $ Left invoiceVerified
    Just x -> return $ Right x

postPurchaseInvoicesR :: CompanyId -> Handler Value
postPurchaseInvoicesR companyId = do
  --invoice <- requireCheckJsonBody :: Handler PurchaseInvoice
  let invoice = defPurchaseInvoice 
  id <- runDB $ do
    entity <- insertEntity' invoice

    -- Let's check if someone has the right to verify
    setTasks
      entity
      PurchaseInvoiceProcessingTaskVerify
      {- [ PurchaseInvoiceProcessingTaskVerify,
        PurchaseInvoiceProcessingTaskApproveOrReject
      ]
 -}
    -- If verifier and approver are set we can change the status to Created
    update (entityKey entity) [PurchaseInvoiceDocumentStatus =. PurchaseInvoiceStatus PurchaseInvoiceStatusInvoiceCreated]
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
