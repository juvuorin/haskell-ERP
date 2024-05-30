{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Invoice.Purchase.CrudEndpoints.PurchaseInvoices where

import Data.Aeson
import Database.Persist.Sql (
  fromSqlKey,
 )
import GHC.Exts
import Import
import Handler.Task.Task

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

data PurchaseInvoicePaymentStatus' (status :: PurchaseInvoicePaymentStatus) where
  PurchaseInvoiceDue :: Entity PurchaseInvoice -> PurchaseInvoicePaymentStatus' 'PurchaseInvoicePaymentStatusInvoiceDue

data PurchaseInvoiceProcessingStatus' (status :: PurchaseInvoiceProcessingStatus) where
  PurchaseInvoiceInVerification :: Entity PurchaseInvoice -> PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceInVerification
  PurchaseInvoiceInApproval :: Entity PurchaseInvoice -> PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceInApproval
  PurchaseInvoiceRejected :: Entity PurchaseInvoice -> PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceRejected
  PurchaseInvoiceOpen :: Entity PurchaseInvoice -> PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceOpen
  PurchaseInvoicePaid :: Entity PurchaseInvoice -> PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoicePaid

class Monad m => MonadInvoice m a where
  invoices :: CompanyId -> m a
  invoice :: CompanyId -> Key PurchaseInvoice -> m a

type PurchaseInvoicesDue = DB [PurchaseInvoicePaymentStatus' 'PurchaseInvoicePaymentStatusInvoiceDue]
type PurchaseInvoicesOpen = DB [PurchaseInvoicePaymentStatus' 'PurchaseInvoicePaymentStatusInvoiceOpen]

type PurchaseInvoiceInVerificationTx = DB (PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceInVerification)
type PurchaseInvoiceVerifiedTx = DB (PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceVerified)
type PurchaseInvoiceInApprovalTx = DB (PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceInApproval)
type PurchaseInvoiceApprovedTx = DB (PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceApproved)

type PurchaseInvoicesInVerificationTx = DB [PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceInVerification]
type PurchaseInvoicesVerifiedTx = DB [PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceVerified]
type PurchaseInvoicesInApprovalTx = DB [PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceInApproval]
type PurchaseInvoicesApprovedTx = DB [PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceApproved]

type Created = [PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceCreated]
type PurchaseInvoicesCreatedTx = DB Created

instance MonadInvoice Handler [PurchaseInvoicePaymentStatus' 'PurchaseInvoicePaymentStatusInvoiceDue] where
  invoices companyId = do
    invoices <- runDB $ selectList [PurchaseInvoiceCompanyId ==. companyId, PurchaseInvoicePaymentstatus ==. Just PurchaseInvoicePaymentStatusInvoiceDue] [] :: Handler [Entity PurchaseInvoice]
    return $ map PurchaseInvoiceDue invoices

{- instance MonadInvoice Handler [PurchaseInvoicePaymentStatus' 'PurchaseInvoicePaymentStatusInvoiceOpen] whe+re
  --getOpenInvoices:: MonadHandler m => CompanyId -> m ()
  invoices companyId = do
    invoices <- runDB $ selectList [PurchaseInvoiceCompanyId ==. companyId, PurchaseInvoicePaymentstatus ==. Just PurchaseInvoicePaymentStatusInvoiceOpen] [] :: Handler [Entity PurchaseInvoice]
    return $ map PurchaseInvoiceOpen invoices

instance MonadInvoice DB [PurchaseInvoicePaymentStatus' 'PurchaseInvoicePaymentStatusInvoiceDue] where
  --getOpenInvoices:: MonadHandler m => CompanyId -> m ()
  invoices companyId = do
    invoices <- selectList [PurchaseInvoiceCompanyId ==. companyId, PurchaseInvoicePaymentstatus ==. Just PurchaseInvoicePaymentStatusInvoiceDue] [] :: DB [Entity PurchaseInvoice]
    return $ map PurchaseInvoiceDue invoices
 -}
instance MonadInvoice DB [PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceInVerification] where
  invoices companyId = do
    invoices <- selectList [PurchaseInvoiceCompanyId ==. companyId, PurchaseInvoiceProcessingStatus ==. PurchaseInvoiceProcessingStatusInvoiceInVerification] [] :: DB [Entity PurchaseInvoice]
    return $ map PurchaseInvoiceInVerification invoices

instance MonadInvoice DB (PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceInVerification) where
  invoice companyId invoiceId = do
    invoice <- selectFirst [PurchaseInvoiceId ==. invoiceId, PurchaseInvoiceCompanyId ==. companyId, PurchaseInvoiceProcessingStatus ==. PurchaseInvoiceProcessingStatusInvoiceInVerification] [] -- :: DB (Maybe (Entity PurchaseInvoice))
    case invoice of
      Just invoice -> return $ PurchaseInvoiceInVerification invoice
      Nothing -> sendResponseStatus status404 ("Invoice not found" :: Text)

instance MonadInvoice DB (PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceInApproval) where
  invoice companyId invoiceId = do
    invoice <- selectFirst [PurchaseInvoiceId ==. invoiceId,PurchaseInvoiceCompanyId ==. companyId, PurchaseInvoiceProcessingStatus ==. PurchaseInvoiceProcessingStatusInvoiceInApproval] [] -- :: DB (Maybe (Entity PurchaseInvoice))
    case invoice of
      Just invoice -> return $ PurchaseInvoiceInApproval invoice
      Nothing -> sendResponseStatus status404 ("Invoice not found" :: Text)

getVerifiableInvoice :: (PersistQueryRead backend, MonadHandler m,  BaseBackend backend ~ SqlBackend) => CompanyId -> PurchaseInvoiceId -> ReaderT backend m (PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceInVerification)
getVerifiableInvoice companyId invoiceId = do
    invoice <- selectFirst [PurchaseInvoiceId ==. invoiceId, PurchaseInvoiceCompanyId ==. companyId, PurchaseInvoiceProcessingStatus ==. PurchaseInvoiceProcessingStatusInvoiceInVerification] [] -- :: DB (Maybe (Entity PurchaseInvoice))
    case invoice of
      Just invoice -> return $ PurchaseInvoiceInVerification invoice
      Nothing -> sendResponseStatus status404 ("Invoice not found" :: Text)

getVerifiableInvoice' :: (PersistQueryRead backend, MonadHandler m,  BaseBackend backend ~ SqlBackend) => CompanyId -> PurchaseInvoiceId -> ReaderT backend m (Entity PurchaseInvoice)
getVerifiableInvoice' companyId invoiceId = do
    invoice <- selectFirst [PurchaseInvoiceId ==. invoiceId, PurchaseInvoiceCompanyId ==. companyId, PurchaseInvoiceProcessingStatus ==. PurchaseInvoiceProcessingStatusInvoiceInVerification] [] -- :: DB (Maybe (Entity PurchaseInvoice))
    case invoice of
      Just invoice -> return $ invoice
      Nothing -> sendResponseStatus status404 ("Invoice not found" :: Text)

postPurchaseInvoicesR :: CompanyId -> Handler Value
postPurchaseInvoicesR companyId = do
  invoice <- requireCheckJsonBody :: Handler PurchaseInvoice
  id <- runDB $ do
    entity <- insertEntity invoice

    -- Let's check if someone has the right to verify invoice
    setTasks companyId entity   [PurchaseInvoiceProcessingTaskVerified,
                                 PurchaseInvoiceProcessingTaskApproved,
                                 PurchaseInvoiceProcessingTaskRejected]

    -- If verifier and approver are set we can change the status to InVerification
    update (entityKey entity) [PurchaseInvoiceProcessingStatus =. PurchaseInvoiceProcessingStatusInvoiceInVerification]
    return (entityKey entity)

  return $ toJSON (fromSqlKey id)

postVerifyInvoiceR :: CompanyId -> PurchaseInvoiceId -> Handler ()
postVerifyInvoiceR companyId invoiceId = do
  runDB $ (invoice companyId invoiceId :: PurchaseInvoiceInVerificationTx) >>= verify
  runDB $ getVerifiableInvoice' companyId invoiceId >>= verify'

postApproveInvoiceR :: CompanyId -> PurchaseInvoiceId -> Handler ()
postApproveInvoiceR companyId invoiceId = do
  runDB $ (invoice companyId invoiceId :: PurchaseInvoiceInApprovalTx) >>= approve

postRejectInvoiceR :: CompanyId -> PurchaseInvoiceId -> Handler ()
postRejectInvoiceR companyId invoiceId = do
  runDB $ (invoice companyId invoiceId :: PurchaseInvoiceInApprovalTx) >>= reject

verify :: PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceInVerification -> DB ()
verify (PurchaseInvoiceInVerification invoice) = do
  processTasks invoice PurchaseInvoiceProcessingTaskVerified PurchaseInvoiceProcessingStatusInvoiceVerified 

verify' :: Entity PurchaseInvoice -> DB ()
verify' invoice = do
  processTasks invoice PurchaseInvoiceProcessingTaskVerified PurchaseInvoiceProcessingStatusInvoiceVerified 

approve :: PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceInApproval -> DB ()
approve (PurchaseInvoiceInApproval invoice) = do
  processTasks invoice PurchaseInvoiceProcessingTaskApproved PurchaseInvoiceProcessingStatusInvoiceApproved 

reject :: PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceInApproval -> DB ()
reject (PurchaseInvoiceInApproval invoice) = do
  processTasks invoice PurchaseInvoiceProcessingTaskRejected PurchaseInvoiceProcessingStatusInvoiceRejected 

test :: CompanyId -> Handler ()
test companyId = runDB $ do
  --duePurchaseInvoices <- invoices companyId :: PurchaseInvoicesDue
  --openPurchaseInvoices <- invoices companyId :: PurchaseInvoicesOpen
  return ()

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

postPurchaseInvoiceDetailsR :: CompanyId -> PurchaseInvoiceId -> Handler ()
postPurchaseInvoiceDetailsR companyId invoiceId = do
  details <- requireCheckJsonBody :: Handler [PurchaseInvoiceDetail]
  rawIds <-
    runDB $ do
      purchaseInvoiceDetailsIds <- insertMany (details)
      return $ show $ map (fromSqlKey) purchaseInvoiceDetailsIds
  sendResponseStatus status201 (rawIds)

putPurchaseInvoiceDetailR :: CompanyId -> PurchaseInvoiceDetailId -> Handler ()
putPurchaseInvoiceDetailR companyId purchaseInvoiceDetailId = do
  invoicedetail <- (requireCheckJsonBody :: Handler PurchaseInvoiceDetail)
  runDB $
    update
      purchaseInvoiceDetailId
      [ PurchaseInvoiceDetailGross =. purchaseInvoiceDetailGross invoicedetail
      , PurchaseInvoiceDetailNet =. purchaseInvoiceDetailNet invoicedetail
      , PurchaseInvoiceDetailQuantity
          =. purchaseInvoiceDetailQuantity invoicedetail
      , PurchaseInvoiceDetailVatamount
          =. purchaseInvoiceDetailVatamount invoicedetail
      , PurchaseInvoiceDetailVatPctId
          =. purchaseInvoiceDetailVatPctId invoicedetail
      , PurchaseInvoiceDetailProductname
          =. purchaseInvoiceDetailProductname invoicedetail
      , 
        PurchaseInvoiceDetailProductId
          =. purchaseInvoiceDetailProductId invoicedetail
      ]
  sendResponseStatus status200 ("UPDATED" :: Text)

deletePurchaseInvoiceDetailR :: CompanyId -> PurchaseInvoiceDetailId -> Handler ()
deletePurchaseInvoiceDetailR companyId purchaseInvoiceDetailId = do
  runDB $ delete purchaseInvoiceDetailId
  sendResponseStatus status200 ("DELETED" :: Text)
