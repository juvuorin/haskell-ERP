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
import qualified Data.List
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

class Monad m => ValidInvoice m a where
  invoices :: CompanyId -> m [a]
  invoice :: CompanyId -> Key PurchaseInvoice -> m a


instance ValidInvoice DB Created where
  invoice companyId invoiceId = do
    invoice <- selectFirst' [PurchaseInvoiceId ==. invoiceId, PurchaseInvoiceDocumentStatus ==. PurchaseInvoiceStatus PurchaseInvoiceStatusInvoiceCreated] [] -- :: DB (Maybe (Entity PurchaseInvoice))
    case invoice of
      Just invoice -> return $ MkDocument invoice
      Nothing -> sendResponseStatus status404 ("Invoice is not in created state or does not exist" :: Text)

instance ValidInvoice DB Verified where
  invoice companyId invoiceId = do
    invoice <- selectFirst' [PurchaseInvoiceId ==. invoiceId, PurchaseInvoiceDocumentStatus ==. PurchaseInvoiceStatus PurchaseInvoiceStatusInvoiceVerified] [] -- :: DB (Maybe (Entity PurchaseInvoice))
    case invoice of
      Just invoice -> return $ MkDocument invoice
      Nothing -> sendResponseStatus status404 ("Invoice is not in verified state or does not exist" :: Text)

instance ValidInvoice DB Open where
  invoices companyId = do
    invoices <- selectList' [PurchaseInvoiceDocumentStatus ==. PurchaseInvoiceStatus PurchaseInvoiceStatusInvoiceOpen] [] -- :: DB (Maybe (Entity PurchaseInvoice))
    return $ map MkDocument invoices

postVerifyInvoiceR :: CompanyId -> PurchaseInvoiceId -> Handler ()
postVerifyInvoiceR companyId invoiceId = do  
  result <- runDB $ invoice companyId invoiceId >>= verify 
  case result of
    Right _ -> sendResponseStatus status200 ("Invoice is successfully verified by all verifiers" :: Text)
    Left _ -> sendResponseStatus status200 ("Invoice is successfully verified by you" :: Text)

postApproveInvoiceR :: CompanyId -> PurchaseInvoiceId -> Handler ()
postApproveInvoiceR companyId invoiceId = do
  result <- runDB $ invoice companyId invoiceId >>= approve 
  case result of
    Right _ -> sendResponseStatus status200 ("Invoice is successfully approved by all approvers" :: Text)
    Left _ -> sendResponseStatus status200 ("Invoice is successfully approved by you" :: Text)

postRejectInvoiceR :: CompanyId -> PurchaseInvoiceId -> Handler ()
postRejectInvoiceR companyId invoiceId = do
  result <- runDB $ invoice companyId invoiceId >>= reject 
  case result of
    Right _ -> sendResponseStatus status200 ("Invoice is successfully rejected by all rejecters" :: Text)
    Left _ -> sendResponseStatus status200 ("Invoice is successfully rejected by you" :: Text)


getPreviousStatusByTask :: Task -> PurchaseInvoiceStatus
getPreviousStatusByTask task = case task of
  PurchaseInvoiceProcessingTaskVerify -> PurchaseInvoiceStatus PurchaseInvoiceStatusInvoiceCreated
  PurchaseInvoiceProcessingTaskApproveOrReject -> PurchaseInvoiceStatus PurchaseInvoiceStatusInvoiceVerified

getPreviousStatus purchaseInvoiceId = do
  user <- liftHandler $ getAuthenticatedUser
  let userId = entityKey user  
  taskGroupEntity <-
    selectList'
      [ 
        TaskGroupDocumentId ==. (fromSqlKey purchaseInvoiceId)    
      ]
      []
  case taskGroupEntity of
    l@(x:xs) -> do
      let entity = (Data.List.last l) 
      let lastTask = taskGroupTask (entityVal (Data.List.last l)) 
      let previousStatus = getPreviousStatusByTask lastTask
      setTasks entity lastTask 

      workQueueEntity <-
        selectList'
          [ 
            WorkQueueUserId ==. userId,
            WorkQueueTaskGroupId ==. entityKey entity,
            WorkQueueTaskcomplete ==. True

          ]
          []
      -- update invoice
      update purchaseInvoiceId [PurchaseInvoiceDocumentStatus =. previousStatus]


    [] -> sendResponseStatus status404 ("TaskGroup not found" :: Text)
  
  




{- postCancelTaskR :: CompanyId -> PurchaseInvoiceId -> Handler ()
postCancelTaskR companyId invoiceId = do

  result <- runDB $ invoice companyId invoiceId >>= cancel 
  result <- processTasks invoice PurchaseInvoiceProcessingTaskCancel 
                        (toTaskResult PurchaseInvoiceProcessingTaskResultInvoiceVerified) 
                        (PurchaseInvoiceStatus PurchaseInvoiceStatusInvoiceVerified)

  case result of
    Right _ -> sendResponseStatus status200 ("Invoice is successfully rejected by all rejecters" :: Text)
    Left _ -> sendResponseStatus status200 ("Invoice is successfully rejected by you" :: Text)

cancel :: Created -> DB (Either Created Verified)
cancel invoice = do
  result <- processTasks invoice PurchaseInvoiceProcessingTaskVerify 
                        (toTaskResult PurchaseInvoiceProcessingTaskResultInvoiceVerified) 
                        (PurchaseInvoiceStatus PurchaseInvoiceStatusInvoiceVerified)
  case result of
    Nothing -> return $ Left invoice
    Just x -> return $ Right x


 -}
class ToTaskResult a where
  toTaskResult :: a -> TaskResult 

instance ToTaskResult PurchaseInvoiceProcessingTaskResult where
  toTaskResult = PurchaseInvoiceProcessingTaskResult

getInvoices :: DB [Entity PurchaseInvoice]
getInvoices = selectList' [] []
 
verify :: Created -> DB (Either Created Verified)
verify invoice = do
  result <- processTasks invoice PurchaseInvoiceProcessingTaskVerify 
                        (toTaskResult PurchaseInvoiceProcessingTaskResultInvoiceVerified) 
                        (PurchaseInvoiceStatus PurchaseInvoiceStatusInvoiceVerified)
  case result of
    Nothing -> return $ Left invoice
    Just x -> return $ Right x

approve :: Verified -> DB (Either Verified Open)
approve invoice = do
  result <- processTasks invoice
                         PurchaseInvoiceProcessingTaskApproveOrReject 
                         (toTaskResult PurchaseInvoiceProcessingTaskResultInvoiceApproved)
                         (PurchaseInvoiceStatus PurchaseInvoiceStatusInvoiceOpen)
  case result of
    Nothing -> return $ Left invoice
    Just x -> return $ Right x

reject :: Verified -> DB (Either Verified Verified)
reject invoice = do
  result <- processTasks  invoice
                          PurchaseInvoiceProcessingTaskApproveOrReject
                          (toTaskResult PurchaseInvoiceProcessingTaskResultInvoiceRejected) 
                          (PurchaseInvoiceStatus PurchaseInvoiceStatusInvoiceOpen)
  case result of
    Nothing -> return $ Left invoice
    Just x -> return $ Right x
 


postPurchaseInvoicesR :: CompanyId -> Handler Value
postPurchaseInvoicesR companyId = do
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
