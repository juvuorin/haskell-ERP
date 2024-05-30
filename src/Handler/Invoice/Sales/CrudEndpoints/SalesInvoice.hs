{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

--{-# OPTIONS_GHC -Wno-orphans #-}

module Handler.Invoice.Sales.CrudEndpoints.SalesInvoice where
import Import

import System.Directory
    ( doesDirectoryExist,
      listDirectory,
      removeDirectoryRecursive,
      removeFile )
import Database.Persist.Sql (fromSqlKey)
import Data.Aeson(Value(Object))
import qualified Data.HashMap.Lazy as HML
import Common (Extension, extend, CoherentObject (requireCheckJsonBodyForceCoherence))
import Data.List.Split.Internals (Chunk(Text))
import External.Finvoice.Maventa (sendInvoiceRequest')
import Import (EntityField(SalesInvoiceCompanyId), SalesInvoice (salesInvoiceDuedate))
import Database.Persist.Sql
import Data.Time
import ClassyPrelude.Yesod (sendResponseStatus)
import Control.Monad.State
import Language.Haskell.TH (uInfixE)
import ClassyPrelude (undefined)
-- Rename Order to OrderData and remove the status and shipmentInfo


--data SalesInvoiceData = SalesInvoiceData
--  {
   --  status :: InvoiceStatus 
    -- status :: OrderStatus,
    -- shipmentInfo :: Maybe ShipmentInfo
 -- }

--data PurchaseInvoiceDueStatus =  DuePurchase | OverduePurchase
--data SalesInvoiceDueStatus =  OpenSalesI| OverOpenSales

--test2 :: CompanyId -> Handler ()
--test2 companyId = do
--  Open <- getOpenInvoices companyId 
--  return ()           


{- type OpenSalesInvoice = SalesInvoicePaymentStatus' 'OpenSales



instance Invoices OpenSalesInvoice SalesInvoice where
  getOpen companyId = do
    invoices <- runDB $ selectList [SalesInvoiceCompanyId ==. companyId,SalesInvoiceStatus ==. OpenSales] [] :: Handler [Entity SalesInvoice]  
    return $ map (\invoice -> OpenSalesInvoice invoice) invoices 
 -}
--  getDueDate :: SomeInvoice -> Handler Day

--class Duedate a where
--  getDueDate ::  a-> Handler Day

--class Invoicing a where 
  --getAllOverdue :: CompanyId -> SomeInvoice -> Handler [a]
 -- getDueInfo :: SomeInvoice -> Handler [SomeInvoiceWithDueStatus] 
--  getDueDate :: Entity b -> Handler Day

--DueInvoice :: SomeInvoice -> SomeInvoiceDueStatus' 'SomeOverdue
--OverdueInvoice :: SomeInvoice -> SomeInvoiceDueStatus' 'SomeDue

{- data SomeInvoice' invoice where
  SomeSalesInvoice' :: Entity SalesInvoice -> SomeInvoice' 'SalesType
  SomePurchaseInvoice' :: Entity PurchaseInvoice -> SomeInvoice' 'PurchaseType
 -}

{-  SomeSalesInvoice' :: Entity SalesInvoice -> SomeInvoice' 'SalesType
  SomePurchaseInvoice' :: Entity PurchaseInvoice -> SomeInvoice' 'PurchaseType
 -}

{- getDueDate :: SomeInvoice -> Handler Day
getDueDate invoice = do

     case invoice of 
        SomeInvoice (SomeSalesInvoice' x) -> do
          let due = salesInvoiceDuedate (entityVal x)
          case due of 
            Just x ->  return x
            Nothing -> sendResponseStatus status404 ("Sales invoice does not have a due date"::Text) 
 
        SomeInvoice (SomePurchaseInvoice' x) -> do
          let due = purchaseInvoiceDuedate (entityVal x)
          case due of 
            Just x -> return x
            Nothing -> sendResponseStatus  status404 ("Purchase invoice does not have a due date"::Text) 

 -}  
   --   Nothing -> sendResponseStatus status404 ("Sales invoice does not have a due date::Text")  -}
{- getDueDate (SomePurchaseInvoice' invoice) = do
    case purchaseInvoiceDuedate (entityVal invoice) of
      Just x -> return x
      Nothing -> sendResponseStatus status404 ("Purchase invoice does not have a due date::Text") 
 -}
 

{- instance Duedate (SomeInvoice' 'SalesType)  where 
  getDueDate invoice = do
    let (SomeSalesInvoice' x) = invoice
    case salesInvoiceDuedate (entityVal x) of
      Just x -> return x
      Nothing -> sendResponseStatus status404 ("Sales invoice does not have a due date::Text") 

instance Duedate (SomeInvoice' 'PurchaseType)  where 
  getDueDate invoice = do
    let (SomePurchaseInvoice' x) = invoice

    case purchaseInvoiceDuedate (entityVal x) of
      Just x -> return x
      Nothing -> sendResponseStatus status404 ("Purchase invoice does not have a due date::Text") 
 
 
 -}--instance Invoicing SomeInvoice where
--  getAllOverdue :: [SomeInvoice] -> Handler [SomeInvoice]

{-   getInvoicesAllTypes companyId = do 
      sales <- runDB $ selectList [SalesInvoiceCompanyId ==. companyId] [] :: Handler [Entity SalesInvoice]
      purchase <- runDB $ selectList [PurchaseInvoiceCompanyId ==. companyId] [] :: Handler [Entity PurchaseInvoice]
      let sales' = map (\invoice -> SomeInvoice (SomeSalesInvoice' invoice)) sales  
      let purchase' = map (\invoice -> SomeInvoice (SomePurchaseInvoice' invoice)) purchase 
      return (sales'++purchase')
 -}  
  

{- calculateOverDue :: SomeInvoice  -> Handler SomeInvoiceWithDueStatus

calculateOverDue invoice = do
    dueDate <- getDueDate invoice
    currentDate <- liftIO $ currentDay
    let totalDifferenceInDays = diffDays dueDate currentDate 
    
    if totalDifferenceInDays < 0 then return $ SomeInvoiceWithDueStatus (EarlyInvoice invoice) 
      else if totalDifferenceInDays == 0 then return $ SomeInvoiceWithDueStatus (DueInvoice invoice)
      else return $ SomeInvoiceWithDueStatus (OverdueInvoice invoice)

 -}
 



{- class InvoicePaymentStatus a where 
  markAsPaid :: a -> Handler ()
  markAsOpen :: a -> Handler ()

instance InvoicePaymentStatus (SalesInvoicePaymentStatus' 'PaidSales)  where
  markAsOpen  x = do
    let (PaidSalesInvoice invoice) = x
    runDB $ update (entityKey invoice) [SalesInvoiceStatus =. OpenSales]
 
instance InvoicePaymentStatus (SalesInvoicePaymentStatus' 'OpenSales)  where
  markAsPaid  x =  do
    let (OpenSalesInvoice invoice) = x
    runDB $ update (entityKey invoice) [SalesInvoiceStatus =. PaidSales]
 
instance InvoicePaymentStatus (PurchaseInvoice' 'PaidPurchase)  where
  markAsOpen  x = do
    let (PaidPurchaseInvoice invoice) = x
    runDB $ update (entityKey invoice) [PurchaseInvoiceStatus =. OpenPurchase]

instance InvoicePaymentStatus (PurchaseInvoice' 'OpenPurchase)  where
  markAsPaid  x = do
    let (OpenPurchaseInvoice invoice) = x
    runDB $ update (entityKey invoice) [PurchaseInvoiceStatus =. PaidPurchase]


 -}{- testPaid companyId = do
  invoices <- getInvoices companyId [SalesInvoiceCompanyId ==. (toSqlKey 1)] :: Handler [SomeSalesInvoiceWithPaymentStatus]
  mapM_ (\invoice -> do
    case invoice of 
      SomeSalesInvoiceWithPaymentStatus (OpenSalesInvoice x) -> markAsPaid (OpenSalesInvoice x)
      SomeSalesInvoiceWithPaymentStatus (PaidSalesInvoice x) -> markAsOpen (PaidSalesInvoice x)) invoices
 -} 









{- calculateOverDue :: Entity SalesInvoice -> Handler SomeSalesInvoiceWithDueStatus

calculateOverDue invoice = do
    let dueDate = salesInvoiceDuedate (entityVal invoice)
    case dueDate of
      Just dueDate -> do

        currentDate <- liftIO $ currentDay
        let totalDifferenceInDays = diffDays dueDate currentDate 
        if totalDifferenceInDays < 0 then return $ SomeSalesInvoiceWithDueStatus (OpenSalesInvoice invoice)
        else return $ SomeSalesInvoiceWithDueStatus (OverOpenSalesInvoice invoice)
      Nothing -> sendResponseStatus status404 ("due date is missing from a sales invoice"::Text)
 -}
{- instance Invoices SomeSalesInvoiceWithDueStatus SalesInvoice where
  getInvoice invoiceId = do
    invoice <- runDB $ getEntity invoiceId
    case invoice of
      Just x -> calculateOverDue x
      Nothing -> sendResponseStatus status404 ("Cannot find invoice"::Text)

  getInvoices :: CompanyId -> [Filter SalesInvoice] -> Handler [SomeSalesInvoiceWithDueStatus] 
  getInvoices companyId filter = do 
    invoices <- runDB $ selectList filter [] -- :: [Entity SalesInvoice]
    mapM (\invoice-> calculateOverDue invoice) invoices
 -}

{- instance Invoices SomeInvoice SalesInvoice where
  getInvoice invoiceId = do
    invoice <- runDB $ getEntity invoiceId
    case invoice of
      Just x -> calculateOverDue x
      Nothing -> sendResponseStatus status404 ("Cannot find invoice"::Text)
 -}



{- getInvoicesWithPaymentStatus :: CompanyId -> [Filter SalesInvoice] -> Handler [SomeSalesInvoiceWithPaymentStatus]
getInvoicesWithPaymentStatus companyId filter = do 
  (getInvoices companyId filter) :: Handler [SomeSalesInvoiceWithPaymentStatus]
  

--getOpenInvoices :: CompanyId -> Handler [SomeSalesInvoiceWithPaymentStatus]
getOpenSalesInvoices :: CompanyId -> Handler [Entity SalesInvoice]
getOpenSalesInvoices companyId = do

--  OpenSalesInvoice :: Entity SalesInvoice -> SalesInvoicePaymentStatus' 'OpenSales
--  PaidSalesInvoice :: Entity SalesInvoice -> SalesInvoicePaymentStatus' 'PaidSales

  invoices <- getInvoicesWithPaymentStatus companyId [] 
  return $ foldr (\invoice acc-> do
    case invoice of 
        SomeSalesInvoiceWithPaymentStatus (OpenSalesInvoice x) -> acc++[x]
        _ -> acc
      ) [] invoices  
   

--  (getInvoices companyId ([] :: [Filter SalesInvoice])) :: Handler [SomeSalesInvoiceWithPaymentStatus]



getInvoiceWithDueStatus salesInvoiceId = do 
  invoice <- getInvoice salesInvoiceId :: Handler SomeSalesInvoiceWithDueStatus
  return invoice
 -}
  --getInvoices companyId [] :: Handler [SomeSalesInvoiceWithDueStatus]


--    let dueDate = salesInvoiceDuedate (entityVal invoice)
--    currentDate <- currentDay
--    let totalDifferenceInDays = diffDays dueDate currentDate 
--    if totalDifferenceInDays < 0 then return $ SomeSalesInvoiceWithDueStatus (OpenSalesInvoice x)
--    else return $ SomeSalesInvoiceWithPDueStatus (OverOpenSalesInvoice x)
     
--  getInvoices companyId filter = do 
--    invoices <- runDB $ selectList filter [] -- :: [Entity SalesInvoice]
 --   let invoices'' = map (entityVal) invoices
  --  mapM (\invoice-> return $ calculateOverDue invoice)
      
--    return all







{- instance Invoices SomeSalesInvoiceWithPaymentStatus SalesInvoice where
  getInvoice invoiceId = do
    invoice <- runDB $ getEntity invoiceId
    case invoice of
      Just x -> do
            case salesInvoiceStatus (entityVal x) of
              PaidSales -> return $ SomeSalesInvoiceWithPaymentStatus (PaidSalesInvoice x)
              OpenSales -> return $ SomeSalesInvoiceWithPaymentStatus (OpenSalesInvoice x)
      Nothing -> sendResponseStatus status404 ("Cannot find invoice"::Text)
 
  getInvoices companyId filter = do 
    invoices <- runDB $ selectList filter [] -- :: [Entity SalesInvoice]
 --   let invoices'' = map (entityVal) invoices
    let all = map (\invoice-> do
          case salesInvoiceStatus (entityVal invoice) of
            PaidSales -> SomeSalesInvoiceWithPaymentStatus (PaidSalesInvoice invoice)
            OpenSales -> SomeSalesInvoiceWithPaymentStatus (OpenSalesInvoice invoice)) invoices
    
      
    return all
 -}
{- instance Invoices SomePurchaseInvoice [Filter (Entity PurchaseInvoice)] where
  getInvoices companyId filter  = do
    invoices <- runDB $ selectList filter [] -- :: [Entity SalesInvoice]
    let invoices'' = map (entityVal) invoices
    let all = map (\invoice-> do
          case purchaseInvoiceStatus invoice of
            PaidPurchase -> SomePurchaseInvoice (PaidPurchaseInvoice invoice)
            OpenPurchase -> SomePurchaseInvoice (OpenPurchaseInvoice invoice)) invoices''
    
--    mapM_ (\item->print item) all
--    mapM_ (\invoice -> do
--      case (invoice) of 
 --         SomePurchaseInvoice (OpenPurchaseInvoice x) ->(markAsPaid (OpenPurchaseInvoice x))) all
      
    return all
 -}
{- postMarkAsPaid :: CompanyId -> SalesInvoiceId -> Handler ()
postMarkAsPaid companyId salesInvoiceId  = do
    invoice <- (getInvoice salesInvoiceId) :: Handler SomeSalesInvoiceWithPaymentStatus   
    case invoice of 
        SomeSalesInvoiceWithPaymentStatus (OpenSalesInvoice x) -> markAsPaid (OpenSalesInvoice x)
        _ -> sendResponseStatus status404 ("Cannot set set an invoice to paid if it is paid already"::Text) 

postMarkAsOpen :: CompanyId -> SalesInvoiceId -> Handler ()
postMarkAsOpen companyId salesInvoiceId  = do
    invoice <- (getInvoice salesInvoiceId) :: Handler SomeSalesInvoiceWithPaymentStatus   
    case invoice of 
        SomeSalesInvoiceWithPaymentStatus (PaidSalesInvoice x) -> markAsOpen (PaidSalesInvoice x)
        _ -> sendResponseStatus status404 ("Cannot set set an invoice to Open if it is Open already"::Text) 

 -}
postSendSalesInvoiceR :: CompanyId-> SalesInvoiceId -> Handler ()
postSendSalesInvoiceR companyId salesInvoiceId  = do
    sendInvoiceRequest' salesInvoiceId
    --sendResponseStatus status200 ("INVOICE SENT" :: Text)



putSalesInvoiceR :: CompanyId-> SalesInvoiceId -> Handler Value
putSalesInvoiceR cid id  = do
    invoice<- requireCheckJsonBody
    --invoice <- (requireCheckJsonBody :: Handler SalesInvoice)
    runDB $ replace id invoice
    sendResponseStatus status200 ("UPDATED" :: Text)

{-  customerId          PartnerId Maybe
    companyId           CompanyId Maybe 
    duedate             Day Maybe
    paymentdate        Day Maybe
    date                Day Maybe
    total               Double Maybe
    nettotal            Double Maybe
    vattotal            Double Maybe
    paymenttermId       PaymenttermId Maybe
    paymentterms        Text Maybe
    referencenumber     Text Maybe          -- should be fixed to RefNum tupe
    sellerreference     Text Maybe
    buyerreference      Text Maybe
    paymenttermId      paymenttermId Maybe
    paymentterms       Text Maybe
    penaltyinterest     Double Maybe
    notes               Text Maybe
    noticeperiod        Int Maybe
    deriving (Show)
 -}


merge :: [Value] -> Value
merge = Object . HML.unions . map (\(Object x) -> x)

getSalesInvoiceR :: CompanyId -> SalesInvoiceId -> Handler Value
getSalesInvoiceR cid id = do
    invoice <- runDB $ get404 id
    return $ object ["invoice" .= (Entity id invoice)]

getSalesInvoiceDetailsR:: CompanyId -> SalesInvoiceId -> Handler Value
getSalesInvoiceDetailsR companyId invoiceId = do
    let filePath =  "companies"</>(show $ fromSqlKey companyId)</>"invoices"</>(show $ fromSqlKey invoiceId)
    putStrLn (pack filePath)
  {-   isDirectoryPresent <- liftIO $ doesDirectoryExist filePath    
    files <- case isDirectoryPresent of
                    True ->  liftIO $ listDirectory filePath  
                    False -> return []
    putStrLn (pack $ show files)
   -}

    entries <- runDB $ selectList [SalesInvoiceDetailSalesInvoiceId==.invoiceId] [Asc SalesInvoiceDetailId] :: Handler [Entity SalesInvoiceDetail]
  
    return $ object ["rows" .= entries]


deleteSalesInvoiceR :: CompanyId -> SalesInvoiceId -> Handler Value
deleteSalesInvoiceR companyId docId = do
    runDB $ delete docId
    let filePath =  ("companies"</>(show companyId)</>"invoices"</>(show docId))
    removeDirectoryIfExists filePath
    sendResponseStatus status200 ("DELETED" :: Text)

