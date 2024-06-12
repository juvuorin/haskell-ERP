{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BlockArguments #-}
{-# Language KindSignatures #-}
{-# Language ExistentialQuantification #-}
{-# Language ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}


module Handler.Invoice.Purchase.CrudEndpoints.PurchaseInvoices where

import Data.Aeson
import Database.Persist.Sql (
  fromSqlKey, toSqlKey,
 )
import GHC.Exts
import qualified GHC.OverloadedLabels
import Import
--import Handler.Task.Task
import Data.Kind (Type)
import Data.List (foldl)

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


data PurchaseInvoiceProcessingStatus'' (a::PurchaseInvoiceProcessingStatus) {- -> Type  -}where
  PurchaseInvoiceInVerification'' :: PurchaseInvoiceProcessingStatus'' 'PurchaseInvoiceProcessingStatusInvoiceInVerification
  PurchaseInvoiceInApproval'' :: PurchaseInvoiceProcessingStatus'' 'PurchaseInvoiceProcessingStatusInvoiceInApproval
  PurchaseInvoiceRejected'' :: PurchaseInvoiceProcessingStatus'' 'PurchaseInvoiceProcessingStatusInvoiceRejected
  PurchaseInvoiceOpen'' :: PurchaseInvoiceProcessingStatus'' 'PurchaseInvoiceProcessingStatusInvoiceOpen
  PurchaseInvoicePaid'' :: PurchaseInvoiceProcessingStatus'' 'PurchaseInvoiceProcessingStatusInvoicePaid

--data SDoorState :: DoorState -> Type where
--    SOpened :: SDoorState 'Opened
--    SClosed :: SDoorState 'Closed
--    SLocked :: SDoorState 'Locked

payAnyInvoice :: PurchaseInvoiceProcessingStatus'' s -> Invoice' s -> ()
payAnyInvoice status invoice = case status of
    _ -> () -- in this branch, s is 'Opened
  --  SClosed -> lockDoor door             -- in this branch, s is 'Closed
  --  SLocked -> door           

data WitnessStatus status where
  WitnessPurchaseInvoiceInVerification :: WitnessStatus 'PurchaseInvoiceProcessingStatusInvoiceInVerification
  WitnessPurchaseInvoiceInApproval :: WitnessStatus 'PurchaseInvoiceProcessingStatusInvoiceInApproval
  WitnessPurchaseInvoiceRejected :: WitnessStatus 'PurchaseInvoiceProcessingStatusInvoiceRejected
  WitnessPurchaseInvoiceOpen :: WitnessStatus 'PurchaseInvoiceProcessingStatusInvoiceOpen
  WitnessPurchaseInvoicePaid :: WitnessStatus 'PurchaseInvoiceProcessingStatusInvoicePaid

data Invoice' (status :: PurchaseInvoiceProcessingStatus) = Invoice'
  { 
    purchaseInvoice:: PurchaseInvoice
  , purchaseInvoiceStatus :: WitnessStatus status
  }



data SomeInvoice' where
  SomeInvoice' :: Invoice' a -> SomeInvoice'


getInvoice :: PurchaseInvoiceId -> DB SomeInvoice'
getInvoice id = do
  invoice <- getEntity404' id 
  let result = case purchaseInvoiceProcessingStatus (entityVal invoice) of
        PurchaseInvoiceProcessingStatusInvoiceOpen 
          -> (SomeInvoice' (Invoice' {purchaseInvoice = entityVal invoice, purchaseInvoiceStatus = WitnessPurchaseInvoiceOpen}))
        PurchaseInvoiceProcessingStatusInvoicePaid 
          ->(SomeInvoice' (Invoice' {purchaseInvoice = entityVal invoice, purchaseInvoiceStatus = WitnessPurchaseInvoicePaid}))                      
        PurchaseInvoiceProcessingStatusInvoiceInVerification 
          ->(SomeInvoice' (Invoice' {purchaseInvoice = entityVal invoice, purchaseInvoiceStatus = WitnessPurchaseInvoiceInVerification}))                    
        PurchaseInvoiceProcessingStatusInvoiceInApproval 
          -> (SomeInvoice' (Invoice' {purchaseInvoice = entityVal invoice, purchaseInvoiceStatus = WitnessPurchaseInvoiceInApproval}))
        PurchaseInvoiceProcessingStatusInvoiceRejected 
          -> (SomeInvoice' (Invoice' {purchaseInvoice = entityVal invoice, purchaseInvoiceStatus = WitnessPurchaseInvoiceRejected}))
  return result      
                  -- Add case alternatives for other possible values
        

fn3 :: Invoice' PurchaseInvoiceProcessingStatusInvoiceOpen -> DB ()
fn3 invoice = return ()

--getInvoice :: DB SomeInvoice'
--getInvoice = return $ SomeInvoice' (Invoice' {text="moi", purchaseInvoiceStatus = WitnessPurchaseInvoiceOpen})

test2 :: PurchaseInvoiceId -> DB ()
test2 invoiceId = do
  --let inv = getInvoice  --let inv = InvoiceOpen defInvoice 
  (SomeInvoice' inv2) <- getInvoice invoiceId
  case purchaseInvoiceStatus inv2 of 
    WitnessPurchaseInvoiceOpen -> fn3 inv2
    --_ -> error "problem"
  

data DoorState = Opened_ | Closed_ | Locked_
  deriving (Show, Eq)


data Door (s::DoorState) where
    UnsafeMkDoor :: { doorMaterial :: String } -> Door s

{- data PurchaseInvoiceProcessingStatus''' (status :: PurchaseInvoiceProcessingStatus) where
  PurchaseInvoiceInVerification''' :: InvoiceGADT a-> PurchaseInvoiceProcessingStatus''' 'PurchaseInvoiceProcessingStatusInvoiceInVerification
  PurchaseInvoiceInApproval''' :: InvoiceGADT a-> PurchaseInvoiceProcessingStatus''' 'PurchaseInvoiceProcessingStatusInvoiceInApproval
  PurchaseInvoiceRejected''' :: InvoiceGADT a -> PurchaseInvoiceProcessingStatus''' 'PurchaseInvoiceProcessingStatusInvoiceRejected
  PurchaseInvoiceOpen''' :: InvoiceGADT a-> PurchaseInvoiceProcessingStatus''' 'PurchaseInvoiceProcessingStatusInvoiceOpen
  PurchaseInvoicePaid''' :: InvoiceGADT a -> PurchaseInvoiceProcessingStatus''' 'PurchaseInvoiceProcessingStatusInvoicePaid
 -}

data InvoiceGADT (s::PurchaseInvoiceProcessingStatus) where
  MkInvoice  :: { entityInvoice :: Entity PurchaseInvoice }-> InvoiceGADT s

data SDoorState :: DoorState -> Type where
    SOpened :: SDoorState 'Opened_
    SClosed :: SDoorState 'Closed_
    SLocked :: SDoorState 'Locked_

closeDoor :: Door 'Opened_ -> Door 'Closed_
closeDoor door = UnsafeMkDoor (doorMaterial door)

lockDoor :: Door 'Closed_ -> Door 'Locked_
lockDoor door = UnsafeMkDoor (doorMaterial door)

lockAnyDoor :: SDoorState s -> Door s -> Door 'Locked_
{- lockAnyDoor sng door = case sng of
    SOpened -> lockDoor (closeDoor door) -- in this branch, s is 'Opened
    SClosed -> lockDoor door             -- in this branch, s is 'Closed
    SLocked -> door                      -- in this branch, s is 'Locked
 -}
lockAnyDoor SOpened  door = lockDoor (closeDoor door) -- in this branch, s is 'Opened
lockAnyDoor SClosed  door = lockDoor door             -- in this branch, s is 'Closed
lockAnyDoor SLocked  door = door                     -- in this branch, s is 'Locked

fn8:: ()

fn8 = do
  let locked = lockAnyDoor SOpened (UnsafeMkDoor "wooden")
  ()
{- fn4 :: Invoice' a -> DB ()
fn4 invoice = do
  let purchaseInvoice' = purchaseInvoice invoice

  return ()
 -}


-- User privileges for our users
data UserPrivilege = Member_ | Admin_ | Guest_

-- Our type witness
data WitnessPrivilege up where
  WitnessMember :: WitnessPrivilege Member_
  WitnessGuest :: WitnessPrivilege Guest_
  WitnessAdmin :: WitnessPrivilege Admin_

-- Our user type
data User' (up :: UserPrivilege) = User'
  { userId :: Integer
  , userName :: String
  , userPrivilege :: WitnessPrivilege up
  }

-- The type that we use to hide the privilege type variable
data SomeUser where
  SomeUser :: User' a -> SomeUser

-- A function that accept a user id (Integer), and reads
-- the corresponding user from the database. Note that the return
-- type level privilege is hidden in the return value `SomeUser`.
readUser :: Integer -> IO SomeUser
readUser userId = pure $ case find ((== userId) . (\(a, _, _) -> a)) dbRows of
  Just (id_, name_, type_) ->
    case type_ of
      "member" -> SomeUser (User' id_ name_ WitnessMember)
      "guest" -> SomeUser (User' id_ name_ WitnessGuest)
      "admin" -> SomeUser (User' id_ name_ WitnessAdmin)
  Nothing -> error "User not found"

-- This is a function that does not care
-- about user privilege
getUserName :: User' up -> String
getUserName = userName

-- This is a function only allows user
-- with Admin privilege.
deleteStuffAsAdmin :: User' 'Admin_ -> IO ()
deleteStuffAsAdmin _ = pure ()

main :: IO ()
main = do
  (SomeUser user) <- readUser 12

  print $ getUserName user -- We don't care about user privilege here

  case userPrivilege user of -- But here we do.
    -- So we bring the type-level user privilege in scope by matching
    -- on `userPrivilege` field and then GHC knows that `user`
    -- is actually `User 'Admin`, and so we can call `deleteStuffAsAdmin`
    -- with `user`.
    WitnessAdmin ->
      deleteStuffAsAdmin user
    _ -> error "Need admin user"

dbRows :: [(Integer, String, String)]
dbRows =
  [ (10, "John", "member")
  , (11, "alice", "guest")
  , (12, "bob", "admin")
  ]


doSomethingWithInvoice :: PurchaseInvoiceProcessingStatus' a -> Handler (PurchaseInvoiceProcessingStatus' a)
doSomethingWithInvoice = undefined

data Installment = Installment {amount::Double, dueDate::Day}

data PurchaseInvoicePaymentType (typ::PaymentInstallmentType) where
  PurchaseInvoiceWithoutInstallments :: PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceOpen -> PurchaseInvoicePaymentType 'WithoutInstallments
  PurchaseInvoiceWithInstallments :: PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceOpen -> [Installment]-> PurchaseInvoicePaymentType 'WithInstallments

data PaymentInstallmentType = WithInstallments | WithoutInstallments

data PayableInvoice where
  SomeInvoice :: PurchaseInvoicePaymentType a -> PayableInvoice

invoicesForPayment :: DB ()
invoicesForPayment = do
  companyId <- liftHandler $ getCompanyId
  openInvoices <- invoices companyId :: PurchaseInvoicesOpen
  let withInstallmentInfo = map PurchaseInvoiceWithoutInstallments openInvoices
  let someInvoices = map SomeInvoice withInstallmentInfo
  mapM sendToBankForPayment someInvoices
  return ()


invoicesForPayment' :: DB [InvoiceGADT 'PurchaseInvoiceProcessingStatusInvoiceOpen]
invoicesForPayment' = do
  companyId <- liftHandler $ getCompanyId
  openInvoices <- invoices companyId 
  return openInvoices
--  map (\invoice-> MkInvoice) openInvoices
  --let withInstallmentInfo = map PurchaseInvoiceWithoutInstallments openInvoices
  --let someInvoices = map SomeInvoice withInstallmentInfo
 -- mapM sendToBankForPayment someInvoices
--  return ()


--runDB $ update key [PurchaseInvoiceProcessingStatus =. PurchaseInvoiceProcessingStatusInvoicePaid]
--payInvoices' [] = return ()

payInvoices' entityInvoice =
    

    -- check if the invoice is open
    -- check if the invoice has installments
    -- implement code to pay an invoice with installments (PurchaseInvoice could have hasInstallments flag etc.)
    -- implement code to pay an invoice without installments
    
    undefined

sendToBankForPayment :: PayableInvoice -> DB ()
sendToBankForPayment (SomeInvoice (PurchaseInvoiceWithoutInstallments (PurchaseInvoiceOpen (Entity key invoice)))) 
    -- pay an invoice without installments (just 1 installment) using Banking API 1
    = error "not implemented"

sendToBankForPayment (SomeInvoice (PurchaseInvoiceWithInstallments (PurchaseInvoiceOpen (Entity key invoice)) installments)) 
    -- pay an invoice with 2 or more installments using Banking API 2
    = error "not implemented"
    

--payInvoices' :: Entity Invoice -> Handler ({- PurchaseInvoiceProcessingStatus' PurchaseInvoiceProcessingStatusInvoicePaid -})
--payInvoices' antities)))] =
--    runDB $ update key [PurchaseInvoiceProcessingStatus =. PurchaseInvoiceProcessingStatusInvoicePaid]

    --
    
    --return $ SomeInvoice (PurchaseInvoiceWithoutInstallments (PurchaseInvoiceOpen (Entity key newValue)))    
      

  --runDB $ do
    -- run payment side effects here
   -- update (entityKey invoice) [PurchaseInvoiceProcessingStatus =. PurchaseInvoiceProcessingStatusInvoicePaid]
   -- return $ PurchaseInvoicePaid invoice 

{- payInvoices [SomeInvoice (PurchaseInvoiceWithInstallments ent@(Entity key invoice) installments)] =
  runDB $ do
    -- run payment side effects here
    update key [PurchaseInvoiceProcessingStatus =. PurchaseInvoiceProcessingStatusInvoicePaid]
    return $ PurchaseInvoicePaid ent

 -} 
{- payInvoice ([SomeInvoice (PurchaseInvoiceSplit invoice installments)]) =  
  runDB $ do
    update (entityKey invoice) [PurchaseInvoiceProcessingStatus =. PurchaseInvoiceProcessingStatusInvoicePaid]
    return $ PurchaseInvoicePaid invoice
 -}


class Monad m => MonadInvoice m a where
  invoices :: CompanyId -> m [a]
  invoice :: CompanyId -> Key PurchaseInvoice -> m a

type PurchaseInvoicesDue = DB [PurchaseInvoicePaymentStatus' 'PurchaseInvoicePaymentStatusInvoiceDue]

type PurchaseInvoiceInVerificationTx = DB (PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceInVerification)
type PurchaseInvoiceVerifiedTx = DB (PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceVerified)
type PurchaseInvoiceInApprovalTx = DB (PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceInApproval)
type PurchaseInvoiceApprovedTx = DB (PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceApproved)
type PurchaseInvoiceOpenTx = DB (PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceOpen)
type PurchaseInvoicesOpenTx = DB [PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceOpen]
type PurchaseInvoicesOpen = DB [PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceOpen]

type PurchaseInvoicesInVerificationTx = DB [PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceInVerification]
type PurchaseInvoicesVerifiedTx = DB [PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceVerified]
type PurchaseInvoicesInApprovalTx = DB [PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceInApproval]
type PurchaseInvoicesApprovedTx = DB [PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceApproved]

type Created = [PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceCreated]
type PurchaseInvoicesCreatedTx = DB Created




instance MonadInvoice Handler (PurchaseInvoicePaymentStatus' 'PurchaseInvoicePaymentStatusInvoiceDue) where
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
{- instance MonadInvoice DB (PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceInVerification) where
  invoices :: CompanyId
-> DB
     [[PurchaseInvoiceProcessingStatus'
         'PurchaseInvoiceProcessingStatusInvoiceInVerification]]
  invoices companyId = do
    invoices <- selectList [PurchaseInvoiceCompanyId ==. companyId, PurchaseInvoiceProcessingStatus ==. PurchaseInvoiceProcessingStatusInvoiceInVerification] [] :: DB [Entity PurchaseInvoice]
    return $ map PurchaseInvoiceInVerification invoices

 -}
instance MonadInvoice DB (PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceInVerification) where
  invoice companyId invoiceId = do
    invoice <- selectFirst [PurchaseInvoiceId ==. invoiceId, PurchaseInvoiceCompanyId ==. companyId, PurchaseInvoiceProcessingStatus ==. PurchaseInvoiceProcessingStatusInvoiceInVerification] [] -- :: DB (Maybe (Entity PurchaseInvoice))
    case invoice of
      Just invoice -> return $ PurchaseInvoiceInVerification invoice
      Nothing -> sendResponseStatus status404 ("Invoice not found" :: Text)

instance MonadInvoice DB (InvoiceGADT PurchaseInvoiceProcessingStatusInvoiceInVerification) where
  invoice companyId invoiceId = do
    invoice <- selectFirst [PurchaseInvoiceId ==. invoiceId, PurchaseInvoiceCompanyId ==. companyId, PurchaseInvoiceProcessingStatus ==. PurchaseInvoiceProcessingStatusInvoiceInVerification] [] -- :: DB (Maybe (Entity PurchaseInvoice))
    case invoice of
      Just invoice -> return $ MkInvoice invoice
      Nothing -> sendResponseStatus status404 ("Invoice not found" :: Text)

instance MonadInvoice DB (PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceInApproval) where
  invoice companyId invoiceId = do
    invoice <- selectFirst [PurchaseInvoiceId ==. invoiceId,PurchaseInvoiceCompanyId ==. companyId, PurchaseInvoiceProcessingStatus ==. PurchaseInvoiceProcessingStatusInvoiceInApproval] [] -- :: DB (Maybe (Entity PurchaseInvoice))
    case invoice of
      Just invoice -> return $ PurchaseInvoiceInApproval invoice
      Nothing -> sendResponseStatus status404 ("Invoice not found" :: Text)

instance MonadInvoice DB (InvoiceGADT PurchaseInvoiceProcessingStatusInvoiceInApproval) where
  invoice companyId invoiceId = do  
    invoice <- selectFirst [PurchaseInvoiceId ==. invoiceId,PurchaseInvoiceCompanyId ==. companyId, PurchaseInvoiceProcessingStatus ==. PurchaseInvoiceProcessingStatusInvoiceInApproval] [] -- :: DB (Maybe (Entity PurchaseInvoice))
    case invoice of
        Just invoice -> return $ MkInvoice invoice
        Nothing -> sendResponseStatus status404 ("Invoice not found" :: Text)



instance MonadInvoice DB (PurchaseInvoiceProcessingStatus' 'PurchaseInvoiceProcessingStatusInvoiceOpen) where
  invoices companyId = do
    invoices_ <- selectList [PurchaseInvoiceCompanyId ==. companyId, PurchaseInvoiceProcessingStatus ==. PurchaseInvoiceProcessingStatusInvoiceOpen] [] -- :: DB (Maybe (Entity PurchaseInvoice))
    return $ map PurchaseInvoiceOpen invoices_

instance MonadInvoice DB (InvoiceGADT PurchaseInvoiceProcessingStatusInvoiceOpen) where
  invoices companyId = do
    invoices_ <- selectList [PurchaseInvoiceCompanyId ==. companyId, PurchaseInvoiceProcessingStatus ==. PurchaseInvoiceProcessingStatusInvoiceOpen] [] -- :: DB (Maybe (Entity PurchaseInvoice))
    return $ map MkInvoice invoices_

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

allGoes :: PurchaseInvoiceProcessingStatus' a -> DB ()
allGoes (PurchaseInvoiceInVerification invoice) = do
  processTasks invoice PurchaseInvoiceProcessingTaskVerified PurchaseInvoiceProcessingStatusInvoiceVerified


verifyInvoice :: CompanyId 
          -> PurchaseInvoiceId 
          -- -> {- InvoiceGADT PurchaseInvoiceProcessingStatusInvoiceInVerification-} 
          -> DB (Either (InvoiceGADT PurchaseInvoiceProcessingStatusInvoiceInVerification) (InvoiceGADT PurchaseInvoiceProcessingStatusInvoiceVerified))
verifyInvoice companyId invoiceId = do
  invoiceInVerification <- (invoice companyId invoiceId) :: DB (InvoiceGADT PurchaseInvoiceProcessingStatusInvoiceInVerification) 
  result<-processTasks' invoiceInVerification PurchaseInvoiceProcessingTaskVerified PurchaseInvoiceProcessingStatusInvoiceVerified
  case result of
    Nothing-> return $ Left invoiceInVerification
    Just x-> return $ Right x

{- verifyAnyDocument :: CompanyId 
          -> Key a 
          -- -> {- InvoiceGADT PurchaseInvoiceProcessingStatusInvoiceInVerification-} 
          -> DB (Either (InvoiceGADT PurchaseInvoiceProcessingStatusInvoiceInVerification) (InvoiceGADT PurchaseInvoiceProcessingStatusInvoiceVerified))

verifyAnyDocument companyId invoiceId = do
  invoiceInVerification <- (invoice companyId invoiceId) :: DB (InvoiceGADT PurchaseInvoiceProcessingStatusInvoiceInVerification) 
  result<-processTasks' invoiceInVerification PurchaseInvoiceProcessingTaskVerified PurchaseInvoiceProcessingStatusInvoiceVerified
  case result of
    Nothing-> return $ Left invoiceInVerification
    Just x-> return $ Right x
 -}


postVerifyInvoiceR :: CompanyId -> PurchaseInvoiceId -> Handler ()
postVerifyInvoiceR companyId invoiceId = do
  runDB $ (invoice companyId invoiceId :: PurchaseInvoiceInVerificationTx) >>= verify
--  runDB $ getVerifiableInvoice' companyId invoiceId >>= verify'

postVerifyInvoiceR' :: CompanyId -> PurchaseInvoiceId -> Handler ()
postVerifyInvoiceR' companyId invoiceId = do
  result <- runDB $ verifyInvoice companyId invoiceId
  case result of
    Right _ -> sendResponseStatus status200 ("Invoice is successfully verified by all verifiers" :: Text) 
    Left _ -> sendResponseStatus status200 ("Invoice is successfully verified by you" :: Text)

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

reject' :: InvoiceGADT 'PurchaseInvoiceProcessingStatusInvoiceInApproval -> DB (InvoiceGADT 'PurchaseInvoiceProcessingStatusInvoiceRejected)
reject' invoice = do
  processTasks (entityInvoice invoice) PurchaseInvoiceProcessingTaskRejected PurchaseInvoiceProcessingStatusInvoiceRejected
  return $ MkInvoice (entityInvoice invoice) 

  
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













type DocumentId = Int64

setTasks :: (ToBackendKey SqlBackend a) => CompanyId -> Entity a -> [Task] -> DB ()
setTasks companyId entity tasks = do
  let documentId = fromSqlKey (entityKey entity)
  forM_ tasks $ \task -> do
    accessRightEntity <- selectFirst [AccessRightRight ==. task] []
    case accessRightEntity of
      Just accessRightEntity -> do
        roles <- selectList [AccessRightRoleAccessRightId ==. (entityKey accessRightEntity), AccessRightRoleCompanyId ==. companyId] []
        if null roles
          then sendResponseStatus status400 ("A role with " ++ show task ++ " right is missing. Please add.")
          else do
            let roleIds = map accessRightRoleRoleId (map entityVal roles)
            users <- selectList [UserCompanyCompanyId ==. companyId] []
            let userIds = map (userCompanyUserId . entityVal) users
            usersroles <- selectList [UserRoleUserId <-. userIds, UserRoleRoleId <-. roleIds] []
            if null usersroles
              then sendResponseStatus status400 ("A user with a role with" ++ show task ++ " right is missing. Please add.")
              else do
                let userIds = map userRoleUserId (map entityVal usersroles)
                keys <- mapM (\userId ->
                  do
                    -- Let's check if the user  already has the same task pending
                    taskEntity <- selectFirst [WorkQueueDocumentId ==. documentId,
                                              WorkQueueUserId ==. userId,
                                              WorkQueueTask ==. task,
                                              WorkQueueTaskcomplete ==. False] []
                    case taskEntity of
                      Nothing -> do
                        -- Add task
                        key <- insert (WorkQueue documentId userId task False Nothing False)
                        return ()
                      _ -> return ()
                      ) userIds

                return ()
      Nothing -> sendResponseStatus status400 ("AccessRight table is missing " ++ show task ++ " right. Please add.")
dependentTasks :: [[Task]]
dependentTasks = [[PurchaseInvoiceProcessingTaskApproved,PurchaseInvoiceProcessingTaskRejected]]

getDependentTasks :: Task ->  [Task]
getDependentTasks task = do
        let allTasks = foldl (\acc item -> if task `elem` item then item else acc
                                                  ) [] dependentTasks
        -- remove task from the allTasks list
        filter (/= task) allTasks

-- dependent tasks!
completeTaskOrFail :: DocumentId -> Task -> DB ()
completeTaskOrFail documentId task = do
  taskEntity <- findTaskEntityOrFail documentId task
  let taskType = workQueueTask (entityVal taskEntity)
  dependentEntities <- findDependentEntities documentId taskType

  if workQueueTaskcomplete (entityVal taskEntity)
    then sendResponseStatus status400 ("The user has already handled this task" :: Text)
    else do
      -- Update the task by compliting it
      update
        (entityKey taskEntity)
        [ WorkQueueTaskcomplete =. True
        , WorkQueueTaskresult =. Just task
        ]

      -- Update dependent tasks by completing them 
      mapM_ (\entity-> do
        update (entityKey entity) [WorkQueueTaskcomplete =. True, WorkQueueTaskresult =. Nothing] ) dependentEntities


findTaskEntityOrFail :: DocumentId -> Task -> DB (Entity WorkQueue)
findTaskEntityOrFail documentId task = do
  user <- liftHandler $ getAuthenticatedUser
  taskEntity <-
    selectFirst
      [ WorkQueueDocumentId ==.  documentId
      , WorkQueueTask ==. task
      , WorkQueueUserId ==. entityKey user
      ]
      []
  case taskEntity of
    Just task -> return task
    Nothing -> liftHandler $ sendResponseStatus status404 ("This document has not been set " ++ show task ++ " task")


findDependentEntities :: DocumentId -> Task -> DB [Entity WorkQueue]
findDependentEntities documentId task = do
  user <- liftHandler $ getAuthenticatedUser
  taskEntities <-
    selectList
      [ WorkQueueDocumentId ==. documentId
      , WorkQueueUserId ==. entityKey user
      ]
      []
  let delendentTasks' = getDependentTasks task
  return $ filter (\item->elem (workQueueTask (entityVal item)) delendentTasks') taskEntities

removeTask :: DocumentId -> Task -> DB ()
removeTask documentId task = do
  taskEntity <- findTaskEntityOrFail documentId task
  if workQueueTaskcomplete (entityVal taskEntity)
    then update (entityKey taskEntity) [WorkQueueRemoved =. True]
    else sendResponseStatus status400 ("Task must be completed before it can be removed" :: Text)

--instance SymbolToField "purchase_invoice_processing_status" PurchaseInvoice PurchaseInvoiceProcessingStatus where symbolToField = PurchaseInvoiceProcessingStatus
--instance SymbolToField "id" Association AssociationId where symbolToField = AssociationId

{- instance GHC.OverloadedLabels.IsLabel "id" (Association -> Key Association) where
    fromLabel = 
 -}

{- instance GHC.OverloadedLabels.IsLabel "purchase_invoice_processing_status" (PurchaseInvoice -> PurchaseInvoiceProcessingStatus) where
    fromLabel = purchaseInvoiceProcessingStatus
 -}

processTasks :: (ToBackendKey SqlBackend a) => Entity a -> Task -> PurchaseInvoiceProcessingStatus -> DB ()
processTasks entity task status = do
  let documentId = fromSqlKey $ entityKey entity
  completeTaskOrFail documentId task
  taskList <-
    selectList
      [ WorkQueueTask ==. task
      , WorkQueueDocumentId ==. documentId
      , WorkQueueTaskcomplete ==. False
      ]
      []
  if null taskList
    then do
      update (toSqlKey documentId) [PurchaseInvoiceProcessingStatus =. status]
      return ()
    else return ()


invoiceStatus :: InvoiceGADT s -> PurchaseInvoiceProcessingStatus
invoiceStatus (MkInvoice entity) = purchaseInvoiceProcessingStatus (entityVal entity)

processTasks' :: InvoiceGADT (_ :: PurchaseInvoiceProcessingStatus) -> Task -> PurchaseInvoiceProcessingStatus -> DB (Maybe (InvoiceGADT (_ :: PurchaseInvoiceProcessingStatus))) 
processTasks' gadtEntity task newStatus = do
  let (MkInvoice entity) = gadtEntity 
  let documentId = fromSqlKey $ entityKey entity
  completeTaskOrFail documentId task
  taskList <-
    selectList
      [ WorkQueueTask ==. task
      , WorkQueueDocumentId ==. documentId
      , WorkQueueTaskcomplete ==. False
      ]
      []
  if null taskList
    then do
      update (toSqlKey documentId) [PurchaseInvoiceProcessingStatus =. newStatus]
      let newVal = (entityVal entity) {purchaseInvoiceProcessingStatus = newStatus}
      return $ Just $ MkInvoice {entityInvoice=(Entity (entityKey entity) newVal)}-- return ()
    else return $ Nothing-- return ()


processTasks''' ::  GenericGADT (_ :: GenericStatus) -> Task -> GenericStatus -> DB (Maybe (GenericGADT (_ :: GenericStatus))) 
processTasks''' gadtEntity task newStatus = do
   case (gadtEntity) of
      MkDocument (Entity key value)->do
        completeTaskOrFail  (fromSqlKey key) task
        taskList <- selectList
          [ WorkQueueTask ==. task
          , WorkQueueDocumentId ==. (fromSqlKey key)
          , WorkQueueTaskcomplete ==. False
          ]
          []
        if null taskList
        then do
          update key [#status =. newStatus]
          newValue <- get404 key
          return $ Just $ MkDocument {entityDocument=Entity key newValue}
    
        else return $ Nothing
    

  
removeCompletedAction :: DocumentId -> Task -> DB ()
removeCompletedAction documentId task = do
  removeTask documentId task

