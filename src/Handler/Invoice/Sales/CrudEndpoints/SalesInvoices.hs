{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Invoice.Sales.CrudEndpoints.SalesInvoices where
import Import
import Database.Persist.Sql (ConnectionPool, runSqlPool, fromSqlKey, toSqlKey)
import Data.Fixed
import Data.Time
    ( TimeOfDay(TimeOfDay), gregorianMonthLength, timeOfDayToTime )
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import GHC.Exts 
import qualified Data.Vector as V
import qualified Import.NoFoundation as Handler
import Common

{- mkUTCTime :: (Integer, Int, Int)
          -> (Int, Int, Pico)
          -> UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
  UTCTime (fromGregorian year mon day)
          (timeOfDayToTime (TimeOfDay hour min sec))


getCurrentYear::Handler Integer
getCurrentYear = do
    let date = liftIO $ getCurrentTime >>= return . toGregorian . utctDay
    (year, month, day) <-  date
    return year
        
 -}
{- getOneMonthDates::Integer->Int->(UTCTime,UTCTime)
getOneMonthDates year month = do
    let days=gregorianMonthLength year month
    let timeStart = mkUTCTime (year, month, 1) (0, 0, 0)
    let timeEnd = mkUTCTime (year, month, days) (0, 0, 0)
    (timeStart,timeEnd)

getOneMonthDays::Integer->Int->(Day,Day)
getOneMonthDays year month = do

    let days=gregorianMonthLength year month
    let timeStart = fromGregorian year month 1 
    let timeEnd = fromGregorian year month days 
    (timeStart,timeEnd)

 -}
getSalesInvoicesR :: CompanyId-> Handler Value
getSalesInvoicesR companyId = do
    invoices<-runDB $ selectList [] [] :: Handler [Entity SalesInvoice]   
    returnJson invoices
{- PartnermersR :: CompanyId-> Handler Value
getCustomersR companyId = doPartner
    customerss<-runDB $ selectList [] [] :: Handler [Entity Customer]   
    returnJson customers
 -}

getProductsR :: CompanyId-> Handler Value
getProductsR companyId = do
    products<-runDB $ selectList [] [] :: Handler [Entity Product]   
    returnJson products

{- getPaymenttermsR :: CompanyId-> Handler Value
getPaymenttermsR companyId = do
    paymentterms<-runDB $ selectList [] [] :: Handler [Entity Paymentterm]   
    returnJson paymentterms
 -}
getDeliverytermsR :: CompanyId-> Handler Value
getDeliverytermsR companyId = do
    deliveryterms<-runDB $ selectList [] [] :: Handler [Entity Deliveryterm]   
    returnJson deliveryterms

getSalesInvoicesByMonthR :: CompanyId -> Int -> Integer -> Handler Value
getSalesInvoicesByMonthR id m y = do
    --year <-getCurrentYear
    --(timeStart, timeEnd) <-  getCurrentYear >>= (\x->return (getOneMonthDays x m))

    let (timeStart, timeEnd) =  getOneMonthDays y m
    

    transactions<-runDB $ selectList [ SalesInvoiceCompanyId==. id,
                                    SalesInvoiceDate >=. timeStart,
                                    SalesInvoiceDate <=. timeEnd] [] :: Handler [Entity SalesInvoice]   
    returnJson transactions

data MonthInfo = MonthInfo {month::Int} deriving (Show, Generic)
instance ToJSON MonthInfo
instance FromJSON MonthInfo

postSalesInvoicesByMonthJsonR :: CompanyId ->  Handler Value
postSalesInvoicesByMonthJsonR id = do
    monthInfo <- requireCheckJsonBody :: Handler MonthInfo    
    let m = month monthInfo
    year <-getCurrentYear
    let (timeStart, timeEnd) = getOneMonthDays year m
    transactions<-runDB $ selectList   [ SalesInvoiceCompanyId==.id
                                    , SalesInvoiceDate >=.  timeStart
                                    , SalesInvoiceDate <=.  timeEnd] [] :: Handler [Entity SalesInvoice]   
    returnJson transactions
    
    
postSalesInvoicesR :: CompanyId -> Handler ()
postSalesInvoicesR companyId = do  
    transaction <- requireCheckJsonBodyForceCoherence :: Handler SalesInvoice 
    id  <- runDB $ insert transaction
    sendResponseStatus status201 (object [ (.=) "id" (fromSqlKey id)])

data SalesInvoiceList = SalesInvoiceList [SalesInvoice]  deriving (Generic,Show)
instance FromJSON SalesInvoiceList 
instance ToJSON SalesInvoiceList

postSalesInvoiceDetailsR :: CompanyId-> SalesInvoiceId -> Handler ()
postSalesInvoiceDetailsR companyId id = do
      details <- requireCheckJsonBody::Handler [SalesInvoiceDetail]
      ids <-  runDB $ insertMany (details)
      let rawIds = show $ map (fromSqlKey) ids  
      sendResponseStatus status201 (rawIds) 

putSalesInvoiceDetailR :: CompanyId->SalesInvoiceDetailId ->Handler ()
putSalesInvoiceDetailR companyId id = do
    invoicedetail <- (requireCheckJsonBody :: Handler SalesInvoiceDetail)

   {-  gross               Double
    net                 Double
    vatpercentage       Double
    vatamount           Double
    productname         Text    
    quantity            Double
 -}
    runDB $ update id 
        [ SalesInvoiceDetailGross=.salesInvoiceDetailGross invoicedetail
         ,SalesInvoiceDetailNet=.salesInvoiceDetailNet invoicedetail
         ,SalesInvoiceDetailQuantity=.salesInvoiceDetailQuantity invoicedetail
         ,SalesInvoiceDetailVatamount=.salesInvoiceDetailVatamount invoicedetail
         ,SalesInvoiceDetailVatPctId=.salesInvoiceDetailVatPctId invoicedetail
         ,SalesInvoiceDetailProductname=.salesInvoiceDetailProductname invoicedetail
--         ,SalesInvoiceDetailVatId=.salesInvoiceDetailVatId invoicedetail
         ,SalesInvoiceDetailProductId=.salesInvoiceDetailProductId invoicedetail
         
         ]
        
    sendResponseStatus status200 ("UPDATED" :: Text)

      
deleteSalesInvoiceDetailR::CompanyId->SalesInvoiceDetailId -> Handler ()
deleteSalesInvoiceDetailR companyId salesInvoiceDetailId= do 
    runDB $ delete salesInvoiceDetailId    
    sendResponseStatus status200 ("DELETED" :: Text)


--TODO!! This does not work! Needs to get transactionId from invoice
putSalesInvoiceDetailsManyR :: CompanyId->Handler ()
putSalesInvoiceDetailsManyR companyId  = do
   --details <- requireCheckJsonBody::Handler SalesInvoiceList
   --   (o ::Result Details_) <-parseCheckJsonBody
   {-    case o of
            Success x -> do
                let ent = map (\i-> createSalesInvoice i) (details x)
                let ids = map (\i-> Handler.SalesInvoices.id i) (details x)
                let allData = zip ids ent
                _ <-mapM (\item->runDB $ replace (toSqlKey $ fst item) (snd item)) allData    
                return ()
            _ -> putStrLn ("Ei toiminut")
  -}
      --res<-parseMaybe (details o)
--      putStrLn (pack("PCKED:"++show o))
    --runDB $ mapM (\x->replace ( x) x) (getDetailsFromList details)
---    runDB $ putMany (getDetailsFromList details)
      sendResponseStatus status200 ("UPDATED" :: Text)
     
--    sendResponseStatus status201 (object ["ids" .=( fromSqlKey id)])
        
