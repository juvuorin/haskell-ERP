{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module External.IncomeRegister.Types
    ( module External.IncomeRegister.Types
    ) where
import Import hiding (Id, Report, Content)
import Text.XML.HaXml.Schema.PrimitiveTypes 
import Text.XML.HaXml (Content)
import Text.XML.HaXml.Schema.Schema ( SchemaType(schemaTypeToXML) )
import Data.WageReportsToIR
import qualified Data.IRCommonTypes as Irct
import Data.WageReportsToIRTypes
import External.IncomeRegister.XmlUtils
import Text.XML.HaXml.Util (contentElem)
import Text.XML.HaXml.Posn (noPos)
import Text.XML.HaXml.Types hiding (Entity)
import qualified Data.PayerSummaryReportsToIRTypes as Summary
import Data.PayerSummaryReportsToIR
    ( elementToXMLPayerSummaryReportRequestToIR )
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import GHC.Base (Any)
import Import (PayeventId, Handler, Entity (Entity), Employee (Employee))
import Model (EntityField(EmployeeType))
import Prelude hiding (print, (++))
import Handler.Bookkeeping.InternalReporting.GeneralLedger
import Data.UUID.V4 (nextRandom)
import Data.Time
import Database.Persist.Sql (fromSqlKey)
--import qualified External.IncomeRegister.Types as External.IncomeRegister.PayerSummary
--import External.IncomeRegister.Types

--data IRPaymentData a = IRPaymentData a 

--data Jompikumpi = A PayeventId | B CompanyId

class Fetchable a b where
  getReportData :: a->Handler b 

class Reportable a  where
  buildReport  :: a->[Content()]
  service  :: a->String
  action  :: a->String

  

data BasicDataEarner = BasicDataEarner { 

                    incomeEarnerBasic_lastName :: String
                  , incomeEarnerBasic_firstName :: String
                  , deliveryData_timestamp :: String -- "2022-02-08T15:52:41Z"
                  , deliveryData_deliveryId :: String --show reportRef 
                  , deliveryData_productionEnvironment:: Bool
                  , id_code_earner :: String
                  , payerBasic_companyName :: String
                  , id_code_payer :: String -- "7017229-7"
       
                  , id_code_owner :: String  -- "7017229-7"
                  , id_code_creator:: String -- = "7017229-7"
                  , id_code_sender:: String -- = "7017229-7"

                  , address_street :: String-- = "Iidesranta 18 A 12"
                  , address_postalCode:: String  -- = "33100"
                  , address_postOffice:: String  -- = "Tampere"
           --       , address_countryCode:: String -- = "FI"

                  , paymentPeriod_paymentDate :: String--  = "2022-01-31"
                  , paymentPeriod_startDate :: String--  = "2022-01-01"
                  , paymentPeriod_endDate :: String-- =  "2022-01-01"

                  , contactPerson_name :: String-- = "Juuso Vuorinen"
                  , contactPerson_telephone :: String -- = "0504828940"
                  , reportData_reportId :: String
      --            , transBasic_transactionCodeTotalWage::Int -- = 101          -- palkka yhteissumma
                  , transBasic_amountTotal::Double -- = 1500.00 
      --            , transBasic_transactionCodeIncomeTax::Int -- = 402          -- tulovero ep.
                  , transBasic_amountIncomeTax::Double -- = 200
          --        , incomeEarnerBasic_companyName::Maybe String
                  , pensionPolicyNo::String
                  , employeetype::Maybe EmployeeType
      }



data BasicDataCompany = BasicDataCompany {           
            
              deliveryData_timestamp :: String -- "2022-02-08T15:52:41Z"
            , deliveryData_deliveryId :: String --show reportRef 
            , deliveryData_productionEnvironment:: Bool
            , deliveryData_reportDate::String
             
            , id_code_payer :: String -- "7017229-7"       
            , id_code_owner :: String  -- "7017229-7"
            , id_code_creator:: String -- = "7017229-7"
            , id_code_sender:: String -- = "7017229-7"

--            , paymentMonth_month::Int
--            , paymentMonth_year::Int

            , address_street :: String-- = "Iidesranta 18 A 12"
            , address_postalCode:: String  -- = "33100"
            , address_postOffice:: String  -- = "Tampere"

            , contactPerson_name :: String-- = "Juuso Vuorinen"
            , contactPerson_telephone :: String -- = "0504828940"

            , reportData_reportId :: String    

            , payerBasic_companyName::String
}



getBasicDataEarner:: PayeventId -> Handler BasicDataEarner
getBasicDataEarner payeventId = do
    -- IR Income types 101 (palkka) ja 412 (ennakonpidätys)
   
    -- Get the payevent with id payeventId
    payevent <- runDB $ get404 payeventId

    let paymentPeriod_paymentDate  = show $ payeventDate payevent
    let paymentPeriod_startDate = show $ payeventStartDate payevent
    let paymentPeriod_endDate =  show $ payeventEndDate payevent

    let transBasic_amountTotal = abs $ payeventGross payevent

    -- Look for "tax" deduction type in the set of payadjustment types 
    payadjustmenttype<-runDB $ selectFirst [PayadjustmenttypeCode==."tax"] [] :: Handler (Maybe (Entity Payadjustmenttype))

    -- If "tax" type is found in the set of payadjustmenttypes then look for the payadjustment with "tax" code in the payevent
    taxAdjustment <-
        case payadjustmenttype of
            Just x -> do
                let typeId = entityKey x
                runDB $ selectFirst[ PayadjustmentPayeventId==.payeventId,
                                                    PayadjustmentPayadjustmenttypeId==.typeId] [] :: Handler (Maybe (Entity Payadjustment))
            _ -> sendResponseStatus status200 ("Tax deduction type was not found int payadjustmenttypes" :: Text)

    -- Find income tax amount
    transBasic_amountIncomeTax <-
        case taxAdjustment of
            Just (Entity key adjustment) -> case (payadjustmentAmount adjustment) of
                Just amount -> return (abs amount)
                _ ->sendResponseStatus status200  ("Tax deduction amount has no value" :: Text)

            _ -> sendResponseStatus status200 ("Tax deduction entry was not found in payadjustments for this payevent" :: Text)

    earner<-runDB $ selectFirst [EmployeeId==.(payeventEmployeeId payevent)] [] :: Handler (Maybe (Entity Employee))


    (  incomeEarnerBasic_firstName
     , incomeEarnerBasic_lastName
     , id_code_earner
     , id_code_payer
     , payerBasic_companyName
     , contactPerson_telephone
     , contactPerson_name
     , address_street
     , address_postalCode
     , address_postOffice
     , pensionPolicyNo
     , employeetype)      
        <- case earner of
            Just (Entity id employee) -> do
                    company <- runDB $ get (employeeCompanyId employee)
                    case company of
                        Just company -> 
                            case (companyPensionpolicyno company) of
                                Just pensionPolicyNo ->
                                    return(unpack $ employeeFirstname employee,
                                    unpack $ employeeLastname employee,
                                    unpack $ employeeSocialsecurityid employee,
                                    unpack $ companyCompanyid company,
                                    unpack $ companyName company,
                                    unpack $ companyPhone company,
                                    unpack $ companyContact company,
                                    unpack $ companyAddress company,
                                    unpack $ companyZipcode company,
                                    unpack $ companyCity company,
                                    unpack $ pensionPolicyNo,
                                    employeeType employee)
                                _ -> sendResponseStatus status200 ("Pension policy number is not set" :: Text)                        
                        _ -> sendResponseStatus status200 ("Company not found in the set of companies" :: Text)
            _ -> sendResponseStatus status200 ("No earner found in employees table" :: Text)

    let id_code_owner = id_code_payer
    let id_code_creator = id_code_payer
    let id_code_sender = id_code_payer

    deliveryData_timestamp <- liftIO $ getZonedTime >>= (\t -> return $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Ez" $ t)

    reportRef1 <- liftIO $ nextRandom
    reportRef2 <- liftIO $ nextRandom

    companyId <- case earner of
        Just x -> return $ employeeCompanyId (entityVal x) 
        Nothing -> sendResponseStatus status200 ("Shit has hit the fan!" :: Text)        

    let deliveryData_deliveryId = show reportRef1
    let reportData_reportId = show reportRef2
    let deliveryData_productionEnvironment = if ((fromSqlKey companyId)/=5) then True else False

--    let incomeEarnerBasic_companyName = Nothing
--    incomeEarnerBasic_companyName
    -- let payerBasic_companyName = incomeEarnerBasic_companyName

    let report = BasicDataEarner {..}
    print "basic data OK"
    return report



getBasicDataCompany:: CompanyId -> Handler BasicDataCompany
getBasicDataCompany companyId = do

    -- Get the payevent with id payeventId
    --payevent <- runDB $ getJust payeventId

    --earner<-runDB $ selectFirst [EmployeeId==.(payeventEmployeeId payevent)] [] :: Handler (Maybe (Entity Employee))
    --companyId <- case earner of
    --    Just x -> return $ employeeCompanyId (entityVal x) 
    --    Nothing -> sendResponseStatus status200 ("Shit has hit the fan!" :: Text)        
                  
    (  
       id_code_payer
     , payerBasic_companyName
     , contactPerson_telephone
     , contactPerson_name
     , address_street
     , address_postalCode
     , address_postOffice
     )      
        <-  do
                company <- runDB $ get companyId
                case company of
                    Just company -> 
                        case (companyPensionpolicyno company) of
                            Just pensionPolicyNo ->
                                return(
                                unpack $ companyCompanyid company,
                                unpack $ companyName company,
                                unpack $ companyPhone company,
                                unpack $ companyContact company,
                                unpack $ companyAddress company,
                                unpack $ companyZipcode company,
                                unpack $ companyCity company
                            
                                )
                            _ -> sendResponseStatus status200 ("Pension policy number is not set" :: Text)        
                    _ -> sendResponseStatus status200 ("Company not found in the set of companies" :: Text)
           
    let id_code_owner = id_code_payer
    let id_code_creator = id_code_payer
    let id_code_sender = id_code_payer

    deliveryData_timestamp <- liftIO $ getZonedTime >>= (\t -> return $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Ez" $ t)

    -- Create hashes for report references
    reportRef1 <- liftIO $ nextRandom
    reportRef2 <- liftIO $ nextRandom
    let deliveryData_deliveryId = show reportRef1
    let reportData_reportId = show reportRef2

    let deliveryData_productionEnvironment = if ((fromSqlKey companyId) /=5) then True else False

    now <- liftIO $ getZonedTime

    let deliveryData_reportDate = show $ localDay $ zonedTimeToLocalTime now

    let (y, m , _)= toGregorian $ localDay $ zonedTimeToLocalTime now

  --  let paymentMonth_month = m
  --  let paymentMonth_year = fromInteger y
    let report = BasicDataCompany {..}
    print "basic company data OK"

    return report

instance Fetchable PayeventId ContractEmployerData  where 
  getReportData payeventId = do
    -- IR Income types 101 (palkka) ja 412 (ennakonpidätys)

    BasicDataEarner {..} <- getBasicDataEarner payeventId

    -- Look for "tyel" deduction type in the set of payadjustment types 
    payadjustmenttypeTyel<-runDB $ selectFirst [PayadjustmenttypeCode==."tyel"] [] :: Handler (Maybe (Entity Payadjustmenttype))

    tyelAdjustment <-
        case payadjustmenttypeTyel of
            Just x -> do
                let typeId = entityKey x
                runDB $ selectFirst[ PayadjustmentPayeventId==.payeventId,
                                                    PayadjustmentPayadjustmenttypeId==.typeId] [] :: Handler (Maybe (Entity Payadjustment))
            _ -> sendResponseStatus status200 ("Tyel deduction type was not found int payadjustmenttypes" :: Text)

    transBasic_amountPension <-
        case tyelAdjustment of
            Just (Entity key adjustment) -> case (payadjustmentAmount adjustment) of
                Just amount -> return (abs amount)
                _ -> sendResponseStatus status200  ("Pension deduction amount has no value" :: Text)

            _ -> sendResponseStatus status200 ("Pension deduction entry was not found in payadjustments for this payevent" :: Text)

    -- Look for "tvm" deduction type in the set of payadjustment types 
    payadjustmenttypeTvm<-runDB $ selectFirst [PayadjustmenttypeCode==."tvm"] [] :: Handler (Maybe (Entity Payadjustmenttype))

    tvmAdjustment <-
        case payadjustmenttypeTvm of
            Just x -> do
                let typeId = entityKey x
                runDB $ selectFirst[ PayadjustmentPayeventId==.payeventId,
                                                    PayadjustmentPayadjustmenttypeId==.typeId] [] :: Handler (Maybe (Entity Payadjustment))
            _ -> sendResponseStatus status200 ("Tvm deduction type was not found int payadjustmenttypes" :: Text)

    transBasic_amountUnemployment <-
        case tvmAdjustment of
            Just (Entity key adjustment) -> case (payadjustmentAmount adjustment) of
                Just amount -> return (abs amount)
                _ ->sendResponseStatus status200  ("Unemployment deduction amount has no value" :: Text)

            _ -> sendResponseStatus status200 ("Unemployment deduction entry was not found in payadjustments for this payevent" :: Text)


    let profession_code = "91110"
    let pensionInsur_pensionProvIdCode = read (Prelude.take 2 pensionPolicyNo)::Int  -- 54
    let pensionInsur_pensionPolicyNo = pensionPolicyNo -- "54-0000000U"
    let address_countryCode = "FI"

    Import.print transBasic_amountPension 
    Import.print transBasic_amountUnemployment
    Import.print transBasic_amountTotal
    Import.print transBasic_amountIncomeTax 
    let report = ContractEmployerData {..}

    liftIO $ Prelude.putStrLn (pack deliveryData_timestamp)

    return report

-- TODO instance Fetchable CompanyId PayerSummaryData where
{- toId :: CompanyId -> Int ->  Handler AccountId
toId companyId code= do
  result <- runDB $ selectFirst ([AccountCompanyId ==. companyId, AccountCode ==. code ]) []
  case result of
    Nothing -> sendResponseStatus status404 ("Cannot find account!"::Text)
    Just entity -> return $ entityKey entity
 -}

instance Fetchable CompanyId PayerSummaryData where
  getReportData companyId = do

      BasicDataCompany {..} <- getBasicDataCompany companyId
      let address_countryCode = "FI"
      time <- liftIO $ getZonedTime
      let (year, monthOfYear, _) = toGregorian $ localDay $ zonedTimeToLocalTime time
      let lastDay = addDays (-1) (fromGregorian year monthOfYear 1)
      
      -- | Payment month is the previous month, if decla
      let (paymentMonth_yearInteger, paymentMonth_month, _ ) = toGregorian lastDay 
      let paymentMonth_year = fromIntegral paymentMonth_yearInteger
      transBasic_amountHealthinsurance <- do
            accountId <- toId 2362 companyId                                              
            case keyFromValues [PersistInt64 $ fromSqlKey accountId, PersistDay lastDay]   of
              Right key -> do
                balance <- runDB $ get key
                case balance of
                  Just x -> return (-monthlyBalanceBalance x)
                  Nothing ->sendResponseStatus status404 ("Balance not available"::Text)
              Left _ -> sendResponseStatus status404 ("Cannot create compound key for Balance record"::Text)
                    
     
      let report = PayerSummaryData {..}

      print "PHASE2 OK"

      return report


instance Fetchable PayeventId YelEmployeeData  where 
  getReportData payeventId = do
    BasicDataEarner {..} <- getBasicDataEarner payeventId
    let address_countryCode = "FI"
    let incomeEarnerBasic_companyName = payerBasic_companyName

    let report = YelEmployeeData {..}
   
    print "YEL employee data OK"
    print report
    return report

wageReportService = "WageReportService.svc"
payerSummaryService = "PayerSummaryReportService.svc"
sendWageReportAction = "SendWageReport"
sendPayerSummaryReportAction = "SendPayerSummaryReport"

--data DataType = ContractEmployerData(..) | TemporaryEmployerData(..)

data ContractEmployerData = ContractEmployerData { 

              incomeEarnerBasic_lastName :: String
            , incomeEarnerBasic_firstName :: String
            , deliveryData_timestamp :: String -- "2022-02-08T15:52:41Z"
            , deliveryData_deliveryId :: String --show reportRef 
            , deliveryData_productionEnvironment:: Boolean
            , id_code_earner :: String
            , payerBasic_companyName :: String
            , id_code_payer :: String -- "7017229-7"
       
            , id_code_owner :: String  -- "7017229-7"
            , id_code_creator:: String -- = "7017229-7"
            , id_code_sender:: String -- = "7017229-7"

            , address_street :: String-- = "Iidesranta 18 A 12"
            , address_postalCode:: String  -- = "33100"
            , address_postOffice:: String  -- = "Tampere"
            , address_countryCode:: String -- = "FI"

            , paymentPeriod_paymentDate :: String--  = "2022-01-31"
            , paymentPeriod_startDate :: String--  = "2022-01-01"
            , paymentPeriod_endDate :: String-- =  "2022-01-01"

            , contactPerson_name :: String-- = "Juuso Vuorinen"
            , contactPerson_telephone :: String -- = "0504828940"
            , reportData_reportId :: String    
--            , transBasic_transactionCodeTotalWage::Int -- = 101          -- palkka yhteissumma
            , transBasic_amountTotal::Double -- = 1500.00 
--            , transBasic_transactionCodeIncomeTax::Int -- = 402          -- tulovero ep.
            , transBasic_amountIncomeTax::Double -- = 200
            , transBasic_amountPension::Double -- = 200
            , transBasic_amountUnemployment::Double -- = 200
--            , incomeEarnerBasic_companyName::Maybe String
            , profession_code::String
            , pensionInsur_pensionProvIdCode::Int  -- = Just pensionInsur_pensionProvIdCode
            , pensionInsur_pensionPolicyNo::String -- = Just $ Irct.PensionPolicyNo $ Normalized pensionInsur_pensionPolicyNo

}

data TemporaryEmployerData = TemporaryEmployerData { 

              incomeEarnerBasic_lastName :: String
            , incomeEarnerBasic_firstName :: String
            , deliveryData_timestamp :: String -- "2022-02-08T15:52:41Z"
            , deliveryData_deliveryId :: String --show reportRef 
            , deliveryData_productionEnvironment:: Boolean
            , id_code_earner :: String
            , payerBasic_companyName :: String
            , id_code_payer :: String -- "7017229-7"
       
            , id_code_owner :: String  -- "7017229-7"
            , id_code_creator:: String -- = "7017229-7"
            , id_code_sender:: String -- = "7017229-7"

            , address_street :: String-- = "Iidesranta 18 A 12"
            , address_postalCode:: String  -- = "33100"
            , address_postOffice:: String  -- = "Tampere"
            , address_countryCode:: String -- = "FI"

            , paymentPeriod_paymentDate :: String--  = "2022-01-31"
            , paymentPeriod_startDate :: String--  = "2022-01-01"
            , paymentPeriod_endDate :: String-- =  "2022-01-01"

            , contactPerson_name :: String-- = "Juuso Vuorinen"
            , contactPerson_telephone :: String -- = "0504828940"
            , reportData_reportId :: String    
--            , transBasic_transactionCodeTotalWage::Int -- = 101          -- palkka yhteissumma
            , transBasic_amountTotal::Double -- = 1500.00 
--            , transBasic_transactionCodeIncomeTax::Int -- = 402          -- tulovero ep.
            , transBasic_amountIncomeTax::Double -- = 200
            , transBasic_amountPension::Double -- = 200
            , transBasic_amountUnemployment::Double -- = 200
            , incomeEarnerBasic_companyName::String
            , profession_code::String
            , pensionInsur_pensionProvIdCode::Int  -- = Just pensionInsur_pensionProvIdCode
            , pensionInsur_pensionPolicyNo::String -- = Just $ Irct.PensionPolicyNo $ Normalized pensionInsur_pensionPolicyNo

} deriving (Show)

data YelEmployeeData = YelEmployeeData {

              incomeEarnerBasic_lastName :: String
            , incomeEarnerBasic_firstName :: String
            , deliveryData_timestamp :: String -- "2022-02-08T15:52:41Z"
            , deliveryData_deliveryId :: String --show reportRef 
            , deliveryData_productionEnvironment:: Boolean
            , id_code_earner :: String
            , payerBasic_companyName :: String
            , id_code_payer :: String -- "7017229-7"

            , id_code_owner :: String  -- "7017229-7"
            , id_code_creator:: String -- = "7017229-7"
            , id_code_sender:: String -- = "7017229-7"

            , address_street :: String-- = "Iidesranta 18 A 12"
            , address_postalCode:: String  -- = "33100"
            , address_postOffice:: String  -- = "Tampere"
            , address_countryCode:: String -- = "FI"

            , paymentPeriod_paymentDate :: String--  = "2022-01-31"
            , paymentPeriod_startDate :: String--  = "2022-01-01"
            , paymentPeriod_endDate :: String-- =  "2022-01-01"

            , contactPerson_name :: String-- = "Juuso Vuorinen"
            , contactPerson_telephone :: String -- = "0504828940"
            , reportData_reportId :: String
--            , transBasic_transactionCodeTotalWage::Int -- = 101          -- palkka yhteissumma
            , transBasic_amountTotal::Double -- = 1500.00 
--            , transBasic_transactionCodeIncomeTax::Int -- = 402          -- tulovero ep.
            , transBasic_amountIncomeTax::Double -- = 200
            , incomeEarnerBasic_companyName::String

} deriving (Show)


data PayerSummaryData = PayerSummaryData {
              deliveryData_timestamp :: String -- "2022-02-08T15:52:41Z"
            , deliveryData_deliveryId :: String --show reportRef 
            , deliveryData_productionEnvironment:: Boolean
            , deliveryData_reportDate::String
            , paymentMonth_month ::Int
            , paymentMonth_year ::Int
            , payerBasic_companyName :: String
            , id_code_payer :: String -- "7017229-7"
            , id_code_owner :: String  -- "7017229-7"
            , id_code_creator:: String -- = "7017229-7"
            , id_code_sender:: String -- = "7017229-7"
            , address_street :: String-- = "Iidesranta 18 A 12"
            , address_postalCode:: String  -- = "33100"
            , address_postOffice:: String  -- = "Tampere"
            , address_countryCode:: String -- = "FI"

            , paymentPeriod_paymentDate :: String--  = "2022-01-31"
            , paymentPeriod_startDate :: String--  = "2022-01-01"
            , paymentPeriod_endDate :: String-- =  "2022-01-01"

            , contactPerson_name :: String-- = "Juuso Vuorinen"
            , contactPerson_telephone :: String -- = "0504828940"
            , reportData_reportId :: String
            , transBasic_amountHealthinsurance::Double 
} deriving (Show)
--            , transBasic_transactionCodeTotalWage::Int -- = 101          -- palkka yhteissumma
--            , transBasic_amountTotal::Double -- = 1500.00 
--            , transBasic_transactionCodeIncomeTax::Int -- = 402          -- tulovero ep.
 --           , transBasic_amountIncomeTax::Double -- = 200
--            , incomeEarnerBasic_companyName::String

 --build :: ReportType->String
--build type' = 
--    case type' of 
---        YELInsured          x -> buildReport x
  --      TemporaryEmployer   x -> buildReport x 
  --      ContractEmployer    x -> buildReport x


instance Reportable YelEmployeeData where
    service x =  wageReportService
    action x = sendWageReportAction
    buildReport input = let
        YelEmployeeData {..} = input
{- 
        incomeEarnerBasic_lastName = "Jokela"
        incomeEarnerBasic_firstName = "Sirkku"
        deliveryData_timestamp = "2022-02-08T15:52:41Z"
        deliveryData_deliveryId = show reportRef1 
        deliveryData_productionEnvironment = False
        id_code_earner ="110849-882M"
        payerBasic_companyName = "Larlheinz 521 Testifirma OY"
        id_code_payer = "7017229-7"
    
        id_code_owner = "7017229-7"
        id_code_creator = "7017229-7"
        id_code_sender = "7017229-7"

        address_street = "Iidesranta 18 A 12"
        address_postalCode = "33100"
        address_postOffice = "Tampere"
        address_countryCode = "FI"

        paymentPeriod_paymentDate = "2022-01-31"
        paymentPeriod_startDate = "2022-01-01"
        paymentPeriod_endDate =  "2022-01-01"

        contactPerson_name = "Juuso Vuorinen"
        contactPerson_telephone = "0504828940"

        transBasic_transactionCodeTotalWage = 101          -- palkka yhteissumma
        transBasic_amountTotal = 1500.00 
        nsBasic_transactionCodeIncomeTax = 402          -- tulovero ep.
        transBasic_amountIncomeTax = 200

        reportData_reportId = show reportRef2
-}
        insuranceExceptions = InsuranceExceptions { insurExcept_exceptionCode = [ExceptionCode 3,ExceptionCode 4,ExceptionCode 5] }

        wageReportsToIR = WageReportsToIR
                { wageReportsToIR_deliveryData = deliveryData
                , wageReportsToIR_signature = Nothing  -- Just signatureType 
                }

        
        deliveryData = DeliveryData
                { deliveryData_timestamp = DateTime deliveryData_timestamp
                , deliveryData_source = Irct.String30 $ Normalized "EasyBooks"
                , deliveryData_type = 100
                , deliveryData_deliveryId = Irct.String40 $ Normalized $ deliveryData_deliveryId
                , deliveryData_faultyControl = 2
                , deliveryData_productionEnvironment = Irct.TrueOrFalse deliveryData_productionEnvironment
                , deliveryData_owner = id1
                , deliveryData_creator = id2
                , deliveryData_sender = id3
                , deliveryData_paymentPeriod = period
                , deliveryData_contactPersons = contactPersons
                , deliveryData_payer = payer
                , deliveryData_reports = reports
                }

        reports = Reports
                { reports_report = [report]
                }
        report = Report
                { report_data =  reportData
                , report_incomeEarner = incomeEarner
                , report_transactions   =Just transactions
                , report_foreignLeasedWork  = Nothing
                , report_stayPeriodsInFinland  = Nothing
                , report_workPeriodsInFinland  = Nothing
                , report_workCountries  = Nothing
                , report_absence  = Nothing
                }
        incomeEarnerBasic = IncomeEarnerBasic
                { incomeEarnerBasic_missingId = Nothing
                , incomeEarnerBasic_companyName =Just $ Irct.String200 $ Normalized incomeEarnerBasic_companyName
             --   , incomeEarnerBasic_companyName = Nothing

                , incomeEarnerBasic_lastName = Just $ Irct.String200 $ Normalized incomeEarnerBasic_lastName
                , incomeEarnerBasic_firstName = Just $ Irct.String100 $ Normalized incomeEarnerBasic_firstName
                , incomeEarnerBasic_birthDate = Nothing
                , incomeEarnerBasic_gender = Nothing
                }
        transactions = Transactions
                { transactions_transaction = [transaction'1,transaction'2]
                }

        reportData = ReportData
                { reportData_actionCode = 1
                , reportData_iRReportId  = Nothing
                , reportData_reportId = Just $ Irct.String40 $ Normalized $ reportData_reportId
                , reportData_reportVersion  = Nothing
                }

        incomeEarner = IncomeEarner
                { incomeEarner_ids = Just incomeEarnerIds
                , incomeEarner_basic  = Just incomeEarnerBasic
                , incomeEarner_addresses  = Nothing
                , incomeEarner_subOrgs  = Nothing
                , incomeEarner_employment  = Nothing
                , incomeEarner_professions  = {- Just professions -} Nothing
                , incomeEarner_employmentRegs  = Nothing
                , incomeEarner_placeOfBusiness  = Nothing
                , incomeEarner_pensionInsurance  = Just pensionInsurance
                , incomeEarner_accidentInsurance  = Nothing
                , incomeEarner_insuranceExceptions  = Just insuranceExceptions
                , incomeEarner_internationalData  = Nothing
                , incomeEarner_other  = Nothing
                }

        pensionInsurance :: PensionInsurance
        pensionInsurance = PensionInsurance
                { pensionInsur_pensionActCode = 3
                , pensionInsur_pensionProvIdCode = {- Just 54 -} Nothing
                , pensionInsur_pensionPolicyNo = {- Just $ Irct.PensionPolicyNo $ Normalized "54-0000000U" -} Nothing
                }

        incomeEarnerIds = IncomeEarnerIds
                { incomeEarnerIds_id = [earnerId]
                }
        earnerId = Id
                { id_type = 2
                , id_code = Irct.String30 $ Normalized id_code_earner
                , id_countryCode  = Nothing {- Just $ Irct.String2 $ XsdString "FI" -}
                , id_countryName  = Nothing
                }
        payer = Payer
                { payer_ids = Just ids
                , payer_basic = Just payerBasic
                , payer_address = Just address
                , payer_subOrgs  = Nothing
                , payer_other = Nothing --  Just payerOther
                , payer_substitutePayer  = Nothing
                }
        payerOther = PayerOther
                { payerOther_payerTypes = payerTypes
                }
        payerTypes = PayerTypes
                { payerTypes_code = [3]
                }
        ids = PayerIds
                { payerIds_id = [payerId]
                }
        payerId = Id
                { id_type = 1
                , id_code = Irct.String30 $ Normalized id_code_payer
                , id_countryCode  = {- Just $ Irct.String2 $ XsdString "FI" -- -}Nothing
                , id_countryName  = Nothing
                }
        payerBasic = PayerBasic
                { payerBasic_missingId   = Nothing
                , payerBasic_companyName = Just $ Irct.String200 $ Normalized payerBasic_companyName
                , payerBasic_lastName  = Nothing
                , payerBasic_firstName  = Nothing
                , payerBasic_birthDate  = Nothing
                , payerBasic_language = Just 1
                }
        address = Address
                { address_co = Nothing
                , address_street = Just $ Irct.String100 $ Normalized address_street
                , address_pOBox = Nothing
                , address_postalCode = Irct.String20 $ Normalized address_postalCode
                , address_postOffice = Irct.String200 $ Normalized address_postOffice
                , address_countryCode = Just $ Irct.String2 $ XsdString address_countryCode
                , address_countryName = Nothing
                }
        period = PaymentPeriod
                { paymentPeriod_paymentDate = Date paymentPeriod_paymentDate
                , paymentPeriod_startDate = Date paymentPeriod_startDate
                , paymentPeriod_endDate = Date paymentPeriod_endDate
                }
        contactPersons =  ContactPersons
                { contactPersons_contactPerson =  [person]
                }
        person = ContactPerson
                { contactPerson_name = Irct.String200 $ Normalized contactPerson_name
                , contactPerson_telephone  = Irct.String40 $ Normalized contactPerson_telephone
                , contactPerson_email = Nothing
                , contactPerson_responsibilityCode = Nothing
                }
        id1 = Id
                { id_type = 1
                , id_code = Irct.String30 $ Normalized id_code_owner
                , id_countryCode = {- Just $ Irct.String2 $ XsdString "FI" -} Nothing
                , id_countryName = Nothing
                }
        id2 = Id
                { id_type = 1
                , id_code = Irct.String30 $ Normalized id_code_creator
                , id_countryCode  ={-  Just $ Irct.String2 $ XsdString "FI" -} Nothing
                , id_countryName  = Nothing
                }
        id3 = Id
                { id_type = 1
                , id_code = Irct.String30 $ Normalized id_code_sender
                , id_countryCode  = {- Just $ Irct.String2 $ XsdString "FI" -} Nothing
                , id_countryName  = Nothing

                }

        transaction'1 = Data.WageReportsToIRTypes.Transaction
                { transaction_basic =  transactionBasic1
                , transaction_insuranceData = Nothing
                , transaction_earningPeriods =Nothing
                , transaction_unitWages =     Nothing
                , transaction_carBenefit =    Nothing
                , transaction_mealBenefit =   Nothing
                , transaction_otherBenefit =  Nothing
                , transaction_sailorIncome =  Nothing
                , transaction_recoveryData =  Nothing
                , transaction_dailyAllowance =Nothing
                , transaction_kmAllowance =   Nothing
                , transaction_sixMonthRule =  Nothing
                }

        transaction'2 = Data.WageReportsToIRTypes.Transaction
                { transaction_basic =  transactionBasic2
                , transaction_insuranceData = Nothing
                , transaction_earningPeriods =Nothing
                , transaction_unitWages =     Nothing
                , transaction_carBenefit =    Nothing
                , transaction_mealBenefit =   Nothing
                , transaction_otherBenefit =  Nothing
                , transaction_sailorIncome =  Nothing
                , transaction_recoveryData =  Nothing
                , transaction_dailyAllowance =Nothing
                , transaction_kmAllowance =   Nothing
                , transaction_sixMonthRule =  Nothing
                }



        transactionBasic1 = TransactionBasic
                { transBasic_transactionCode = 101 --transBasic_transactionCodeTotalWage          -- palkka yhteissumma
                , transBasic_amount = Just $ Irct.Decimal2 $ Decimal transBasic_amountTotal
                , transBasic_noMoney = Nothing
                , transBasic_oneOff = Nothing
                , transBasic_unjustEnrichment = Nothing
                , transBasic_recovery = Nothing
                }
        transactionBasic2 = TransactionBasic
                { transBasic_transactionCode = 402 -- transBasic_transactionCodeIncomeTax
                , transBasic_amount = Just $ Irct.Decimal2 $ Decimal transBasic_amountIncomeTax
                , transBasic_noMoney = Nothing
                , transBasic_oneOff = Nothing
                , transBasic_unjustEnrichment = Nothing
                , transBasic_recovery = Nothing
                } 
        in addWageReportAttributes $ schemaTypeToXML "WageReportRequestToIR" wageReportsToIR

          {-   let nameSpace = Namespace {nsPrefix = "wrtir", nsURI ="http://www.tulorekisteri.fi/2017/1/WageReportsToIR"}
            let attr1 = (N "xmlns:wrtir", AttValue [Left "http://www.tulorekisteri.fi/2017/1/WageReportsToIR"])
            let attr2 = (N "xmlns:xsi", AttValue [Left "http://www.w3.org/2001/XMLSchema-instance"])
            let attr3 = (N "xsi:schemaLocation", AttValue [Left "http://www.tulorekisteri.fi/2017/1/WageReportsToIR WageReportsToIR.xsd"])
            Elem (HaxTypes.QN nameSpace "WageReportRequestToIR")  [attr1, attr2,attr3] deliveryDataXml
 -}


            --schemaTypeToXML "DeliveryData"  deliveryData 
            --Wage deliveryData 
           -- let addedAttributes = wageReportAttributes (head xml)
           -- let (Document _ _ root _)  =  xmlParse "(No Document)" $ verbatim $ [addedAttributes]   -- :: String -> String -> Document Posn
           -- let (Right haskellData) = fst $ runParser elementWageReportRequestToIR [CElem root noPos]--[CElem root noPos]
            -- (Parser addedAttributes)
           -- return haskellData
            --return wageReportsToIR

 --           elementWageReportRequestToIR = parseSchemaType "WageReportRequestToIR"


--            let signature = mkElem  "Signature" []             
--            let wageReportRequestToIR = tag "WageReportRequestToIR" (head xml)
--            let xml' = tag "DeliveryDat" (head xml)
          --  let sign = position 0  (deep children) 
          --  let sign =  (deep $ tag "Signature") 

          --  let b = head $ sign (head xml) 
          --  let sign = (tag "Signature")
         --   let sign'' = (deep children)

            --            let sign' = position 2 $ sign
          --  let sign = path [children, tag "Signature"] (head xml)
--            let ok = mkElem "Testi" [] (head m)
--          
            --let addedAttributes = wageReportAttributes (head xml)

            --addedAttributes
           -- (head sign)
        --  
--            let m = wageReportAttributes (head xml)
            --  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"

            --Prolog (Maybe XMLDecl) [Misc] (Maybe DocTypeDecl) [Misc]	 
            --XMLDecl VersionInfo (Maybe EncodingDecl) (Maybe SDDecl)	 
--                let r = Prolog Nothing [] Nothing []
         --   Wage deliveryData --xml
         --    let prologue = Prolog (Just $ XMLDecl "1.0" (Just $ EncodingDecl "utf-8") (Just False))  [] Nothing []
        --    let doc =  Document prologue (emptyST) content []	 
--            CElem content ()


{- instance Reportable TemporaryEmployerData where
    service x = wageReportService
    action x = sendWageReportAction
    buildReport input = let 
        
        TemporaryEmployerData {..} = input

        wageReportsToIR = WageReportsToIR
                { wageReportsToIR_deliveryData = deliveryData
                , wageReportsToIR_signature = Nothing
                }
        deliveryData = DeliveryData
                { deliveryData_timestamp = DateTime deliveryData_timestamp
                , deliveryData_source = Irct.String30 $ Normalized "EasyBooks"
                , deliveryData_type = 100
                , deliveryData_deliveryId = Irct.String40 $ Normalized $ deliveryData_deliveryId
                , deliveryData_faultyControl = 2
                , deliveryData_productionEnvironment = Irct.TrueOrFalse deliveryData_productionEnvironment
                , deliveryData_owner = id1
                , deliveryData_creator = id2
                , deliveryData_sender = id3
                , deliveryData_paymentPeriod = paymentPeriod
                , deliveryData_contactPersons = contactPersons
                , deliveryData_payer = payer
                , deliveryData_reports = reports
                }

        reports = Reports
                { reports_report = [report]
                }
        report = Report
                { report_data =  reportData
                , report_incomeEarner = incomeEarner
                , report_transactions   =Just transactions
                , report_foreignLeasedWork  = Nothing
                , report_stayPeriodsInFinland  = Nothing
                , report_workPeriodsInFinland  = Nothing
                , report_workCountries  = Nothing
                , report_absence  = Nothing
                }
        incomeEarnerBasic = IncomeEarnerBasic
                { incomeEarnerBasic_missingId = Nothing
                , incomeEarnerBasic_companyName =Just $ Irct.String200 $ Normalized incomeEarnerBasic_companyName
                , incomeEarnerBasic_lastName = Just $ Irct.String200 $ Normalized incomeEarnerBasic_lastName
                , incomeEarnerBasic_firstName = Just $ Irct.String100 $ Normalized incomeEarnerBasic_firstName 
                , incomeEarnerBasic_birthDate = Nothing
                , incomeEarnerBasic_gender = Nothing
                }

        reportData = ReportData
                { reportData_actionCode = 1
                , reportData_iRReportId  = Nothing
                , reportData_reportId = Just $ Irct.String40 $ Normalized $ reportData_reportId
                , reportData_reportVersion  = Nothing
                }
        
        professions = Professions
                { professions_profession = [profession]
                }

        profession = Profession
                { profession_type = 1
                , profession_code = Irct.String20 $ Normalized profession_code
                , profession_title = Nothing
                }

        transactions = Transactions
                { transactions_transaction = [transaction'1,transaction'2,transaction'3,transaction'4]
                }

                
        incomeEarner = IncomeEarner
                { incomeEarner_ids = Just incomeEarnerIds
                , incomeEarner_basic  = Just incomeEarnerBasic
                , incomeEarner_addresses  = Nothing
                , incomeEarner_subOrgs  = Nothing
                , incomeEarner_employment  = Nothing
                , incomeEarner_professions  = Just professions
                , incomeEarner_employmentRegs  = Nothing
                , incomeEarner_placeOfBusiness  = Nothing
                , incomeEarner_pensionInsurance  = Just pensionInsurance
                , incomeEarner_accidentInsurance  = Nothing
                , incomeEarner_insuranceExceptions  = Nothing
                , incomeEarner_internationalData  = Nothing
                , incomeEarner_other  = Nothing
                }

        {- Jos eläkejärjestelynumero on "Työeläkelaitoksen yhtiötunnus" -tiedon
        mukainen tilapäisen työnantajan geneerinen eläkejärjestelynumero, yhden
        "Maksajan tyyppi" -tiedon on oltava "Tilapäinen työnantaja" ja
        eläkejärjestelynumeron on oltava voimassa "Suorituksen maksupäivä tai muu
        ilmoituspäivä" -tiedon mukaisena päivänä.
        -}
        pensionInsurance :: PensionInsurance
        pensionInsurance = PensionInsurance

                { pensionInsur_pensionActCode = 1
                , pensionInsur_pensionProvIdCode = Just pensionInsur_pensionProvIdCode
                , pensionInsur_pensionPolicyNo = Just $ Irct.PensionPolicyNo $ Normalized pensionInsur_pensionPolicyNo
                }

        incomeEarnerIds = IncomeEarnerIds
                { incomeEarnerIds_id = [earnerId]
                }
        earnerId = Id   
                { id_type = 2
                , id_code = Irct.String30 $ Normalized id_code_earner
                , id_countryCode  = Nothing
                , id_countryName  = Nothing
                }
        
        payer = Payer
                { payer_ids = Just ids
                , payer_basic = Just payerBasic
                , payer_address = Just address
                , payer_subOrgs  = Nothing
                , payer_other = Just payerOther
                , payer_substitutePayer  = Nothing
                }
        payerOther = PayerOther
                { payerOther_payerTypes = payerTypes
                }
        payerTypes = PayerTypes
                { payerTypes_code = [3]
                }
        ids = PayerIds
                { payerIds_id = [payerId]
                }
        payerId = Id 
                { id_type = 1
                , id_code = Irct.String30 $ Normalized id_code_payer
                , id_countryCode  = {- Just $ Irct.String2 $ XsdString "FI" -} Nothing 
                , id_countryName = Nothing
                }
        payerBasic = PayerBasic
                { payerBasic_missingId   = Nothing
                , payerBasic_companyName = Just $ Irct.String200 $ Normalized payerBasic_companyName
                , payerBasic_lastName  = Nothing 
                , payerBasic_firstName  = Nothing
                , payerBasic_birthDate  = Nothing
                , payerBasic_language = Just 1 
                }
        
        address = Address
                { address_co = Nothing 
                , address_street = Just $ Irct.String100 $ Normalized address_street
                , address_pOBox = Nothing
                , address_postalCode = Irct.String20 $ Normalized address_postalCode
                , address_postOffice = Irct.String200 $ Normalized address_postOffice
                , address_countryCode = Just $ Irct.String2 $ XsdString address_countryCode
                , address_countryName = Nothing
                }
        paymentPeriod = PaymentPeriod
                { paymentPeriod_paymentDate = Date paymentPeriod_paymentDate
                , paymentPeriod_startDate = Date paymentPeriod_startDate
                , paymentPeriod_endDate = Date paymentPeriod_endDate
                }            
        contactPersons =  ContactPersons
                { contactPersons_contactPerson =  [person]
                }
        person = ContactPerson
                { contactPerson_name = Irct.String200 $ Normalized contactPerson_name
                , contactPerson_telephone  = Irct.String40 $ Normalized contactPerson_telephone
                , contactPerson_email = Nothing
                , contactPerson_responsibilityCode = Nothing
                }
        id1 = Id
                { id_type = 1
                , id_code = Irct.String30 $ Normalized id_code_owner
                , id_countryCode = Nothing
                , id_countryName = Nothing
                }
        id2 = Id
                { id_type = 1
                , id_code = Irct.String30 $ Normalized id_code_creator
                , id_countryCode  = Nothing
                , id_countryName  = Nothing
                }
        id3 = Id
                { id_type = 1
                , id_code = Irct.String30 $ Normalized id_code_sender
                , id_countryCode  = Nothing
                , id_countryName  = Nothing
        
                }
        
                

        transaction'1 = Transaction
                { transaction_basic =  transactionBasic1
                , transaction_insuranceData = Nothing
                , transaction_earningPeriods =Nothing
                , transaction_unitWages =     Nothing
                , transaction_carBenefit =    Nothing
                , transaction_mealBenefit =   Nothing
                , transaction_otherBenefit =  Nothing
                , transaction_sailorIncome =  Nothing
                , transaction_recoveryData =  Nothing
                , transaction_dailyAllowance =Nothing
                , transaction_kmAllowance =   Nothing
                , transaction_sixMonthRule =  Nothing
                }

        transaction'2 = Transaction
                { transaction_basic =  transactionBasic2
                , transaction_insuranceData = Nothing
                , transaction_earningPeriods =Nothing
                , transaction_unitWages =     Nothing
                , transaction_carBenefit =    Nothing
                , transaction_mealBenefit =   Nothing
                , transaction_otherBenefit =  Nothing
                , transaction_sailorIncome =  Nothing
                , transaction_recoveryData =  Nothing
                , transaction_dailyAllowance =Nothing
                , transaction_kmAllowance =   Nothing
                , transaction_sixMonthRule =  Nothing
                }
        transaction'3 = Transaction
                { transaction_basic =  transactionBasic3
                , transaction_insuranceData = Nothing
                , transaction_earningPeriods =Nothing
                , transaction_unitWages =     Nothing
                , transaction_carBenefit =    Nothing
                , transaction_mealBenefit =   Nothing
                , transaction_otherBenefit =  Nothing
                , transaction_sailorIncome =  Nothing
                , transaction_recoveryData =  Nothing
                , transaction_dailyAllowance =Nothing
                , transaction_kmAllowance =   Nothing
                , transaction_sixMonthRule =  Nothing
                }
        transaction'4 = Transaction
                { transaction_basic =  transactionBasic4
                , transaction_insuranceData = Nothing
                , transaction_earningPeriods =Nothing
                , transaction_unitWages =     Nothing
                , transaction_carBenefit =    Nothing
                , transaction_mealBenefit =   Nothing
                , transaction_otherBenefit =  Nothing
                , transaction_sailorIncome =  Nothing
                , transaction_recoveryData =  Nothing
                , transaction_dailyAllowance =Nothing
                , transaction_kmAllowance =   Nothing
                , transaction_sixMonthRule =  Nothing
                }

        transactionBasic1 = TransactionBasic
                { transBasic_transactionCode = 101 --transBasic_transactionCodeTotalWage          -- palkka yhteissumma
                , transBasic_amount = Just $ Irct.Decimal2 $ Decimal transBasic_amountTotal 
                , transBasic_noMoney = Nothing
                , transBasic_oneOff = Nothing
                , transBasic_unjustEnrichment = Nothing
                , transBasic_recovery = Nothing
                }
        transactionBasic2 = TransactionBasic
                { transBasic_transactionCode = 402 -- transBasic_transactionCodeIncomeTax
                , transBasic_amount = Just $ Irct.Decimal2 $ Decimal transBasic_amountIncomeTax
                , transBasic_noMoney = Nothing
                , transBasic_oneOff = Nothing
                , transBasic_unjustEnrichment = Nothing
                , transBasic_recovery = Nothing
                }
        transactionBasic3 = TransactionBasic
                { transBasic_transactionCode = 413   -- pension
                , transBasic_amount = Just $ Irct.Decimal2 $ Decimal transBasic_amountPension
                , transBasic_noMoney = Nothing
                , transBasic_oneOff = Nothing
                , transBasic_unjustEnrichment = Nothing
                , transBasic_recovery = Nothing
                }
        transactionBasic4 = TransactionBasic
                { transBasic_transactionCode = 414  --unemploymentPayment     
                , transBasic_amount = Just $ Irct.Decimal2 $ Decimal transBasic_amountUnemployment
                , transBasic_noMoney = Nothing
                , transBasic_oneOff = Nothing
                , transBasic_unjustEnrichment = Nothing
                , transBasic_recovery = Nothing
                }
        in addWageReportAttributes $ elementToXMLWageReportRequestToIR wageReportsToIR
                --xml
                --Wage deliveryData
                --let m = wageReportAttributes (head xml)
                --m



 -}
--buildReport :: TemporaryEmployerData -> String
instance Reportable ContractEmployerData where
    service x = wageReportService 
    action x = sendWageReportAction
    buildReport input = let  
        
        ContractEmployerData {..} = input

        wageReportsToIR = WageReportsToIR
                { wageReportsToIR_deliveryData = deliveryData
                , wageReportsToIR_signature = Nothing
                }
        deliveryData = DeliveryData
                { deliveryData_timestamp = DateTime deliveryData_timestamp
                , deliveryData_source = Irct.String30 $ Normalized "EasyBooks"
                , deliveryData_type = 100
                , deliveryData_deliveryId = Irct.String40 $ Normalized $ deliveryData_deliveryId
                , deliveryData_faultyControl = 2
                , deliveryData_productionEnvironment = Irct.TrueOrFalse deliveryData_productionEnvironment
                , deliveryData_owner = id1
                , deliveryData_creator = id2
                , deliveryData_sender = id3
                , deliveryData_paymentPeriod = paymentPeriod
                , deliveryData_contactPersons = contactPersons
                , deliveryData_payer = payer
                , deliveryData_reports = reports
                }

        reports = Reports
                { reports_report = [report]
                }
        report = Report
                { report_data =  reportData
                , report_incomeEarner = incomeEarner
                , report_transactions   =Just transactions
                , report_foreignLeasedWork  = Nothing
                , report_stayPeriodsInFinland  = Nothing
                , report_workPeriodsInFinland  = Nothing
                , report_workCountries  = Nothing
                , report_absence  = Nothing
                }
        incomeEarnerBasic = IncomeEarnerBasic
                { incomeEarnerBasic_missingId = Nothing
                , incomeEarnerBasic_companyName = Nothing
--                , incomeEarnerBasic_companyName =Just $ Irct.String200 $ Normalized incomeEarnerBasic_companyName

                , incomeEarnerBasic_lastName = Nothing
                , incomeEarnerBasic_firstName = Just $ Irct.String100 $ Normalized incomeEarnerBasic_firstName 
                , incomeEarnerBasic_birthDate = Nothing
                , incomeEarnerBasic_gender = Nothing
                }

        reportData = ReportData
                { reportData_actionCode = 1
                , reportData_iRReportId  = Nothing
                , reportData_reportId = Just $ Irct.String40 $ Normalized $ reportData_reportId
                , reportData_reportVersion  = Nothing
                }
        
        professions = Professions
                { professions_profession = [profession]
                }

        profession = Profession
                { profession_type = 1
                , profession_code = Irct.String20 $ Normalized profession_code
                , profession_title = Nothing
                }

        transactions = Transactions
                { transactions_transaction = [transaction'1,transaction'2,transaction'3,transaction'4]
                }

                
        incomeEarner = IncomeEarner
                { incomeEarner_ids = Just incomeEarnerIds
                , incomeEarner_basic  = Just incomeEarnerBasic
                , incomeEarner_addresses  = Nothing
                , incomeEarner_subOrgs  = Nothing
                , incomeEarner_employment  = Nothing
                , incomeEarner_professions  = Just professions
                , incomeEarner_employmentRegs  = Nothing
                , incomeEarner_placeOfBusiness  = Nothing
                , incomeEarner_pensionInsurance  = Just pensionInsurance
                , incomeEarner_accidentInsurance  = Nothing
                , incomeEarner_insuranceExceptions  = Nothing
                , incomeEarner_internationalData  = Nothing
                , incomeEarner_other  = Nothing
                }

        {- Jos eläkejärjestelynumero on "Työeläkelaitoksen yhtiötunnus" -tiedon
        mukainen tilapäisen työnantajan geneerinen eläkejärjestelynumero, yhden
        "Maksajan tyyppi" -tiedon on oltava "Tilapäinen työnantaja" ja
        eläkejärjestelynumeron on oltava voimassa "Suorituksen maksupäivä tai muu
        ilmoituspäivä" -tiedon mukaisena päivänä.
        -}
        pensionInsurance :: PensionInsurance
        pensionInsurance = PensionInsurance

                { pensionInsur_pensionActCode = 1
                , pensionInsur_pensionProvIdCode = Just pensionInsur_pensionProvIdCode
                , pensionInsur_pensionPolicyNo = Just $ Irct.PensionPolicyNo $ Normalized pensionInsur_pensionPolicyNo
                }

        incomeEarnerIds = IncomeEarnerIds
                { incomeEarnerIds_id = [earnerId]
                }
        earnerId = Id   
                { id_type = 2
                , id_code = Irct.String30 $ Normalized id_code_earner
                , id_countryCode  = Nothing
                , id_countryName  = Nothing
                }
        
        payer = Payer
                { payer_ids = Just ids
                , payer_basic = Just payerBasic
                , payer_address = Just address
                , payer_subOrgs  = Nothing
                , payer_other = Nothing -- Just payerOther
                , payer_substitutePayer  = Nothing
                }
{-         payerOther = PayerOther
                { payerOther_payerTypes = payerTypes
                }
        payerTypes = PayerTypes
                { payerTypes_code = [3]
                }
 -}
        ids = PayerIds
                { payerIds_id = [payerId]
                }
        payerId = Id 
                { id_type = 1
                , id_code = Irct.String30 $ Normalized id_code_payer
                , id_countryCode  = {- Just $ Irct.String2 $ XsdString "FI" -} Nothing 
                , id_countryName = Nothing
                }
        payerBasic = PayerBasic
                { payerBasic_missingId   = Nothing
                , payerBasic_companyName = Just $ Irct.String200 $ Normalized payerBasic_companyName
                , payerBasic_lastName  = Nothing 
                , payerBasic_firstName  = Nothing
                , payerBasic_birthDate  = Nothing
                , payerBasic_language = Just 1 
                }
        
        address = Address
                { address_co = Nothing 
                , address_street = Just $ Irct.String100 $ Normalized address_street
                , address_pOBox = Nothing
                , address_postalCode = Irct.String20 $ Normalized address_postalCode
                , address_postOffice = Irct.String200 $ Normalized address_postOffice
                , address_countryCode = Just $ Irct.String2 $ XsdString address_countryCode
                , address_countryName = Nothing
                }
        paymentPeriod = PaymentPeriod
                { paymentPeriod_paymentDate = Date paymentPeriod_paymentDate
                , paymentPeriod_startDate = Date paymentPeriod_startDate
                , paymentPeriod_endDate = Date paymentPeriod_endDate
                }            
        contactPersons =  ContactPersons
                { contactPersons_contactPerson =  [person]
                }
        person = ContactPerson
                { contactPerson_name = Irct.String200 $ Normalized contactPerson_name
                , contactPerson_telephone  = Irct.String40 $ Normalized contactPerson_telephone
                , contactPerson_email = Nothing
                , contactPerson_responsibilityCode = Nothing
                }
        id1 = Id
                { id_type = 1
                , id_code = Irct.String30 $ Normalized id_code_owner
                , id_countryCode = Nothing
                , id_countryName = Nothing
                }
        id2 = Id
                { id_type = 1
                , id_code = Irct.String30 $ Normalized id_code_creator
                , id_countryCode  = Nothing
                , id_countryName  = Nothing
                }
        id3 = Id
                { id_type = 1
                , id_code = Irct.String30 $ Normalized id_code_sender
                , id_countryCode  = Nothing
                , id_countryName  = Nothing
        
                }
        
                

        transaction'1 = Data.WageReportsToIRTypes.Transaction
                { transaction_basic =  transactionBasic1
                , transaction_insuranceData = Nothing
                , transaction_earningPeriods =Nothing
                , transaction_unitWages =     Nothing
                , transaction_carBenefit =    Nothing
                , transaction_mealBenefit =   Nothing
                , transaction_otherBenefit =  Nothing
                , transaction_sailorIncome =  Nothing
                , transaction_recoveryData =  Nothing
                , transaction_dailyAllowance =Nothing
                , transaction_kmAllowance =   Nothing
                , transaction_sixMonthRule =  Nothing
                }

        transaction'2 = Data.WageReportsToIRTypes.Transaction
                { transaction_basic =  transactionBasic2
                , transaction_insuranceData = Nothing
                , transaction_earningPeriods =Nothing
                , transaction_unitWages =     Nothing
                , transaction_carBenefit =    Nothing
                , transaction_mealBenefit =   Nothing
                , transaction_otherBenefit =  Nothing
                , transaction_sailorIncome =  Nothing
                , transaction_recoveryData =  Nothing
                , transaction_dailyAllowance =Nothing
                , transaction_kmAllowance =   Nothing
                , transaction_sixMonthRule =  Nothing
                }
        transaction'3 = Data.WageReportsToIRTypes.Transaction
                { transaction_basic =  transactionBasic3
                , transaction_insuranceData = Nothing
                , transaction_earningPeriods =Nothing
                , transaction_unitWages =     Nothing
                , transaction_carBenefit =    Nothing
                , transaction_mealBenefit =   Nothing
                , transaction_otherBenefit =  Nothing
                , transaction_sailorIncome =  Nothing
                , transaction_recoveryData =  Nothing
                , transaction_dailyAllowance =Nothing
                , transaction_kmAllowance =   Nothing
                , transaction_sixMonthRule =  Nothing
                }
        transaction'4 = Data.WageReportsToIRTypes.Transaction
                { transaction_basic =  transactionBasic4
                , transaction_insuranceData = Nothing
                , transaction_earningPeriods =Nothing
                , transaction_unitWages =     Nothing
                , transaction_carBenefit =    Nothing
                , transaction_mealBenefit =   Nothing
                , transaction_otherBenefit =  Nothing
                , transaction_sailorIncome =  Nothing
                , transaction_recoveryData =  Nothing
                , transaction_dailyAllowance =Nothing
                , transaction_kmAllowance =   Nothing
                , transaction_sixMonthRule =  Nothing
                }

        transactionBasic1 = TransactionBasic
                { transBasic_transactionCode = 101 --transBasic_transactionCodeTotalWage          -- palkka yhteissumma
                , transBasic_amount = Just $ Irct.Decimal2 $ Decimal transBasic_amountTotal 
                , transBasic_noMoney = Nothing
                , transBasic_oneOff = Nothing
                , transBasic_unjustEnrichment = Nothing
                , transBasic_recovery = Nothing
                }
        transactionBasic2 = TransactionBasic
                { transBasic_transactionCode = 402 -- transBasic_transactionCodeIncomeTax
                , transBasic_amount = Just $ Irct.Decimal2 $ Decimal transBasic_amountIncomeTax
                , transBasic_noMoney = Nothing
                , transBasic_oneOff = Nothing
                , transBasic_unjustEnrichment = Nothing
                , transBasic_recovery = Nothing
                }
        transactionBasic3 = TransactionBasic
                { transBasic_transactionCode = 413   -- pension
                , transBasic_amount = Just $ Irct.Decimal2 $ Decimal transBasic_amountPension
                , transBasic_noMoney = Nothing
                , transBasic_oneOff = Nothing
                , transBasic_unjustEnrichment = Nothing
                , transBasic_recovery = Nothing
                }
        transactionBasic4 = TransactionBasic
                { transBasic_transactionCode = 414  --unemploymentPayment     
                , transBasic_amount = Just $ Irct.Decimal2 $ Decimal transBasic_amountUnemployment
                , transBasic_noMoney = Nothing
                , transBasic_oneOff = Nothing
                , transBasic_unjustEnrichment = Nothing
                , transBasic_recovery = Nothing
                }
        in      do

                let cnt = elementToXMLWageReportRequestToIR wageReportsToIR
 --   let !o = fst $ runParser elementStatusResponseFromIR [CElem (contentElem data') noPos]
 --               let r= Elem (N "Joku") [] cnt 
--                Elem QName [Attribute] [Content i]
              --  let o =  fst $ runParser elementWageReportRequestToIR [CElem r]

                addWageReportAttributes $ elementToXMLWageReportRequestToIR wageReportsToIR
                --xml
                --Wage deliveryData
                --let m = wageReportAttributes (head xml)
                --m


instance Reportable PayerSummaryData where
    service x = payerSummaryService
    action x = sendPayerSummaryReportAction
    buildReport input =  
        let 
            PayerSummaryData {..} = input
            payerSummaryReportsToIR = Summary.PayerSummaryReportsToIR
                { payerSummaryReportsToIR_deliveryData = deliveryData
                , payerSummaryReportsToIR_signature = Nothing
                }

          
            deliveryData = Summary.DeliveryData 
                { deliveryData_timestamp = DateTime deliveryData_timestamp
                , deliveryData_source = Irct.String30 $ Normalized "EasyBooks"
                , deliveryData_type = 101
                , deliveryData_deliveryId = Irct.String40 $ Normalized $ deliveryData_deliveryId
                , deliveryData_faultyControl = 2
                , deliveryData_productionEnvironment = Irct.TrueOrFalse deliveryData_productionEnvironment
                , deliveryData_owner = id1
                , deliveryData_creator = id2
                , deliveryData_sender = id3
                , deliveryData_contactPersons = contactPersons
                , deliveryData_payer = payer
                , deliveryData_reports = reports
                , deliveryData_reportDate = Date deliveryData_reportDate 
                }


-- Just $ Irct.Decimal2 $ Decimal 35.50
            --e = SimpleType Int 1
            paymentMonth = Summary.PaymentMonth
                    {  paymentMonth_month = toEnum (paymentMonth_month-1)
                    ,  paymentMonth_year = paymentMonth_year
                    }
            
            report = Summary.Report
                    { report_data = reportData
                    , report_paymentMonth = paymentMonth
                    , report_transactions =  transactions
                    }
            
            reportData = Summary.ReportData
                    { reportData_actionCode = 1
                    , reportData_iRReportId = Nothing 
                    , reportData_reportId =Just $ Irct.String40 $ Normalized $ reportData_reportId

                    , reportData_reportVersion = Nothing
                    }
            
            reports = Summary.Reports
                    { reports_report= [report]
                    }
            

            transactions = Summary.Transactions
                    { transactions_transaction = [transaction'1]
                    }

                    
            payer = Summary.Payer
                    { payer_ids = Just ids
                    , payer_basic = Just payerBasic
                    , payer_address = Just address
                    , payer_subOrgs  = Nothing
                    , payer_other = Nothing
                    , payer_pensionInsurances = Nothing
                    , payer_accidentInsurances= Nothing
                    }
            ids = Summary.PayerIds
                    { payerIds_id = [payerId]
                    }
            payerId = Summary.Id 
                    { id_type = 1
                    , id_code = Irct.String30 $ Normalized id_code_payer
                    , id_countryCode  = Nothing
                    , id_countryName  = Nothing
                    }
            payerBasic = Summary.PayerBasic
                    { payerBasic_missingId   = Nothing
                    , payerBasic_companyName = Just $ Irct.String200 $ Normalized payerBasic_companyName
                    , payerBasic_lastName  = Nothing 
                    , payerBasic_firstName  = Nothing
                    , payerBasic_birthDate  = Nothing
                    , payerBasic_language = Just 1 
                    }
            address = Summary.Address
                    { address_co = Nothing 
                    , address_street = Just $ Irct.String100 $ Normalized address_street
                    , address_pOBox = Nothing
                    , address_postalCode = Irct.String20 $ Normalized address_postalCode
                    , address_postOffice = Irct.String200 $ Normalized address_postOffice
                    , address_countryCode = Just $ Irct.String2 $ XsdString address_countryCode
                    , address_countryName = Nothing
                    }
            contactPersons =  Summary.ContactPersons
                    { contactPersons_contactPerson =  [person]
                    }
            person = Summary.ContactPerson
                    { contactPerson_name = Irct.String200 $ Normalized contactPerson_name
                    , contactPerson_telephone = Irct.String40 $ Normalized contactPerson_telephone
                    , contactPerson_email = Nothing
                    , contactPerson_responsibilityCode = Nothing
                    }

            id1 = Summary.Id
                { id_type = 1
                , id_code = Irct.String30 $ Normalized id_code_owner
                , id_countryCode = Nothing
                , id_countryName = Nothing
                }
            id2 = Summary.Id
                { id_type = 1
                , id_code = Irct.String30 $ Normalized id_code_creator
                , id_countryCode  = Nothing
                , id_countryName  = Nothing
                }
            id3 = Summary.Id
                { id_type = 1
                , id_code = Irct.String30 $ Normalized id_code_sender
                , id_countryCode  = Nothing
                , id_countryName  = Nothing
        
                }
        
            
            transaction'1 = Summary.Transaction
                    { transaction_basic =  transactionBasic1
                    }


            transactionBasic1 = Summary.TransactionBasic
                    { Summary.transBasic_summaryTransactionCode = 102         -- 102 Employee's health insurance contribution
                    , transBasic_amount = Just $ Irct.Decimal2 $ Decimal transBasic_amountHealthinsurance
                    }
            in addPayerSummaryAttributes  $ elementToXMLPayerSummaryReportRequestToIR payerSummaryReportsToIR
