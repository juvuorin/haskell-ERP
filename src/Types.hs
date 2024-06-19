{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-} 
module Types (module Types) where

import ClassyPrelude.Yesod (Entity, PathPiece, PersistEntity (PersistEntityBackend), PersistQueryRead, PersistRecordBackend, SymbolToField, ToBackendKey)
import Data.Aeson
import Data.Data
import Data.Int (Int64)
import Data.PayerSummaryReportsToIRTypes (DeliveryData)
import Data.String (IsString (..))
import Data.Text hiding (concatMap, filter, map)
import qualified Data.Text as T hiding (pack)
import Data.Time
import Data.Tree
import Data.WageReportsToIRTypes (DeliveryData)
import Database.Persist.Postgresql (PersistQueryWrite, SqlBackend, fromSqlKey, toSqlKey)
import Database.Persist.TH
import GHC.Generics (Generic)
import qualified GHC.OverloadedLabels
import Text.Read (read, readMaybe)
import Yesod.Core (PathPiece (..))
import qualified Data.Text as T
import qualified Data.Char as C

--import Import (Company)

--import Import (Account)
--import qualified Types
--import Model
type AccountCode = Int

newtype AccessCode = AccessCode
  { unAccessCode :: String
  }
  deriving (Show, Generic, IsString)
instance FromJSON AccessCode

newtype ConsentId = ConsentId
  { unConsentId :: String
  }
  deriving (Show, Generic, IsString)
instance FromJSON ConsentId

newtype ClientId = ClientId
  { unClientId :: String
  }
  deriving (Show, Generic, IsString)
instance FromJSON ClientId

newtype XApiKey = XApiKey
  { unXApiKey :: String
  }
  deriving (Show, Generic, IsString)
instance FromJSON XApiKey

newtype Token = Token
  { unToken :: String
  }
  deriving (Show, Generic, IsString)
instance FromJSON Token

newtype Scope = Scope
  { unScope :: String
  }

data PayeventExtraInfoType = DailySicknessAllowance deriving (Read, Show, Generic)
instance FromJSON PayeventExtraInfoType
instance ToJSON PayeventExtraInfoType
derivePersistField "PayeventExtraInfoType"

data PurchaseInvoiceType = DebitInvoice | CreditInvoice | TaxInvoice deriving (Read, Generic, Show, Eq)
instance FromJSON PurchaseInvoiceType
instance ToJSON PurchaseInvoiceType
derivePersistField "PurchaseInvoiceType"

defaultId = 0 :: Int64

newtype TokenEndpoint = TokenEndpoint {unTokenEndpoint :: String} deriving (Show, Generic, IsString)
instance FromJSON TokenEndpoint

newtype AuthEndpoint = AuthEndpoint {unAuthEndpoint :: String} deriving (Show, Generic, IsString)
instance FromJSON AuthEndpoint

newtype RedirectUriAuth = RedirectUriAuth {unRedirectUriAuth :: String} deriving (Show, Generic, IsString)
instance FromJSON RedirectUriAuth

newtype AccountAccessConsentsEndpoint = AccountAccessConsentsEndpoint {unAccountAccessConsentsEndpoint :: String} deriving (Show, Generic, IsString)
instance FromJSON AccountAccessConsentsEndpoint

newtype ApiEndpoint = ApiEndpoint {unApiEndpoint :: String} deriving (Show, Generic, IsString)
instance FromJSON ApiEndpoint

red :: Text -> Text
red x = "\x1b[1;31m" <> x <> "\ESC[0m"

keyToString x = do
  let key = fromSqlKey x
  show (key :: Int64)

currentDay = do
  now <- getZonedTime
  return $ localDay $ zonedTimeToLocalTime now

currentTime = do
  getCurrentTime

round' :: Double -> Double
round' x = fromIntegral (round $ x * 1e2) / 1e2

{- class CoherentObject a b where
  isCoherent :: a -> b Value
 -}
newtype PrivateKey = PrivateKey {unPrivateKey :: Text} deriving (Show)

data TransactionType
  = TypeSalesInvoice
  | TypePurchaseInvoice
  | TypeTravelExpense
  | TypeDeferral
  | TypeBankStatement
  | TypeGeneralExpense
  | TypeVatStatement
  | TypeSalary
  | TypeMemo
  | TypeProfitAndLoss
  | TypeCutOff
  | TypeOpeningOfBooks
  | TypeGeneralExpenseMonthly
  | TypeInterestInvoiceNonTax
  | TypePrepaymentsAndAccruedIncome
  | Closing
  deriving (Show, Read, Eq, Generic)
instance FromJSON TransactionType
instance ToJSON TransactionType
derivePersistField "TransactionType"

data DefaultAccountType = DefaultAccountTypeInterestNonTax | DefaultAccountTypeInterest
  deriving (Ord, Show, Read, Eq, Generic)
instance FromJSON DefaultAccountType
instance ToJSON DefaultAccountType
derivePersistField "DefaultAccountType"

data IRReportType = ReportTypeTyel | ReportTypeYel | ReportTypePaymentSummary
  deriving (Show, Read, Eq, Generic)
instance FromJSON IRReportType
instance ToJSON IRReportType

data AssetType = Equipment
  deriving (Show, Read, Eq, Generic)
instance FromJSON AssetType
instance ToJSON AssetType
derivePersistField "AssetType"

data DepreciationType = Standard
  deriving (Show, Read, Eq, Generic)
instance FromJSON DepreciationType
instance ToJSON DepreciationType
derivePersistField "DepreciationType"

data AccountingYearState = Open | Closed
  deriving (Show, Read, Eq, Generic)
instance FromJSON AccountingYearState
instance ToJSON AccountingYearState
derivePersistField "AccountingYearState"

{- data VerifiedStatus = Verified | NotVerified
  deriving (Show, Read, Eq, Generic)
instance FromJSON VerifiedStatus
instance ToJSON VerifiedStatus
derivePersistField "VerifiedStatus"
 -}
data ApprovedStatus = Approved | NotApproved
  deriving (Show, Read, Eq, Generic)
instance FromJSON ApprovedStatus
instance ToJSON ApprovedStatus
derivePersistField "ApprovedStatus"

{- data DocumentStatus -- PurchaseInvoice statuses
  = PurchaseInvoiceStatusInvoiceCreated
  | PurchaseInvoiceStatusInvoiceVerified
  | PurchaseInvoiceStatusInvoiceRejected
  | PurchaseInvoiceStatusInvoiceOpen
  | PurchaseInvoiceStatusInvoicePaid
  deriving (Show, Read, Eq, Generic)
instance FromJSON DocumentStatus
instance ToJSON DocumentStatus
derivePersistField "DocumentStatus"
 -}

data PurchaseInvoiceStatus -- PurchaseInvoice statuses
  = PurchaseInvoiceStatusInvoiceCreated
  | PurchaseInvoiceStatusInvoiceVerified
  | PurchaseInvoiceStatusInvoiceRejected
  | PurchaseInvoiceStatusInvoiceOpen
  | PurchaseInvoiceStatusInvoicePaid
  deriving (Show, Read, Eq, Generic)
instance FromJSON PurchaseInvoiceStatus
instance ToJSON PurchaseInvoiceStatus
derivePersistField "PurchaseInvoiceStatus"


data DocumentStatus -- PurchaseInvoice statuses
  = PurchaseInvoiceStatus PurchaseInvoiceStatus 
  deriving (Show, Read, Eq, Generic)
instance FromJSON DocumentStatus
instance ToJSON DocumentStatus
derivePersistField "DocumentStatus"


data PurchaseInvoiceProcessingTaskResult
  = PurchaseInvoiceProcessingTaskResultInvoiceVerified
  | PurchaseInvoiceProcessingTaskResultInvoiceApproved
  | PurchaseInvoiceProcessingTaskResultInvoiceRejected
  deriving (Show, Read, Eq, Generic)
instance FromJSON PurchaseInvoiceProcessingTaskResult
instance ToJSON PurchaseInvoiceProcessingTaskResult
derivePersistField "PurchaseInvoiceProcessingTaskResult"

data TaskResult = PurchaseInvoiceProcessingTaskResult PurchaseInvoiceProcessingTaskResult
  deriving (Show, Read, Eq, Generic)
instance FromJSON TaskResult
instance ToJSON TaskResult
derivePersistField "TaskResult"

instance (PathPiece PurchaseInvoiceProcessingTaskResult) where
  fromPathPiece x = readMaybe $ unpack x

{- data Showable = forall a . Show a => MkShowable a

--
-- And a nice existential builder
--
pack :: Show a => a -> Showable
pack = MkShowable

--
-- A heteoregenous list of Showable values
--
hlist :: [Showable]
hlist = [ Types.pack 3
        , Types.pack 'x'
        , Types.pack pi
        , Types.pack "string"
        , Types.pack (Just ()) ]
 -}
instance PathPiece TaskResult where
  fromPathPiece x = readMaybe $ unpack x



{- data PurchaseInvoiceTaskResult
  = PurchaseInvoiceProcessingTaskResultInvoiceVerified'
  | PurchaseInvoiceProcessingTaskResultInvoiceApproved'
  | PurchaseInvoiceProcessingTaskResultInvoiceRejected'
  deriving (Show, Read, Eq, Generic)
instance FromJSON PurchaseInvoiceTaskResult
instance ToJSON PurchaseInvoiceTaskResult
derivePersistField "PurchaseInvoiceTaskResult"
 -}
{- data AnyTaskResult = PurchaseInvoiceTaskResult PurchaseInvoiceTaskResult | TaskResult TaskResult
  deriving (Show, Read, Eq, Generic)
instance FromJSON AnyTaskResult
instance ToJSON AnyTaskResult
derivePersistField "AnyTaskResult"
 -}

data PurchaseInvoicePaymentStatus = PurchaseInvoicePaymentStatusInvoiceOpen | PurchaseInvoicePaymentStatusInvoiceDue
  deriving (Show, Read, Eq, Generic)
instance FromJSON PurchaseInvoicePaymentStatus
instance ToJSON PurchaseInvoicePaymentStatus
derivePersistField "PurchaseInvoicePaymentStatus"

data TaskType = PurchaseInvoiceProcessingTaskVerify | PurchaseInvoiceProcessingTaskApproveOrReject | PurchaseInvoiceProcessingTaskCancel|PurchaseInvoiceProcessingTaskApprove | PurchaseInvoiceProcessingTaskReject
  deriving (Data, Show, Read, Eq, Generic)
instance FromJSON TaskType
instance ToJSON TaskType
derivePersistField "TaskType"

data AccessRightType = VerifyPurchaseInvoice | ApprovePurchaseInvoice | RejectPurchaseInvoice deriving (Enum, Data, Show, Read, Eq, Generic)
instance FromJSON AccessRightType
instance ToJSON AccessRightType
derivePersistField "AccessRightType"

data Rolename = Admin | Accountant
  deriving (Data, Show, Read, Eq, Generic, Ord)
instance FromJSON Rolename
instance ToJSON Rolename
instance PathPiece Rolename where
  toPathPiece Admin = "Admin"
  toPathPiece Accountant = "Accountant"

  fromPathPiece :: Text -> Maybe Rolename
  fromPathPiece s =
    case (reads $ T.unpack s) of
      (i, "") : _
        | i == Admin -> Just Admin
        | i == Accountant -> Just Accountant
      [] -> Nothing

--instance PathPiece Rolename = show
derivePersistField "Rolename"

data SalesInvoiceStatus = PaidSales | OpenSales deriving (Show, Read, Eq, Generic)
instance FromJSON SalesInvoiceStatus
instance ToJSON SalesInvoiceStatus
derivePersistField "SalesInvoiceStatus"

data PurchaseInvoiceDueStatus = EarlyPurchase | DuePurchase | OverduePurchase deriving (Show, Read, Eq, Generic)
instance FromJSON PurchaseInvoiceDueStatus
instance ToJSON PurchaseInvoiceDueStatus

data SalesInvoiceDueStatus = EarlySales | DueSales | OverdueSales deriving (Show, Read, Eq, Generic)
instance FromJSON SalesInvoiceDueStatus
instance ToJSON SalesInvoiceDueStatus

--type VatPercentage = Maybe Double

--data Eit a b= Left a | Right b

--data MyEither = Eit Int String

data MyEither = Either String String

--data VatPercentage = VatPercentage Double
{- data VatPercentage = VatPercentage Int
  deriving  (Show, Read, Eq, Generic)
instance FromJSON VatPercentage
instance ToJSON VatPercentage
derivePersistField "VatPercentage"
 -}

{-
data TaxCode =
      301     {- Suoritettava 24%:n vero kotimaan myynnistä                 -}
    | 302     {- Suoritettava 14%:n vero kotimaan myynnistä                 -}
    | 303     {- Suoritettava 10%:n vero kotimaan myynnistä                 -}
    | 304     {- Vero tavaroiden maahantuonnista EU:n ulkopuolelta          -}
    | 305     {- Vero tavaraostoista muista EU-maista                       -}
    | 306     {- Vero palveluostoista muista EU-maista                      -}
    | 307     {- Verokauden vähennettävä vero                               -}
    | 308     {- Maksettava vero / Palautukseen oikeuttava vero (-)         -}
    | 309     {- 0-verokannan alainen liikevaihto                           -}
    | 310     {- Tavaroiden maahantuonnit EU:n ulkopuolelta                 -}
    | 311     {- Tavaroiden myynnit muihin EU-maihin                        -}
    | 312     {- Palveluiden myynnit muihin EU-maihin                       -}
    | 313     {- Tavaraostot muista EU-maista                               -}
    | 314     {- Palveluostot muista EU-maista                              -}
    | 315     {- Alarajahuojennukseen oikeuttava liikevaihto                -}
    | 316     {- Alarajahuojennukseen oikeuttava vero                       -}
    | 317     {- Alarajahuojennuksen määrä                                  -}
    | 336     {- Alarajahuojennusta voi hakea vain tilikauden viimei- -}
            {-             senä verokautena tai kalenterivuoden viimeisenä  -}
            {-             neljänneksenä tai arvonlisäverovelvollisuuden päät- -}
            {-                         tymiskaudella  -}
            {-             1=haen, koska kyseessä on tilikauden viimeinen ve- -}
            {-             rokausi  -}
            {-             2=haen, koska kyseessä on kalenterivuoden viimei- -}
            {-             nen neljännes  -}
            {-             3=haen, koska alv-velvollisuuteni on päättynyt tällä  -}
            {-             verokaudella  -}
            {-                                                            N1      1,2,3           -}
    | 337     {- Maksuperusteinen arvonlisävero                      -}
    | 318     {- Vero rakentamispalvelun ja metalliromun ostoista    -}
    | 319     {- Rakentamispalvelun ja metalliromun myynnit   -}
    | 320     {- Rakentamispalvelun ja metalliromun ostot   -}
     -}

{- listVatReportType =
   [ Domestic
    ,ReverseCharge24
    ,ReverseCharge14
    ,ReverseCharge10
    ,Import24
    ,Import14
    ,Import10
    ,Import0
    ,EUGoods24
    ,EUGoods14
    ,EUGoods10
    ,EUGoods0
    ,EUServices24
    ,EUServices14
    ,EUServices10
    ,OutsideEU
    ,ConstructionReverseCharge24
    ,ZeroTaxBaseTurnover
    ,EUSaleGoods
    ,EUSaleServices]
 -}

data VatSaleType
  = SaleDomestic
  | SaleEUGoods
  | SaleEUServices
  | SaleZeroTaxBaseTurnover
  | SaleEUTriangularTrade
  | SaleOutsideEU
  | SaleOtherNonTaxed
  | SaleNoVatHandling
  | SaleConstructionReverseCharge24
  deriving (Show, Read, Eq, Generic)

-- 21
data VatPurchaseType
  = PurchaseDomestic
  | PurchaseEUGoods24
  | PurchaseEUGoods14
  | PurchaseEUGoods10
  | PurchaseEUGoods0
  | PurchaseEUServices24
  | PurchaseEUServices14
  | PurchaseEUServices10
  | PurchaseEUServices0
  | PurchaseReverseCharge24
  | PurchaseReverseCharge14
  | PurchaseReverseCharge10
  | PurchaseEUNotTaxedInFinland
  | PurchaseImport24
  | PurchaseImport14
  | PurchaseImport10
  | PurchaseImport0
  | PurchaseOutsideEU
  | PurchaseNoVatRegistration
  | PurchaseConstructionReverseCharge24
  | PurchaseNoVatHandling
  deriving (Show, Read, Eq, Generic)

instance FromJSON VatSaleType
instance ToJSON VatSaleType

instance FromJSON VatPurchaseType
instance ToJSON VatPurchaseType

data VatType = Sale VatSaleType | Purchase VatPurchaseType deriving (Show, Read, Eq, Generic)
instance FromJSON VatType
instance ToJSON VatType

derivePersistField "VatPurchaseType"
derivePersistField "VatSaleType"
derivePersistField "VatType"

type Code = Int

--type VatPct = Double
type Factor = Double
data VatPctType = Vat0 | Vat10 | Vat14 | Vat24 deriving (Show, Read, Eq, Generic)
instance FromJSON VatPctType
instance ToJSON VatPctType

derivePersistField "VatPctType"

{- data VatPctTypeWithValue = VatPctTypeWithValue {vatPctType:: VatPctType,value:: Double}

  deriving (Show, Read, Eq, Generic)

instance FromJSON VatPctTypeWithValue
instance ToJSON VatPctTypeWithValue
 -}

--VatPct json
--    vattype             VatPctType
--    vatpct              Double
--    description         String

--VatReport json

{- data VatReportRule=VatReportRule VatReportIdentifier TransactionType (Maybe VatPctType) VatRule Factor
data TransactionType = Sale | Purchase

data VatReportIdentifier = VatReportIdentifier Code String
v301 = VatReportIdentifier 301 "Suoritettava 24 %:n vero kotimaan myynnistä"
v302 = VatReportIdentifier 302 "Suoritettava 14 %:n vero kotimaan myynnistä"
v303 = VatReportIdentifier 303 "Suoritettava 10 %:n vero kotimaan myynnistä"
v304 = VatReportIdentifier 304 "Vero tavaroiden maahantuonnista EU:n ulkopuolelta"
v305 = VatReportIdentifier 305 "Vero tavaraostoista muista EU-maista"
v306 = VatReportIdentifier 306 "Vero palveluostoista muista EU-maista"
v307 = VatReportIdentifier 307 "Kohdekauden vähennettävä vero"
v308 = VatReportIdentifier 309 "0-verokannan alainen liikevaihto"
v310 = VatReportIdentifier 310 "Tavaroiden maahantuonnit EU:n ulkopuolelta"
v311 = VatReportIdentifier 311 "Tavaran myynti muihin EU-maihin"
v312 = VatReportIdentifier 312 "Palveluiden myynti muihin EU-maihin"
v313 = VatReportIdentifier 313 "Tavaraostot muista EU-maista"
v314 = VatReportIdentifier 314 "Palveluostot muista EU-maista"
v318 = VatReportIdentifier 318 "Vero rakentamispalvelun ostosta"
v319 = VatReportIdentifier 319 "Rakentamispalvelun myynnit"
v320 = VatReportIdentifier 320 "Rakentamispalvelun ostot"

data VatRule = VatPayable | VatDeductible | BookValue | BookValueVatDeductible
 -}
{-
rules = [_301 Sale Vat24 VatPayable (-1)]

Osto

Käännetty verovelvollisuus 24 %

Vientien kp-arvo

0,24
301 Suoritettava 24 %:n vero kotimaan myynnistä

Myynti

24

Kotimaa

Vientien ALV-summa

-1

 -}

--Vat json
--    vattype             VatType
--    description         String

{- purchaseInvoice :

Domestic  -- Kotimaa
EUServicesOrGoods -- 24/14/10/0 % (tavara/palvelu)
ReverseCharge --Käännetty verovelvollisuus 24/14/10 %
PurchaseEUNotTaxedInFinland --EU osto, ei yhteisöhankinta
Import --         -- Maahantuonti
OutsideEU         --EU:n ulkop.
NoVatRegistration   -- Ei ALV rekisteröintiä
ConstructionReverseCharge24  -- Rakennusalan käänteinen ALV 24 %
NoVatHandling-- Ei ALV-käsittelyä
 -}

{- data VatSale =
      Sale
    | EUServiceSale | EUGoodsSale | ConstructionServiceSale
    | NonEUSale | EuServiceSaleNoReverseCharged

data VatPurchase =
      Purchase
    | EUServicePurchase | EUGoodsPurchase | EUGoodsPurchaseNonDeductible | EUServicePurchaseNonDeductible
    | ConstructionServicePurchase |ConstructionServicePurchaseNonDeductible
    | ReverseChargedDeductible | ReverseChargedNonDeductible
    | NonEUPurchase | ImportDeductible | ImportNonDeductible

data VatClass = VatSale | VatPurchase | NoVat
 -}

{- Käännetty verovelvollisuus 24 %
Kotimaa
Käännetty verovelvollisuus 14 %
Käännetty verovelvollisuus 10 %
Maahantuonti 24%
Maahantuonti 14%
Maahantuonti 10%
EU 24 % (tavara)
EU 14 % (tavara)
EU 10 % (tavara)
EU 24 % (palvelu)
EU 14 % (palvelu)
EU 10 % (palvelu)
EU:n ulkopuolella
Rakennusalan käänteinen ALV 24 %
0-verokannan alainen liikevaihto
Maahantuonti 0%
EU, yhteisömyynti (tavara)
EU, yhteisömyynti (palvelu)
EU 0 % (tavara)

 -}

--data IRCategory =  CompanyShareHealthInsurancePayment +

{- data VatType = VAT10 | VAT14 | VAT24
 deriving (Show, Read, Eq, Generic)
instance FromJSON VatType
instance ToJSON VatType
derivePersistField "VatType"

data VatAccount = AccountTypeConstructor VatType VatType

                  deriving (Show, Read, Eq, Generic)
instance FromJSON VatAccount
instance ToJSON VatAccount
derivePersistField "MyTestType"

t = SaleAccount VAT10
t' = SaleAccount VAT14

 -}
--data Sale VatType x = Vat10 Int

-- VAT purchase and Sale account
data PropertyType
  = Vat10SaleAccount
  | Vat14ASaleAccount
  | Vat24SaleAccount
  | Vat10PurchaseAccount
  | Vat14PurchasAeccount
  | Vat24PurchaseAccount
  | -- VAT receivables and payables per vat type
    --                  | Vat10ReceivableAccount | Vat14ReceivableAeccount | Vat24ReceivableAccount
    --                  | Vat10PayableAccount | Vat14PayableAccount | Vat24PayableAccount

    --                  | EUServiceSale | EuServicePurchase

    -- Company health insurance payment to be send to income register
    HealthInsurancePayerSummary
  -- Standard 3 year depreciation

  deriving (Show, Read, Eq, Generic)
instance FromJSON PropertyType
instance ToJSON PropertyType
derivePersistField "PropertyType"

{- data Property =     MyTestType | Vat14ASaleAccount | Vat24SaleAccount
                  | Vat10PurchaseAccount | Vat14PurchasAeccount | Vat24PurchaseAccount

                  -- VAT receivables and payables per vat type
                  | Vat10ReceivableAccount | Vat14ReceivableAeccount | Vat24ReceivableAccount
                  | Vat10PayableAccount | Vat14PayableAccount | Vat24PayableAccount

--                  | EUServiceSale | EuServicePurchase

                -- Company health insurance payment to be send to income register
                 | HealthInsurancePayerSummary

                -- Standard 3 year depreciation

                  deriving (Show, Read, Eq, Generic)
instance FromJSON Property
instance ToJSON Property
derivePersistField "Property"

 -}
-- Customer types

data CustomerType = Juridical | Natural deriving (Show, Read, Eq, Generic)
instance FromJSON CustomerType
instance ToJSON CustomerType

derivePersistField "CustomerType"

--data AssetAccount = LiabilityAssetAccount | AssetAssetAccount | EquityAssetAccount
--data IncomeAccount= ExpenseIncomeAccount | RevenueIncomeAccount

data AccountType = AssetAccount | LiabilityAccount | ExpenseAccount | IncomeAccount | EquityAccount deriving (Show, Read, Eq, Generic)

-- Account types

--data AccountType = AccountTypeLiability | AccountTypeRevenue | AccountTypeExpense deriving (Show, Read, Eq, Generic)
instance FromJSON AccountType
instance ToJSON AccountType
derivePersistField "AccountType"

data AccountDefaultFor = AccountDefaultForPurchase | AccountDefaultForSale deriving (Show, Read, Ord, Eq, Generic)
instance FromJSON AccountDefaultFor
instance ToJSON AccountDefaultFor
derivePersistField "AccountDefaultFor"

data ConfigurationItem = AllSet | HealthInsurenacePayerSummaryProperty | ReportTrees
  deriving (Show, Read, Eq, Generic)
instance FromJSON ConfigurationItem
instance ToJSON ConfigurationItem
derivePersistField "ConfigurationItem"

{- data VatType = Vat10 | Vat14 | Vat24  deriving (Show, Read, Eq, Generic)
instance FromJSON VatType
instance ToJSON VatType
derivePersistField "VatType"
 -}
data ChartOfAccountsType = Osakeyhtiö | Yhdistys deriving (Show, Read, Eq, Generic)
instance FromJSON ChartOfAccountsType
instance ToJSON ChartOfAccountsType
derivePersistField "ChartOfAccountsType"

data EmployeeType = Tyel | Yel deriving (Show, Read, Eq, Generic)
instance FromJSON EmployeeType
instance ToJSON EmployeeType
derivePersistField "EmployeeType"

data EmployerType = TemporaryEmployer | ContractEmployer deriving (Show, Read, Eq, Generic)
instance FromJSON EmployerType
instance ToJSON EmployerType
derivePersistField "EmployerType"

-- Employee Deduction types

data PayadjustmentType = Tax | EmployeeUnemploymentInsurance | EmployeeHealthInsuranceDaily | EmployeePensionInsurance deriving (Show, Read, Eq, Generic, Ord)
instance FromJSON PayadjustmentType
instance ToJSON PayadjustmentType

-- instance PathPiece PayAdjustmentType

derivePersistField "PayadjustmentType"

data EmployerContributionType = EmployerHealthInsurance | EmployerPensionInsurance | EmployerUnemploymentInsurance deriving (Show, Read, Eq, Generic)
instance FromJSON EmployerContributionType
instance ToJSON EmployerContributionType

derivePersistField "EmployerContributionType"

{-
data ReportType = VatReport   deriving (Show, Read, Eq, Generic)
instance FromJSON ReportType
instance ToJSON ReportType

derivePersistField "ReportType"
 -}

data GenericGADT (s :: DocumentStatus) where
  MkDocument :: ( SymbolToField "document_status" record DocumentStatus, ToBackendKey SqlBackend record) 
    => {entityDocument :: Entity record} -> GenericGADT s

