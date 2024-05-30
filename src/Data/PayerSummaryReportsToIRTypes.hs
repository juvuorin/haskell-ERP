{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.PayerSummaryReportsToIRTypes
  ( module Data.PayerSummaryReportsToIRTypes
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.IRCommonTypes as Irct
import Data.Xmldsig as Ds
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
data AccidentInsurance = AccidentInsurance
        { accidInsur_accInsProvId :: Id
        , accidInsur_accInsPolicyNo :: Irct.String20
        }
        deriving (Eq,Show)
instance SchemaType AccidentInsurance where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return AccidentInsurance
            `apply` parseSchemaType "AccInsProvId"
            `apply` parseSchemaType "AccInsPolicyNo"
    schemaTypeToXML s x@AccidentInsurance{} =
        toXMLElement s []
            [ schemaTypeToXML "AccInsProvId" $ accidInsur_accInsProvId x
            , schemaTypeToXML "AccInsPolicyNo" $ accidInsur_accInsPolicyNo x
            ]
 
data AccidentInsurances = AccidentInsurances
        { accidInsur_accidentInsurance :: [AccidentInsurance]
        }
        deriving (Eq,Show)
instance SchemaType AccidentInsurances where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return AccidentInsurances
            `apply` many1 (parseSchemaType "AccidentInsurance")
    schemaTypeToXML s x@AccidentInsurances{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "AccidentInsurance") $ accidInsur_accidentInsurance x
            ]
 
data Address = Address
        { address_co :: Maybe Irct.String70
        , address_street :: Maybe Irct.String100
        , address_pOBox :: Maybe Irct.String10
        , address_postalCode :: Irct.String20
        , address_postOffice :: Irct.String200
        , address_countryCode :: Maybe Irct.String2
        , address_countryName :: Maybe Irct.String70
        }
        deriving (Eq,Show)
instance SchemaType Address where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Address
            `apply` optional (parseSchemaType "Co")
            `apply` optional (parseSchemaType "Street")
            `apply` optional (parseSchemaType "POBox")
            `apply` parseSchemaType "PostalCode"
            `apply` parseSchemaType "PostOffice"
            `apply` optional (parseSchemaType "CountryCode")
            `apply` optional (parseSchemaType "CountryName")
    schemaTypeToXML s x@Address{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "Co") $ address_co x
            , maybe [] (schemaTypeToXML "Street") $ address_street x
            , maybe [] (schemaTypeToXML "POBox") $ address_pOBox x
            , schemaTypeToXML "PostalCode" $ address_postalCode x
            , schemaTypeToXML "PostOffice" $ address_postOffice x
            , maybe [] (schemaTypeToXML "CountryCode") $ address_countryCode x
            , maybe [] (schemaTypeToXML "CountryName") $ address_countryName x
            ]
 
data ContactPerson = ContactPerson
        { contactPerson_name :: Irct.String200
        , contactPerson_telephone :: Irct.String40
        , contactPerson_email :: Maybe Irct.String70
        , contactPerson_responsibilityCode :: Maybe Xs.Int
        }
        deriving (Eq,Show)
instance SchemaType ContactPerson where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return ContactPerson
            `apply` parseSchemaType "Name"
            `apply` parseSchemaType "Telephone"
            `apply` optional (parseSchemaType "Email")
            `apply` optional (parseSchemaType "ResponsibilityCode")
    schemaTypeToXML s x@ContactPerson{} =
        toXMLElement s []
            [ schemaTypeToXML "Name" $ contactPerson_name x
            , schemaTypeToXML "Telephone" $ contactPerson_telephone x
            , maybe [] (schemaTypeToXML "Email") $ contactPerson_email x
            , maybe [] (schemaTypeToXML "ResponsibilityCode") $ contactPerson_responsibilityCode x
            ]
 
data ContactPersons = ContactPersons
        { contactPersons_contactPerson :: [ContactPerson]
        }
        deriving (Eq,Show)
instance SchemaType ContactPersons where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return ContactPersons
            `apply` many1 (parseSchemaType "ContactPerson")
    schemaTypeToXML s x@ContactPersons{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "ContactPerson") $ contactPersons_contactPerson x
            ]
 
data DeliveryData = DeliveryData
        { deliveryData_timestamp :: Xs.DateTime
        , deliveryData_source :: Irct.String30
        , deliveryData_type :: Xs.Int
        , deliveryData_deliveryId :: Irct.String40
        , deliveryData_faultyControl :: Xs.Int
        , deliveryData_productionEnvironment :: Irct.TrueOrFalse
        , deliveryData_owner :: Id
        , deliveryData_creator :: Id
        , deliveryData_sender :: Id
        , deliveryData_contactPersons :: ContactPersons
        , deliveryData_reportDate :: Xs.Date
        , deliveryData_payer :: Payer
        , deliveryData_reports :: Reports
        }
        deriving (Eq,Show)
instance SchemaType DeliveryData where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DeliveryData
            `apply` parseSchemaType "Timestamp"
            `apply` parseSchemaType "Source"
            `apply` parseSchemaType "DeliveryDataType"
            `apply` parseSchemaType "DeliveryId"
            `apply` parseSchemaType "FaultyControl"
            `apply` parseSchemaType "ProductionEnvironment"
            `apply` parseSchemaType "DeliveryDataOwner"
            `apply` parseSchemaType "DeliveryDataCreator"
            `apply` parseSchemaType "DeliveryDataSender"
            `apply` parseSchemaType "ContactPersons"
            `apply` parseSchemaType "ReportDate"
            `apply` parseSchemaType "Payer"
            `apply` parseSchemaType "Reports"
    schemaTypeToXML s x@DeliveryData{} =
        toXMLElement s []
            [ schemaTypeToXML "Timestamp" $ deliveryData_timestamp x
            , schemaTypeToXML "Source" $ deliveryData_source x
            , schemaTypeToXML "DeliveryDataType" $ deliveryData_type x
            , schemaTypeToXML "DeliveryId" $ deliveryData_deliveryId x
            , schemaTypeToXML "FaultyControl" $ deliveryData_faultyControl x
            , schemaTypeToXML "ProductionEnvironment" $ deliveryData_productionEnvironment x
            , schemaTypeToXML "DeliveryDataOwner" $ deliveryData_owner x
            , schemaTypeToXML "DeliveryDataCreator" $ deliveryData_creator x
            , schemaTypeToXML "DeliveryDataSender" $ deliveryData_sender x
            , schemaTypeToXML "ContactPersons" $ deliveryData_contactPersons x
            , schemaTypeToXML "ReportDate" $ deliveryData_reportDate x
            , schemaTypeToXML "Payer" $ deliveryData_payer x
            , schemaTypeToXML "Reports" $ deliveryData_reports x
            ]
 
data Id = Id
        { id_type :: Xs.Int
        , id_code :: Irct.String30
        , id_countryCode :: Maybe Irct.String2
        , id_countryName :: Maybe Irct.String70
        }
        deriving (Eq,Show)
instance SchemaType Id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Id
            `apply` parseSchemaType "Type"
            `apply` parseSchemaType "Code"
            `apply` optional (parseSchemaType "CountryCode")
            `apply` optional (parseSchemaType "CountryName")
    schemaTypeToXML s x@Id{} =
        toXMLElement s []
            [ schemaTypeToXML "Type" $ id_type x
            , schemaTypeToXML "Code" $ id_code x
            , maybe [] (schemaTypeToXML "CountryCode") $ id_countryCode x
            , maybe [] (schemaTypeToXML "CountryName") $ id_countryName x
            ]
 
data Payer = Payer
        { payer_ids :: Maybe PayerIds
        , payer_basic :: Maybe PayerBasic
        , payer_address :: Maybe Address
        , payer_subOrgs :: Maybe SubOrgs
        , payer_pensionInsurances :: Maybe PensionInsurances
        , payer_accidentInsurances :: Maybe AccidentInsurances
        , payer_other :: Maybe PayerOther
        }
        deriving (Eq,Show)
instance SchemaType Payer where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Payer
            `apply` optional (parseSchemaType "PayerIds")
            `apply` optional (parseSchemaType "PayerBasic")
            `apply` optional (parseSchemaType "Address")
            `apply` optional (parseSchemaType "SubOrgs")
            `apply` optional (parseSchemaType "PensionInsurances")
            `apply` optional (parseSchemaType "AccidentInsurances")
            `apply` optional (parseSchemaType "PayerOther")
    schemaTypeToXML s x@Payer{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "PayerIds") $ payer_ids x
            , maybe [] (schemaTypeToXML "PayerBasic") $ payer_basic x
            , maybe [] (schemaTypeToXML "Address") $ payer_address x
            , maybe [] (schemaTypeToXML "SubOrgs") $ payer_subOrgs x
            , maybe [] (schemaTypeToXML "PensionInsurances") $ payer_pensionInsurances x
            , maybe [] (schemaTypeToXML "AccidentInsurances") $ payer_accidentInsurances x
            , maybe [] (schemaTypeToXML "PayerOther") $ payer_other x
            ]
 
data PayerBasic = PayerBasic
        { payerBasic_missingId :: Maybe Irct.True
        , payerBasic_companyName :: Maybe Irct.String200
        , payerBasic_lastName :: Maybe Irct.String200
        , payerBasic_firstName :: Maybe Irct.String100
        , payerBasic_birthDate :: Maybe Xs.Date
        , payerBasic_language :: Maybe Xs.Int
        }
        deriving (Eq,Show)
instance SchemaType PayerBasic where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PayerBasic
            `apply` optional (parseSchemaType "MissingId")
            `apply` optional (parseSchemaType "CompanyName")
            `apply` optional (parseSchemaType "LastName")
            `apply` optional (parseSchemaType "FirstName")
            `apply` optional (parseSchemaType "BirthDate")
            `apply` optional (parseSchemaType "Language")
    schemaTypeToXML s x@PayerBasic{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "MissingId") $ payerBasic_missingId x
            , maybe [] (schemaTypeToXML "CompanyName") $ payerBasic_companyName x
            , maybe [] (schemaTypeToXML "LastName") $ payerBasic_lastName x
            , maybe [] (schemaTypeToXML "FirstName") $ payerBasic_firstName x
            , maybe [] (schemaTypeToXML "BirthDate") $ payerBasic_birthDate x
            , maybe [] (schemaTypeToXML "Language") $ payerBasic_language x
            ]
 
data PayerIds = PayerIds
        { payerIds_id :: [Id]
        }
        deriving (Eq,Show)
instance SchemaType PayerIds where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PayerIds
            `apply` many1 (parseSchemaType "Id")
    schemaTypeToXML s x@PayerIds{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Id") $ payerIds_id x
            ]
 
data PayerOther = PayerOther
        { payerOther_payerTypes :: PayerTypes
        }
        deriving (Eq,Show)
instance SchemaType PayerOther where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PayerOther
            `apply` parseSchemaType "PayerTypes"
    schemaTypeToXML s x@PayerOther{} =
        toXMLElement s []
            [ schemaTypeToXML "PayerTypes" $ payerOther_payerTypes x
            ]
 
data PayerSummaryReportsToIR = PayerSummaryReportsToIR
        { payerSummaryReportsToIR_deliveryData :: DeliveryData
        , payerSummaryReportsToIR_signature :: Maybe SignatureType
        }
        deriving (Eq,Show)
instance SchemaType PayerSummaryReportsToIR where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PayerSummaryReportsToIR
            `apply` parseSchemaType "DeliveryData"
            `apply` optional (elementSignature)
    schemaTypeToXML s x@PayerSummaryReportsToIR{} =
        toXMLElement s []
            [ schemaTypeToXML "DeliveryData" $ payerSummaryReportsToIR_deliveryData x
            , maybe [] (elementToXMLSignature) $ payerSummaryReportsToIR_signature x
            ]
 
data PayerTypes = PayerTypes
        { payerTypes_code :: [Xs.Int]
        }
        deriving (Eq,Show)
instance SchemaType PayerTypes where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PayerTypes
            `apply` many1 (parseSchemaType "Code")
    schemaTypeToXML s x@PayerTypes{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Code") $ payerTypes_code x
            ]
 
data PaymentMonth = PaymentMonth
        { paymentMonth_month :: Irct.MonthsType
        , paymentMonth_year :: Xs.Int
        }
        deriving (Eq,Show)
instance SchemaType PaymentMonth where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PaymentMonth
            `apply` parseSchemaType "Month"
            `apply` parseSchemaType "Year"
    schemaTypeToXML s x@PaymentMonth{} =
        toXMLElement s []
            [ schemaTypeToXML "Month" $ paymentMonth_month x
            , schemaTypeToXML "Year" $ paymentMonth_year x
            ]
 
data PensionInsurance = PensionInsurance
        { pensionInsur_pensionProvIdCode :: Xs.Int
        , pensionInsur_pensionPolicyNo :: Irct.PensionPolicyNo
        }
        deriving (Eq,Show)
instance SchemaType PensionInsurance where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PensionInsurance
            `apply` parseSchemaType "PensionProvIdCode"
            `apply` parseSchemaType "PensionPolicyNo"
    schemaTypeToXML s x@PensionInsurance{} =
        toXMLElement s []
            [ schemaTypeToXML "PensionProvIdCode" $ pensionInsur_pensionProvIdCode x
            , schemaTypeToXML "PensionPolicyNo" $ pensionInsur_pensionPolicyNo x
            ]
 
data PensionInsurances = PensionInsurances
        { pensionInsur_pensionInsurance :: [PensionInsurance]
        }
        deriving (Eq,Show)
instance SchemaType PensionInsurances where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PensionInsurances
            `apply` many1 (parseSchemaType "PensionInsurance")
    schemaTypeToXML s x@PensionInsurances{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "PensionInsurance") $ pensionInsur_pensionInsurance x
            ]
 
data Report = Report
        { report_data :: ReportData
        , report_paymentMonth :: PaymentMonth
        , report_transactions :: Transactions
        }
        deriving (Eq,Show)
instance SchemaType Report where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Report
            `apply` parseSchemaType "ReportData"
            `apply` parseSchemaType "PaymentMonth"
            `apply` parseSchemaType "Transactions"
    schemaTypeToXML s x@Report{} =
        toXMLElement s []
            [ schemaTypeToXML "ReportData" $ report_data x
            , schemaTypeToXML "PaymentMonth" $ report_paymentMonth x
            , schemaTypeToXML "Transactions" $ report_transactions x
            ]
 
data ReportData = ReportData
        { reportData_actionCode :: Xs.Int
        , reportData_iRReportId :: Maybe Irct.Guid
        , reportData_reportId :: Maybe Irct.String40
        , reportData_reportVersion :: Maybe Xs.Int
        }
        deriving (Eq,Show)
instance SchemaType ReportData where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return ReportData
            `apply` parseSchemaType "ActionCode"
            `apply` optional (parseSchemaType "IRReportId")
            `apply` optional (parseSchemaType "ReportId")
            `apply` optional (parseSchemaType "ReportVersion")
    schemaTypeToXML s x@ReportData{} =
        toXMLElement s []
            [ schemaTypeToXML "ActionCode" $ reportData_actionCode x
            , maybe [] (schemaTypeToXML "IRReportId") $ reportData_iRReportId x
            , maybe [] (schemaTypeToXML "ReportId") $ reportData_reportId x
            , maybe [] (schemaTypeToXML "ReportVersion") $ reportData_reportVersion x
            ]
 
data Reports = Reports
        { reports_report :: [Report]
        }
        deriving (Eq,Show)
instance SchemaType Reports where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Reports
            `apply` many1 (parseSchemaType "Report")
    schemaTypeToXML s x@Reports{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Report") $ reports_report x
            ]
 
data SubOrg = SubOrg
        { subOrg_type :: Xs.Int
        , subOrg_code :: Irct.String20
        }
        deriving (Eq,Show)
instance SchemaType SubOrg where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return SubOrg
            `apply` parseSchemaType "Type"
            `apply` parseSchemaType "Code"
    schemaTypeToXML s x@SubOrg{} =
        toXMLElement s []
            [ schemaTypeToXML "Type" $ subOrg_type x
            , schemaTypeToXML "Code" $ subOrg_code x
            ]
 
data SubOrgs = SubOrgs
        { subOrgs_subOrg :: [SubOrg]
        }
        deriving (Eq,Show)
instance SchemaType SubOrgs where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return SubOrgs
            `apply` many1 (parseSchemaType "SubOrg")
    schemaTypeToXML s x@SubOrgs{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "SubOrg") $ subOrgs_subOrg x
            ]
 
data Transaction = Transaction
        { transaction_basic :: TransactionBasic
        }
        deriving (Eq,Show)
instance SchemaType Transaction where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Transaction
            `apply` parseSchemaType "TransactionBasic"
    schemaTypeToXML s x@Transaction{} =
        toXMLElement s []
            [ schemaTypeToXML "TransactionBasic" $ transaction_basic x
            ]
 
data TransactionBasic = TransactionBasic
        { transBasic_summaryTransactionCode :: Xs.Int
        , transBasic_amount :: Maybe Irct.Decimal2
        }
        deriving (Eq,Show)
instance SchemaType TransactionBasic where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return TransactionBasic
            `apply` parseSchemaType "SummaryTransactionCode"
            `apply` optional (parseSchemaType "Amount")
    schemaTypeToXML s x@TransactionBasic{} =
        toXMLElement s []
            [ schemaTypeToXML "SummaryTransactionCode" $ transBasic_summaryTransactionCode x
            , maybe [] (schemaTypeToXML "Amount") $ transBasic_amount x
            ]
 
data Transactions = Transactions
        { transactions_transaction :: [Transaction]
        }
        deriving (Eq,Show)
instance SchemaType Transactions where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Transactions
            `apply` many1 (parseSchemaType "Transaction")
    schemaTypeToXML s x@Transactions{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Transaction") $ transactions_transaction x
            ]
