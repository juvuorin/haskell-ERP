{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.PayerSummaryReportsFromIRTypes'xsd
  ( module Data.PayerSummaryReportsFromIRTypes'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.IRCommonTypes'xsd as Irct
import Data.StatusMessageTypes'xsd as Smt
import Data.Xmldsig'core'schema'xsd as Ds
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
data AccidentInsurance = AccidentInsurance
        { accidInsur_accInsProvId :: Maybe Id
        , accidInsur_accInsPolicyNo :: Maybe Irct.String20
        }
        deriving (Eq,Show)
instance SchemaType AccidentInsurance where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return AccidentInsurance
            `apply` optional (parseSchemaType "AccInsProvId")
            `apply` optional (parseSchemaType "AccInsPolicyNo")
    schemaTypeToXML s x@AccidentInsurance{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "AccInsProvId") $ accidInsur_accInsProvId x
            , maybe [] (schemaTypeToXML "AccInsPolicyNo") $ accidInsur_accInsPolicyNo x
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
        , address_postalCode :: Maybe Irct.String20
        , address_postOffice :: Maybe Irct.String200
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
            `apply` optional (parseSchemaType "PostalCode")
            `apply` optional (parseSchemaType "PostOffice")
            `apply` optional (parseSchemaType "CountryCode")
            `apply` optional (parseSchemaType "CountryName")
    schemaTypeToXML s x@Address{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "Co") $ address_co x
            , maybe [] (schemaTypeToXML "Street") $ address_street x
            , maybe [] (schemaTypeToXML "POBox") $ address_pOBox x
            , maybe [] (schemaTypeToXML "PostalCode") $ address_postalCode x
            , maybe [] (schemaTypeToXML "PostOffice") $ address_postOffice x
            , maybe [] (schemaTypeToXML "CountryCode") $ address_countryCode x
            , maybe [] (schemaTypeToXML "CountryName") $ address_countryName x
            ]
 
data ContactPerson = ContactPerson
        { contactPerson_name :: Maybe Irct.String200
        , contactPerson_telephone :: Maybe Irct.String40
        , contactPerson_email :: Maybe Irct.String70
        , contactPerson_responsibilityCode :: Maybe Xs.Int
        }
        deriving (Eq,Show)
instance SchemaType ContactPerson where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return ContactPerson
            `apply` optional (parseSchemaType "Name")
            `apply` optional (parseSchemaType "Telephone")
            `apply` optional (parseSchemaType "Email")
            `apply` optional (parseSchemaType "ResponsibilityCode")
    schemaTypeToXML s x@ContactPerson{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "Name") $ contactPerson_name x
            , maybe [] (schemaTypeToXML "Telephone") $ contactPerson_telephone x
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
        , deliveryData_source :: Maybe Irct.String30
        , deliveryData_deliveryId :: Maybe Irct.String40
        , deliveryData_iRDeliveryId :: Irct.Guid
        , deliveryData_deliveryToIRChannelCode :: Maybe Xs.Int
        , deliveryData_contactPersons :: Maybe ContactPersons
        , deliveryData_reportDate :: Maybe Xsd.Date
        , deliveryData_payer :: Maybe Payer
        , deliveryData_owner :: Maybe Id
        , deliveryData_creator :: Maybe Id
        , deliveryData_sender :: Maybe Id
        }
        deriving (Eq,Show)
instance SchemaType DeliveryData where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DeliveryData
            `apply` parseSchemaType "Timestamp"
            `apply` optional (parseSchemaType "Source")
            `apply` optional (parseSchemaType "DeliveryId")
            `apply` parseSchemaType "IRDeliveryId"
            `apply` optional (parseSchemaType "DeliveryToIRChannelCode")
            `apply` optional (parseSchemaType "ContactPersons")
            `apply` optional (parseSchemaType "ReportDate")
            `apply` optional (parseSchemaType "Payer")
            `apply` optional (parseSchemaType "DeliveryDataOwner")
            `apply` optional (parseSchemaType "DeliveryDataCreator")
            `apply` optional (parseSchemaType "DeliveryDataSender")
    schemaTypeToXML s x@DeliveryData{} =
        toXMLElement s []
            [ schemaTypeToXML "Timestamp" $ deliveryData_timestamp x
            , maybe [] (schemaTypeToXML "Source") $ deliveryData_source x
            , maybe [] (schemaTypeToXML "DeliveryId") $ deliveryData_deliveryId x
            , schemaTypeToXML "IRDeliveryId" $ deliveryData_iRDeliveryId x
            , maybe [] (schemaTypeToXML "DeliveryToIRChannelCode") $ deliveryData_deliveryToIRChannelCode x
            , maybe [] (schemaTypeToXML "ContactPersons") $ deliveryData_contactPersons x
            , maybe [] (schemaTypeToXML "ReportDate") $ deliveryData_reportDate x
            , maybe [] (schemaTypeToXML "Payer") $ deliveryData_payer x
            , maybe [] (schemaTypeToXML "DeliveryDataOwner") $ deliveryData_owner x
            , maybe [] (schemaTypeToXML "DeliveryDataCreator") $ deliveryData_creator x
            , maybe [] (schemaTypeToXML "DeliveryDataSender") $ deliveryData_sender x
            ]
 
data Id = Id
        { id_type :: Maybe Xs.Int
        , id_code :: Maybe Irct.String30
        , id_countryCode :: Maybe Irct.String2
        , id_countryName :: Maybe Irct.String70
        }
        deriving (Eq,Show)
instance SchemaType Id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Id
            `apply` optional (parseSchemaType "Type")
            `apply` optional (parseSchemaType "Code")
            `apply` optional (parseSchemaType "CountryCode")
            `apply` optional (parseSchemaType "CountryName")
    schemaTypeToXML s x@Id{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "Type") $ id_type x
            , maybe [] (schemaTypeToXML "Code") $ id_code x
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
        , payerBasic_birthDate :: Maybe Xsd.Date
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
        { payerOther_sensitiveInfoIncluded :: Maybe Irct.True
        , payerOther_payerTypes :: Maybe PayerTypes
        }
        deriving (Eq,Show)
instance SchemaType PayerOther where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PayerOther
            `apply` optional (parseSchemaType "SensitiveInfoIncluded")
            `apply` optional (parseSchemaType "PayerTypes")
    schemaTypeToXML s x@PayerOther{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "SensitiveInfoIncluded") $ payerOther_sensitiveInfoIncluded x
            , maybe [] (schemaTypeToXML "PayerTypes") $ payerOther_payerTypes x
            ]
 
data PayerSummaryReportsFromIR = PayerSummaryReportsFromIR
        { payerSummaryReportsFromIR_subscription :: Subscription
        , payerSummaryReportsFromIR_query :: Maybe Query
        , payerSummaryReportsFromIR_summary :: Summary
        , payerSummaryReportsFromIR_messageErrors :: Maybe Smt.MessageErrors
        , payerSummaryReportsFromIR_deliveryErrors :: Maybe Smt.DeliveryErrors
        , payerSummaryReportsFromIR_reports :: Maybe Reports
        , payerSummaryReportsFromIR_signature :: SignatureType
        }
        deriving (Eq,Show)
instance SchemaType PayerSummaryReportsFromIR where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PayerSummaryReportsFromIR
            `apply` parseSchemaType "Subscription"
            `apply` optional (parseSchemaType "Query")
            `apply` parseSchemaType "Summary"
            `apply` optional (parseSchemaType "MessageErrors")
            `apply` optional (parseSchemaType "DeliveryErrors")
            `apply` optional (parseSchemaType "Reports")
            `apply` elementSignature
    schemaTypeToXML s x@PayerSummaryReportsFromIR{} =
        toXMLElement s []
            [ schemaTypeToXML "Subscription" $ payerSummaryReportsFromIR_subscription x
            , maybe [] (schemaTypeToXML "Query") $ payerSummaryReportsFromIR_query x
            , schemaTypeToXML "Summary" $ payerSummaryReportsFromIR_summary x
            , maybe [] (schemaTypeToXML "MessageErrors") $ payerSummaryReportsFromIR_messageErrors x
            , maybe [] (schemaTypeToXML "DeliveryErrors") $ payerSummaryReportsFromIR_deliveryErrors x
            , maybe [] (schemaTypeToXML "Reports") $ payerSummaryReportsFromIR_reports x
            , elementToXMLSignature $ payerSummaryReportsFromIR_signature x
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
        { pensionInsur_pensionProvIdCode :: Maybe Irct.String10
        , pensionInsur_pensionPolicyNo :: Maybe Irct.PensionPolicyNo
        }
        deriving (Eq,Show)
instance SchemaType PensionInsurance where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PensionInsurance
            `apply` optional (parseSchemaType "PensionProvIdCode")
            `apply` optional (parseSchemaType "PensionPolicyNo")
    schemaTypeToXML s x@PensionInsurance{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "PensionProvIdCode") $ pensionInsur_pensionProvIdCode x
            , maybe [] (schemaTypeToXML "PensionPolicyNo") $ pensionInsur_pensionPolicyNo x
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
 
data Query = Query
        { query_iRQueryId :: Irct.Guid
        , query_timestamp :: Xs.DateTime
        , query_timespanStart :: Xs.DateTime
        , query_timespanEnd :: Xs.DateTime
        }
        deriving (Eq,Show)
instance SchemaType Query where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Query
            `apply` parseSchemaType "IRQueryId"
            `apply` parseSchemaType "QueryTimestamp"
            `apply` parseSchemaType "QueryTimespanStart"
            `apply` parseSchemaType "QueryTimespanEnd"
    schemaTypeToXML s x@Query{} =
        toXMLElement s []
            [ schemaTypeToXML "IRQueryId" $ query_iRQueryId x
            , schemaTypeToXML "QueryTimestamp" $ query_timestamp x
            , schemaTypeToXML "QueryTimespanStart" $ query_timespanStart x
            , schemaTypeToXML "QueryTimespanEnd" $ query_timespanEnd x
            ]
 
data Report = Report
        { report_deliveryData :: DeliveryData
        , report_data :: ReportData
        , report_paymentMonth :: PaymentMonth
        , report_transactions :: Maybe Transactions
        }
        deriving (Eq,Show)
instance SchemaType Report where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Report
            `apply` parseSchemaType "DeliveryData"
            `apply` parseSchemaType "ReportData"
            `apply` parseSchemaType "PaymentMonth"
            `apply` optional (parseSchemaType "Transactions")
    schemaTypeToXML s x@Report{} =
        toXMLElement s []
            [ schemaTypeToXML "DeliveryData" $ report_deliveryData x
            , schemaTypeToXML "ReportData" $ report_data x
            , schemaTypeToXML "PaymentMonth" $ report_paymentMonth x
            , maybe [] (schemaTypeToXML "Transactions") $ report_transactions x
            ]
 
data ReportData = ReportData
        { reportData_iRReportId :: Irct.Guid
        , reportData_reportId :: Maybe Irct.String40
        , reportData_reportStatus :: Xs.Int
        , reportData_reportVersion :: Xs.Int
        , reportData_receivedTimestamp :: Xs.DateTime
        , reportData_createdTimestamp :: Maybe Xs.DateTime
        , reportData_versionReceivedTimestamp :: Maybe Xs.DateTime
        }
        deriving (Eq,Show)
instance SchemaType ReportData where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return ReportData
            `apply` parseSchemaType "IRReportId"
            `apply` optional (parseSchemaType "ReportId")
            `apply` parseSchemaType "ReportStatus"
            `apply` parseSchemaType "ReportVersion"
            `apply` parseSchemaType "ReceivedTimestamp"
            `apply` optional (parseSchemaType "CreatedTimestamp")
            `apply` optional (parseSchemaType "VersionReceivedTimestamp")
    schemaTypeToXML s x@ReportData{} =
        toXMLElement s []
            [ schemaTypeToXML "IRReportId" $ reportData_iRReportId x
            , maybe [] (schemaTypeToXML "ReportId") $ reportData_reportId x
            , schemaTypeToXML "ReportStatus" $ reportData_reportStatus x
            , schemaTypeToXML "ReportVersion" $ reportData_reportVersion x
            , schemaTypeToXML "ReceivedTimestamp" $ reportData_receivedTimestamp x
            , maybe [] (schemaTypeToXML "CreatedTimestamp") $ reportData_createdTimestamp x
            , maybe [] (schemaTypeToXML "VersionReceivedTimestamp") $ reportData_versionReceivedTimestamp x
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
        { subOrg_type :: Maybe Xs.Int
        , subOrg_code :: Maybe Irct.String20
        }
        deriving (Eq,Show)
instance SchemaType SubOrg where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return SubOrg
            `apply` optional (parseSchemaType "Type")
            `apply` optional (parseSchemaType "Code")
    schemaTypeToXML s x@SubOrg{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "Type") $ subOrg_type x
            , maybe [] (schemaTypeToXML "Code") $ subOrg_code x
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
 
data Subscription = Subscription
        { subscription_queryDataType :: Xs.Int
        , subscription_queryProfile :: Maybe Irct.String40
        , subscription_productionEnvironment :: Irct.TrueOrFalse
        , subscription_iRMainSubscriptionId :: Maybe Irct.Guid
        , subscription_iRSubscriptionId :: Maybe Irct.Guid
        , subscription_mainSubscriptionId :: Maybe Irct.String40
        , subscription_id :: Maybe Irct.String40
        }
        deriving (Eq,Show)
instance SchemaType Subscription where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Subscription
            `apply` parseSchemaType "QueryDataType"
            `apply` optional (parseSchemaType "QueryProfile")
            `apply` parseSchemaType "ProductionEnvironment"
            `apply` optional (parseSchemaType "IRMainSubscriptionId")
            `apply` optional (parseSchemaType "IRSubscriptionId")
            `apply` optional (parseSchemaType "MainSubscriptionId")
            `apply` optional (parseSchemaType "SubscriptionId")
    schemaTypeToXML s x@Subscription{} =
        toXMLElement s []
            [ schemaTypeToXML "QueryDataType" $ subscription_queryDataType x
            , maybe [] (schemaTypeToXML "QueryProfile") $ subscription_queryProfile x
            , schemaTypeToXML "ProductionEnvironment" $ subscription_productionEnvironment x
            , maybe [] (schemaTypeToXML "IRMainSubscriptionId") $ subscription_iRMainSubscriptionId x
            , maybe [] (schemaTypeToXML "IRSubscriptionId") $ subscription_iRSubscriptionId x
            , maybe [] (schemaTypeToXML "MainSubscriptionId") $ subscription_mainSubscriptionId x
            , maybe [] (schemaTypeToXML "SubscriptionId") $ subscription_id x
            ]
 
data Summary = Summary
        { summary_nrOfReports :: Xs.Int
        }
        deriving (Eq,Show)
instance SchemaType Summary where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Summary
            `apply` parseSchemaType "NrOfReports"
    schemaTypeToXML s x@Summary{} =
        toXMLElement s []
            [ schemaTypeToXML "NrOfReports" $ summary_nrOfReports x
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
        { transBasic_summaryTransactionCode :: Maybe Xs.Int
        , transBasic_amount :: Maybe Irct.Decimal2
        }
        deriving (Eq,Show)
instance SchemaType TransactionBasic where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return TransactionBasic
            `apply` optional (parseSchemaType "SummaryTransactionCode")
            `apply` optional (parseSchemaType "Amount")
    schemaTypeToXML s x@TransactionBasic{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "SummaryTransactionCode") $ transBasic_summaryTransactionCode x
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
