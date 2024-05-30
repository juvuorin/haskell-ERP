{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.BenefitReportsFromIRTypes'xsd
  ( module Data.BenefitReportsFromIRTypes'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.IRCommonTypes'xsd as Irct
import Data.StatusMessageTypes'xsd as Smt
import Data.Xmldsig'core'schema'xsd as Ds
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
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
 
data BenefitReportsFromIR = BenefitReportsFromIR
        { benefitReportsFromIR_subscription :: Subscription
        , benefitReportsFromIR_query :: Maybe Query
        , benefitReportsFromIR_summary :: Summary
        , benefitReportsFromIR_messageErrors :: Maybe Smt.MessageErrors
        , benefitReportsFromIR_deliveryErrors :: Maybe Smt.DeliveryErrors
        , benefitReportsFromIR_reports :: Maybe Reports
        , benefitReportsFromIR_signature :: SignatureType
        }
        deriving (Eq,Show)
instance SchemaType BenefitReportsFromIR where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return BenefitReportsFromIR
            `apply` parseSchemaType "Subscription"
            `apply` optional (parseSchemaType "Query")
            `apply` parseSchemaType "Summary"
            `apply` optional (parseSchemaType "MessageErrors")
            `apply` optional (parseSchemaType "DeliveryErrors")
            `apply` optional (parseSchemaType "Reports")
            `apply` elementSignature
    schemaTypeToXML s x@BenefitReportsFromIR{} =
        toXMLElement s []
            [ schemaTypeToXML "Subscription" $ benefitReportsFromIR_subscription x
            , maybe [] (schemaTypeToXML "Query") $ benefitReportsFromIR_query x
            , schemaTypeToXML "Summary" $ benefitReportsFromIR_summary x
            , maybe [] (schemaTypeToXML "MessageErrors") $ benefitReportsFromIR_messageErrors x
            , maybe [] (schemaTypeToXML "DeliveryErrors") $ benefitReportsFromIR_deliveryErrors x
            , maybe [] (schemaTypeToXML "Reports") $ benefitReportsFromIR_reports x
            , elementToXMLSignature $ benefitReportsFromIR_signature x
            ]
 
data BenefitUnit = BenefitUnit
        { benefitUnit_unitCode :: Maybe Xs.Int
        , benefitUnit_unitAmount :: Maybe Irct.Decimal2
        }
        deriving (Eq,Show)
instance SchemaType BenefitUnit where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return BenefitUnit
            `apply` optional (parseSchemaType "UnitCode")
            `apply` optional (parseSchemaType "UnitAmount")
    schemaTypeToXML s x@BenefitUnit{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "UnitCode") $ benefitUnit_unitCode x
            , maybe [] (schemaTypeToXML "UnitAmount") $ benefitUnit_unitAmount x
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
 
data Deduction = Deduction
        { deduction_transactionCode :: Maybe Xs.Int
        , deduction_type :: Maybe Xs.Int
        , deduction_amount :: Maybe Irct.Decimal2
        , deduction_origBenefitEarningPeriod :: Maybe OrigBenefitEarningPeriod
        , deduction_incomeBeneficiary :: Maybe IncomeBeneficiary
        , deduction_remittancePeriod :: Maybe RemittancePeriod
        }
        deriving (Eq,Show)
instance SchemaType Deduction where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Deduction
            `apply` optional (parseSchemaType "DeductionTransactionCode")
            `apply` optional (parseSchemaType "DeductionType")
            `apply` optional (parseSchemaType "Amount")
            `apply` optional (parseSchemaType "OrigBenefitEarningPeriod")
            `apply` optional (parseSchemaType "IncomeBeneficiary")
            `apply` optional (parseSchemaType "RemittancePeriod")
    schemaTypeToXML s x@Deduction{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "DeductionTransactionCode") $ deduction_transactionCode x
            , maybe [] (schemaTypeToXML "DeductionType") $ deduction_type x
            , maybe [] (schemaTypeToXML "Amount") $ deduction_amount x
            , maybe [] (schemaTypeToXML "OrigBenefitEarningPeriod") $ deduction_origBenefitEarningPeriod x
            , maybe [] (schemaTypeToXML "IncomeBeneficiary") $ deduction_incomeBeneficiary x
            , maybe [] (schemaTypeToXML "RemittancePeriod") $ deduction_remittancePeriod x
            ]
 
data Deductions = Deductions
        { deductions_deduction :: [Deduction]
        }
        deriving (Eq,Show)
instance SchemaType Deductions where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Deductions
            `apply` many1 (parseSchemaType "Deduction")
    schemaTypeToXML s x@Deductions{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Deduction") $ deductions_deduction x
            ]
 
data DeliveryData = DeliveryData
        { deliveryData_timestamp :: Xs.DateTime
        , deliveryData_source :: Maybe Irct.String30
        , deliveryData_deliveryId :: Maybe Irct.String40
        , deliveryData_iRDeliveryId :: Irct.Guid
        , deliveryData_deliveryToIRChannelCode :: Maybe Xs.Int
        , deliveryData_paymentDate :: Xsd.Date
        , deliveryData_contactPersons :: Maybe ContactPersons
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
            `apply` parseSchemaType "PaymentDate"
            `apply` optional (parseSchemaType "ContactPersons")
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
            , schemaTypeToXML "PaymentDate" $ deliveryData_paymentDate x
            , maybe [] (schemaTypeToXML "ContactPersons") $ deliveryData_contactPersons x
            , maybe [] (schemaTypeToXML "Payer") $ deliveryData_payer x
            , maybe [] (schemaTypeToXML "DeliveryDataOwner") $ deliveryData_owner x
            , maybe [] (schemaTypeToXML "DeliveryDataCreator") $ deliveryData_creator x
            , maybe [] (schemaTypeToXML "DeliveryDataSender") $ deliveryData_sender x
            ]
 
data EarningPeriod = EarningPeriod
        { earningPeriod_startDate :: Maybe Xsd.Date
        , earningPeriod_endDate :: Maybe Xsd.Date
        }
        deriving (Eq,Show)
instance SchemaType EarningPeriod where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return EarningPeriod
            `apply` optional (parseSchemaType "StartDate")
            `apply` optional (parseSchemaType "EndDate")
    schemaTypeToXML s x@EarningPeriod{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "StartDate") $ earningPeriod_startDate x
            , maybe [] (schemaTypeToXML "EndDate") $ earningPeriod_endDate x
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
 
data IncomeBeneficiary = IncomeBeneficiary
        { incomeBenef_sensitiveInfoIncluded :: Maybe Irct.True
        , incomeBenef_ids :: Maybe IncomeBeneficiaryIds
        , incomeBenef_basic :: Maybe IncomeBeneficiaryBasic
        , incomeBenef_address :: Maybe Address
        }
        deriving (Eq,Show)
instance SchemaType IncomeBeneficiary where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return IncomeBeneficiary
            `apply` optional (parseSchemaType "SensitiveInfoIncluded")
            `apply` optional (parseSchemaType "IncomeBeneficiaryIds")
            `apply` optional (parseSchemaType "IncomeBeneficiaryBasic")
            `apply` optional (parseSchemaType "Address")
    schemaTypeToXML s x@IncomeBeneficiary{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "SensitiveInfoIncluded") $ incomeBenef_sensitiveInfoIncluded x
            , maybe [] (schemaTypeToXML "IncomeBeneficiaryIds") $ incomeBenef_ids x
            , maybe [] (schemaTypeToXML "IncomeBeneficiaryBasic") $ incomeBenef_basic x
            , maybe [] (schemaTypeToXML "Address") $ incomeBenef_address x
            ]
 
data IncomeBeneficiaryBasic = IncomeBeneficiaryBasic
        { incomeBenefBasic_missingId :: Maybe Irct.True
        , incomeBenefBasic_companyName :: Maybe Irct.String200
        , incomeBenefBasic_lastName :: Maybe Irct.String200
        , incomeBenefBasic_firstName :: Maybe Irct.String100
        , incomeBenefBasic_birthDate :: Maybe Xsd.Date
        , incomeBenefBasic_gender :: Maybe Xs.Int
        , incomeBenefBasic_incomeBeneficiaryType :: Maybe Xs.Int
        }
        deriving (Eq,Show)
instance SchemaType IncomeBeneficiaryBasic where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return IncomeBeneficiaryBasic
            `apply` optional (parseSchemaType "MissingId")
            `apply` optional (parseSchemaType "CompanyName")
            `apply` optional (parseSchemaType "LastName")
            `apply` optional (parseSchemaType "FirstName")
            `apply` optional (parseSchemaType "BirthDate")
            `apply` optional (parseSchemaType "Gender")
            `apply` optional (parseSchemaType "IncomeBeneficiaryType")
    schemaTypeToXML s x@IncomeBeneficiaryBasic{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "MissingId") $ incomeBenefBasic_missingId x
            , maybe [] (schemaTypeToXML "CompanyName") $ incomeBenefBasic_companyName x
            , maybe [] (schemaTypeToXML "LastName") $ incomeBenefBasic_lastName x
            , maybe [] (schemaTypeToXML "FirstName") $ incomeBenefBasic_firstName x
            , maybe [] (schemaTypeToXML "BirthDate") $ incomeBenefBasic_birthDate x
            , maybe [] (schemaTypeToXML "Gender") $ incomeBenefBasic_gender x
            , maybe [] (schemaTypeToXML "IncomeBeneficiaryType") $ incomeBenefBasic_incomeBeneficiaryType x
            ]
 
data IncomeBeneficiaryIds = IncomeBeneficiaryIds
        { incomeBenefIds_id :: [Id]
        }
        deriving (Eq,Show)
instance SchemaType IncomeBeneficiaryIds where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return IncomeBeneficiaryIds
            `apply` many1 (parseSchemaType "Id")
    schemaTypeToXML s x@IncomeBeneficiaryIds{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Id") $ incomeBenefIds_id x
            ]
 
data IncomeEarner = IncomeEarner
        { incomeEarner_sensitiveInfoIncluded :: Maybe Irct.True
        , incomeEarner_ids :: Maybe IncomeEarnerIds
        , incomeEarner_basic :: Maybe IncomeEarnerBasic
        , incomeEarner_address :: Maybe Address
        , incomeEarner_internationalData :: Maybe InternationalData
        }
        deriving (Eq,Show)
instance SchemaType IncomeEarner where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return IncomeEarner
            `apply` optional (parseSchemaType "SensitiveInfoIncluded")
            `apply` optional (parseSchemaType "IncomeEarnerIds")
            `apply` optional (parseSchemaType "IncomeEarnerBasic")
            `apply` optional (parseSchemaType "Address")
            `apply` optional (parseSchemaType "InternationalData")
    schemaTypeToXML s x@IncomeEarner{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "SensitiveInfoIncluded") $ incomeEarner_sensitiveInfoIncluded x
            , maybe [] (schemaTypeToXML "IncomeEarnerIds") $ incomeEarner_ids x
            , maybe [] (schemaTypeToXML "IncomeEarnerBasic") $ incomeEarner_basic x
            , maybe [] (schemaTypeToXML "Address") $ incomeEarner_address x
            , maybe [] (schemaTypeToXML "InternationalData") $ incomeEarner_internationalData x
            ]
 
data IncomeEarnerBasic = IncomeEarnerBasic
        { incomeEarnerBasic_missingId :: Maybe Irct.True
        , incomeEarnerBasic_companyName :: Maybe Irct.String200
        , incomeEarnerBasic_lastName :: Maybe Irct.String200
        , incomeEarnerBasic_firstName :: Maybe Irct.String100
        , incomeEarnerBasic_birthDate :: Maybe Xsd.Date
        , incomeEarnerBasic_gender :: Maybe Xs.Int
        }
        deriving (Eq,Show)
instance SchemaType IncomeEarnerBasic where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return IncomeEarnerBasic
            `apply` optional (parseSchemaType "MissingId")
            `apply` optional (parseSchemaType "CompanyName")
            `apply` optional (parseSchemaType "LastName")
            `apply` optional (parseSchemaType "FirstName")
            `apply` optional (parseSchemaType "BirthDate")
            `apply` optional (parseSchemaType "Gender")
    schemaTypeToXML s x@IncomeEarnerBasic{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "MissingId") $ incomeEarnerBasic_missingId x
            , maybe [] (schemaTypeToXML "CompanyName") $ incomeEarnerBasic_companyName x
            , maybe [] (schemaTypeToXML "LastName") $ incomeEarnerBasic_lastName x
            , maybe [] (schemaTypeToXML "FirstName") $ incomeEarnerBasic_firstName x
            , maybe [] (schemaTypeToXML "BirthDate") $ incomeEarnerBasic_birthDate x
            , maybe [] (schemaTypeToXML "Gender") $ incomeEarnerBasic_gender x
            ]
 
data IncomeEarnerIds = IncomeEarnerIds
        { incomeEarnerIds_id :: [Id]
        }
        deriving (Eq,Show)
instance SchemaType IncomeEarnerIds where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return IncomeEarnerIds
            `apply` many1 (parseSchemaType "Id")
    schemaTypeToXML s x@IncomeEarnerIds{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Id") $ incomeEarnerIds_id x
            ]
 
data Insurance = Insurance
        { insurance_no :: Maybe Irct.String20
        , insurance_policyHolder :: Maybe InsurancePolicyHolder
        }
        deriving (Eq,Show)
instance SchemaType Insurance where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Insurance
            `apply` optional (parseSchemaType "InsuranceNo")
            `apply` optional (parseSchemaType "InsurancePolicyHolder")
    schemaTypeToXML s x@Insurance{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "InsuranceNo") $ insurance_no x
            , maybe [] (schemaTypeToXML "InsurancePolicyHolder") $ insurance_policyHolder x
            ]
 
data InsurancePolicyHolder = InsurancePolicyHolder
        { insurPolicyHolder_sensitiveInfoIncluded :: Maybe Irct.True
        , insurPolicyHolder_ids :: Maybe InsurancePolicyHolderIds
        , insurPolicyHolder_basic :: Maybe InsurancePolicyHolderBasic
        , insurPolicyHolder_address :: Maybe Address
        }
        deriving (Eq,Show)
instance SchemaType InsurancePolicyHolder where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return InsurancePolicyHolder
            `apply` optional (parseSchemaType "SensitiveInfoIncluded")
            `apply` optional (parseSchemaType "InsurancePolicyHolderIds")
            `apply` optional (parseSchemaType "InsurancePolicyHolderBasic")
            `apply` optional (parseSchemaType "Address")
    schemaTypeToXML s x@InsurancePolicyHolder{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "SensitiveInfoIncluded") $ insurPolicyHolder_sensitiveInfoIncluded x
            , maybe [] (schemaTypeToXML "InsurancePolicyHolderIds") $ insurPolicyHolder_ids x
            , maybe [] (schemaTypeToXML "InsurancePolicyHolderBasic") $ insurPolicyHolder_basic x
            , maybe [] (schemaTypeToXML "Address") $ insurPolicyHolder_address x
            ]
 
data InsurancePolicyHolderBasic = InsurancePolicyHolderBasic
        { insurPolicyHolderBasic_missingId :: Maybe Irct.True
        , insurPolicyHolderBasic_companyName :: Maybe Irct.String200
        , insurPolicyHolderBasic_lastName :: Maybe Irct.String200
        , insurPolicyHolderBasic_firstName :: Maybe Irct.String100
        , insurPolicyHolderBasic_birthDate :: Maybe Xsd.Date
        , insurPolicyHolderBasic_gender :: Maybe Xs.Int
        }
        deriving (Eq,Show)
instance SchemaType InsurancePolicyHolderBasic where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return InsurancePolicyHolderBasic
            `apply` optional (parseSchemaType "MissingId")
            `apply` optional (parseSchemaType "CompanyName")
            `apply` optional (parseSchemaType "LastName")
            `apply` optional (parseSchemaType "FirstName")
            `apply` optional (parseSchemaType "BirthDate")
            `apply` optional (parseSchemaType "Gender")
    schemaTypeToXML s x@InsurancePolicyHolderBasic{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "MissingId") $ insurPolicyHolderBasic_missingId x
            , maybe [] (schemaTypeToXML "CompanyName") $ insurPolicyHolderBasic_companyName x
            , maybe [] (schemaTypeToXML "LastName") $ insurPolicyHolderBasic_lastName x
            , maybe [] (schemaTypeToXML "FirstName") $ insurPolicyHolderBasic_firstName x
            , maybe [] (schemaTypeToXML "BirthDate") $ insurPolicyHolderBasic_birthDate x
            , maybe [] (schemaTypeToXML "Gender") $ insurPolicyHolderBasic_gender x
            ]
 
data InsurancePolicyHolderIds = InsurancePolicyHolderIds
        { insurPolicyHolderIds_id :: [Id]
        }
        deriving (Eq,Show)
instance SchemaType InsurancePolicyHolderIds where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return InsurancePolicyHolderIds
            `apply` many1 (parseSchemaType "Id")
    schemaTypeToXML s x@InsurancePolicyHolderIds{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Id") $ insurPolicyHolderIds_id x
            ]
 
data InternationalData = InternationalData
        { internData_nonResident :: Maybe Irct.True
        , internData_nonResidentCountryCode :: Maybe Irct.String2
        , internData_nonResidentCountryName :: Maybe Irct.String70
        , internData_subToWithhold :: Maybe Irct.True
        }
        deriving (Eq,Show)
instance SchemaType InternationalData where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return InternationalData
            `apply` optional (parseSchemaType "NonResident")
            `apply` optional (parseSchemaType "NonResidentCountryCode")
            `apply` optional (parseSchemaType "NonResidentCountryName")
            `apply` optional (parseSchemaType "SubToWithhold")
    schemaTypeToXML s x@InternationalData{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "NonResident") $ internData_nonResident x
            , maybe [] (schemaTypeToXML "NonResidentCountryCode") $ internData_nonResidentCountryCode x
            , maybe [] (schemaTypeToXML "NonResidentCountryName") $ internData_nonResidentCountryName x
            , maybe [] (schemaTypeToXML "SubToWithhold") $ internData_subToWithhold x
            ]
 
data OrigBenefitEarningPeriod = OrigBenefitEarningPeriod
        { origBenefitEarningPeriod_paymentDate :: Maybe Xsd.Date
        , origBenefitEarningPeriod_startDate :: Maybe Xsd.Date
        , origBenefitEarningPeriod_endDate :: Maybe Xsd.Date
        }
        deriving (Eq,Show)
instance SchemaType OrigBenefitEarningPeriod where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return OrigBenefitEarningPeriod
            `apply` optional (parseSchemaType "PaymentDate")
            `apply` optional (parseSchemaType "StartDate")
            `apply` optional (parseSchemaType "EndDate")
    schemaTypeToXML s x@OrigBenefitEarningPeriod{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "PaymentDate") $ origBenefitEarningPeriod_paymentDate x
            , maybe [] (schemaTypeToXML "StartDate") $ origBenefitEarningPeriod_startDate x
            , maybe [] (schemaTypeToXML "EndDate") $ origBenefitEarningPeriod_endDate x
            ]
 
data OrigEarningPeriod = OrigEarningPeriod
        { origEarningPeriod_paymentDate :: Maybe Xsd.Date
        , origEarningPeriod_startDate :: Maybe Xsd.Date
        , origEarningPeriod_endDate :: Maybe Xsd.Date
        }
        deriving (Eq,Show)
instance SchemaType OrigEarningPeriod where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return OrigEarningPeriod
            `apply` optional (parseSchemaType "PaymentDate")
            `apply` optional (parseSchemaType "StartDate")
            `apply` optional (parseSchemaType "EndDate")
    schemaTypeToXML s x@OrigEarningPeriod{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "PaymentDate") $ origEarningPeriod_paymentDate x
            , maybe [] (schemaTypeToXML "StartDate") $ origEarningPeriod_startDate x
            , maybe [] (schemaTypeToXML "EndDate") $ origEarningPeriod_endDate x
            ]
 
data Payer = Payer
        { payer_ids :: Maybe PayerIds
        , payer_basic :: Maybe PayerBasic
        , payer_address :: Maybe Address
        , payer_subOrgs :: Maybe SubOrgs
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
    schemaTypeToXML s x@Payer{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "PayerIds") $ payer_ids x
            , maybe [] (schemaTypeToXML "PayerBasic") $ payer_basic x
            , maybe [] (schemaTypeToXML "Address") $ payer_address x
            , maybe [] (schemaTypeToXML "SubOrgs") $ payer_subOrgs x
            ]
 
data PayerBasic = PayerBasic
        { payerBasic_companyName :: Irct.String200
        }
        deriving (Eq,Show)
instance SchemaType PayerBasic where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PayerBasic
            `apply` parseSchemaType "CompanyName"
    schemaTypeToXML s x@PayerBasic{} =
        toXMLElement s []
            [ schemaTypeToXML "CompanyName" $ payerBasic_companyName x
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
 
data RecourseData = RecourseData
        { recourseData_recoursePaymentDate :: Xsd.Date
        }
        deriving (Eq,Show)
instance SchemaType RecourseData where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return RecourseData
            `apply` parseSchemaType "RecoursePaymentDate"
    schemaTypeToXML s x@RecourseData{} =
        toXMLElement s []
            [ schemaTypeToXML "RecoursePaymentDate" $ recourseData_recoursePaymentDate x
            ]
 
data RecoveryData = RecoveryData
        { recoveryData_recoveryDate :: Maybe Xsd.Date
        , recoveryData_withhold :: Maybe Irct.Decimal2
        , recoveryData_taxAtSource :: Maybe Irct.Decimal2
        , recoveryData_origEarningPeriod :: Maybe OrigEarningPeriod
        }
        deriving (Eq,Show)
instance SchemaType RecoveryData where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return RecoveryData
            `apply` optional (parseSchemaType "RecoveryDate")
            `apply` optional (parseSchemaType "Withhold")
            `apply` optional (parseSchemaType "TaxAtSource")
            `apply` optional (parseSchemaType "OrigEarningPeriod")
    schemaTypeToXML s x@RecoveryData{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "RecoveryDate") $ recoveryData_recoveryDate x
            , maybe [] (schemaTypeToXML "Withhold") $ recoveryData_withhold x
            , maybe [] (schemaTypeToXML "TaxAtSource") $ recoveryData_taxAtSource x
            , maybe [] (schemaTypeToXML "OrigEarningPeriod") $ recoveryData_origEarningPeriod x
            ]
 
data RemittancePeriod = RemittancePeriod
        { remittPeriod_startDate :: Maybe Xsd.Date
        , remittPeriod_endDate :: Maybe Xsd.Date
        }
        deriving (Eq,Show)
instance SchemaType RemittancePeriod where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return RemittancePeriod
            `apply` optional (parseSchemaType "StartDate")
            `apply` optional (parseSchemaType "EndDate")
    schemaTypeToXML s x@RemittancePeriod{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "StartDate") $ remittPeriod_startDate x
            , maybe [] (schemaTypeToXML "EndDate") $ remittPeriod_endDate x
            ]
 
data Report = Report
        { report_deliveryData :: DeliveryData
        , report_data :: ReportData
        , report_incomeEarner :: Maybe IncomeEarner
        , report_transactions :: Maybe Transactions
        }
        deriving (Eq,Show)
instance SchemaType Report where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Report
            `apply` parseSchemaType "DeliveryData"
            `apply` parseSchemaType "ReportData"
            `apply` optional (parseSchemaType "IncomeEarner")
            `apply` optional (parseSchemaType "Transactions")
    schemaTypeToXML s x@Report{} =
        toXMLElement s []
            [ schemaTypeToXML "DeliveryData" $ report_deliveryData x
            , schemaTypeToXML "ReportData" $ report_data x
            , maybe [] (schemaTypeToXML "IncomeEarner") $ report_incomeEarner x
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
        { transaction_basic :: Maybe TransactionBasic
        , transaction_earningPeriod :: Maybe EarningPeriod
        , transaction_benefitUnit :: Maybe BenefitUnit
        , transaction_other :: Maybe TransactionOther
        , transaction_recoveryData :: Maybe RecoveryData
        , transaction_recourseData :: Maybe RecourseData
        , transaction_insurance :: Maybe Insurance
        , transaction_deductions :: Maybe Deductions
        }
        deriving (Eq,Show)
instance SchemaType Transaction where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Transaction
            `apply` optional (parseSchemaType "TransactionBasic")
            `apply` optional (parseSchemaType "EarningPeriod")
            `apply` optional (parseSchemaType "BenefitUnit")
            `apply` optional (parseSchemaType "TransactionOther")
            `apply` optional (parseSchemaType "RecoveryData")
            `apply` optional (parseSchemaType "RecourseData")
            `apply` optional (parseSchemaType "Insurance")
            `apply` optional (parseSchemaType "Deductions")
    schemaTypeToXML s x@Transaction{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "TransactionBasic") $ transaction_basic x
            , maybe [] (schemaTypeToXML "EarningPeriod") $ transaction_earningPeriod x
            , maybe [] (schemaTypeToXML "BenefitUnit") $ transaction_benefitUnit x
            , maybe [] (schemaTypeToXML "TransactionOther") $ transaction_other x
            , maybe [] (schemaTypeToXML "RecoveryData") $ transaction_recoveryData x
            , maybe [] (schemaTypeToXML "RecourseData") $ transaction_recourseData x
            , maybe [] (schemaTypeToXML "Insurance") $ transaction_insurance x
            , maybe [] (schemaTypeToXML "Deductions") $ transaction_deductions x
            ]
 
data TransactionBasic = TransactionBasic
        { transBasic_transactionCode :: Maybe Xs.Int
        , transBasic_retroactiveTransactionCode :: Maybe Xs.Int
        , transBasic_amount :: Maybe Irct.Decimal2
        , transBasic_taxTreatment :: Maybe Xs.Int
        , transBasic_oneOff :: Maybe Irct.True
        , transBasic_unjustEnrichment :: Maybe Irct.True
        , transBasic_recovery :: Maybe Irct.True
        , transBasic_unpromptedRefund :: Maybe Irct.True
        , transBasic_delayIncrease :: Maybe Irct.True
        , transBasic_recourse :: Maybe Irct.True
        , transBasic_paymentReallocation :: Maybe Irct.True
        , transBasic_noTaxEffect :: Maybe Irct.True
        }
        deriving (Eq,Show)
instance SchemaType TransactionBasic where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return TransactionBasic
            `apply` optional (parseSchemaType "TransactionCode")
            `apply` optional (parseSchemaType "RetroactiveTransactionCode")
            `apply` optional (parseSchemaType "Amount")
            `apply` optional (parseSchemaType "TaxTreatment")
            `apply` optional (parseSchemaType "OneOff")
            `apply` optional (parseSchemaType "UnjustEnrichment")
            `apply` optional (parseSchemaType "Recovery")
            `apply` optional (parseSchemaType "UnpromptedRefund")
            `apply` optional (parseSchemaType "DelayIncrease")
            `apply` optional (parseSchemaType "Recourse")
            `apply` optional (parseSchemaType "PaymentReallocation")
            `apply` optional (parseSchemaType "NoTaxEffect")
    schemaTypeToXML s x@TransactionBasic{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "TransactionCode") $ transBasic_transactionCode x
            , maybe [] (schemaTypeToXML "RetroactiveTransactionCode") $ transBasic_retroactiveTransactionCode x
            , maybe [] (schemaTypeToXML "Amount") $ transBasic_amount x
            , maybe [] (schemaTypeToXML "TaxTreatment") $ transBasic_taxTreatment x
            , maybe [] (schemaTypeToXML "OneOff") $ transBasic_oneOff x
            , maybe [] (schemaTypeToXML "UnjustEnrichment") $ transBasic_unjustEnrichment x
            , maybe [] (schemaTypeToXML "Recovery") $ transBasic_recovery x
            , maybe [] (schemaTypeToXML "UnpromptedRefund") $ transBasic_unpromptedRefund x
            , maybe [] (schemaTypeToXML "DelayIncrease") $ transBasic_delayIncrease x
            , maybe [] (schemaTypeToXML "Recourse") $ transBasic_recourse x
            , maybe [] (schemaTypeToXML "PaymentReallocation") $ transBasic_paymentReallocation x
            , maybe [] (schemaTypeToXML "NoTaxEffect") $ transBasic_noTaxEffect x
            ]
 
data TransactionOther = TransactionOther
        { transOther_nonresidentPensionBasis :: Xs.Int
        }
        deriving (Eq,Show)
instance SchemaType TransactionOther where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return TransactionOther
            `apply` parseSchemaType "NonresidentPensionBasis"
    schemaTypeToXML s x@TransactionOther{} =
        toXMLElement s []
            [ schemaTypeToXML "NonresidentPensionBasis" $ transOther_nonresidentPensionBasis x
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
