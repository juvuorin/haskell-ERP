{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.WageReportsToIRTypes
  ( module Data.WageReportsToIRTypes
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.IRCommonTypes as Irct
import Data.Xmldsig as Ds
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
newtype AllowanceCode = AllowanceCode Xs.Int deriving (Eq,Show)
instance Restricts AllowanceCode Xs.Int where
    restricts (AllowanceCode x) = x
instance SchemaType AllowanceCode where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (AllowanceCode x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AllowanceCode where
    acceptingParser = fmap AllowanceCode acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (AllowanceCode x) = simpleTypeText x
 
newtype BenefitCode = BenefitCode Xs.Int deriving (Eq,Show)
instance Restricts BenefitCode Xs.Int where
    restricts (BenefitCode x) = x
instance SchemaType BenefitCode where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (BenefitCode x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType BenefitCode where
    acceptingParser = fmap BenefitCode acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (BenefitCode x) = simpleTypeText x
 
newtype ExceptionCode = ExceptionCode Xs.Int deriving (Eq,Show)
instance Restricts ExceptionCode Xs.Int where
    restricts (ExceptionCode x) = x
instance SchemaType ExceptionCode where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (ExceptionCode x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ExceptionCode where
    acceptingParser = fmap ExceptionCode acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (ExceptionCode x) = simpleTypeText x
 
newtype FormCode = FormCode Xs.Int deriving (Eq,Show)
instance Restricts FormCode Xs.Int where
    restricts (FormCode x) = x
instance SchemaType FormCode where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (FormCode x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType FormCode where
    acceptingParser = fmap FormCode acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (FormCode x) = simpleTypeText x
 
newtype IncomeEarnerType = IncomeEarnerType Xs.Int deriving (Eq,Show)
instance Restricts IncomeEarnerType Xs.Int where
    restricts (IncomeEarnerType x) = x
instance SchemaType IncomeEarnerType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (IncomeEarnerType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType IncomeEarnerType where
    acceptingParser = fmap IncomeEarnerType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (IncomeEarnerType x) = simpleTypeText x
 
newtype PaymentType = PaymentType Xs.Int deriving (Eq,Show)
instance Restricts PaymentType Xs.Int where
    restricts (PaymentType x) = x
instance SchemaType PaymentType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (PaymentType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PaymentType where
    acceptingParser = fmap PaymentType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (PaymentType x) = simpleTypeText x
 
newtype RemunerationCode = RemunerationCode Xs.Int deriving (Eq,Show)
instance Restricts RemunerationCode Xs.Int where
    restricts (RemunerationCode x) = x
instance SchemaType RemunerationCode where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (RemunerationCode x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType RemunerationCode where
    acceptingParser = fmap RemunerationCode acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (RemunerationCode x) = simpleTypeText x
 
newtype WorkMunicipality = WorkMunicipality Irct.String200 deriving (Eq,Show)
instance Restricts WorkMunicipality Irct.String200 where
    restricts (WorkMunicipality x) = x
instance SchemaType WorkMunicipality where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (WorkMunicipality x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType WorkMunicipality where
    acceptingParser = fmap WorkMunicipality acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (WorkMunicipality x) = simpleTypeText x
 
data Absence = Absence
        { absence_repStartDate :: Maybe Xs.Date
        , absence_repEndDate :: Maybe Xs.Date
        , absence_unpaidAbsence :: Maybe UnpaidAbsence
        , absence_paidAbsence :: Maybe PaidAbsence
        }
        deriving (Eq,Show)
instance SchemaType Absence where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Absence
            `apply` optional (parseSchemaType "AbsenceRepStartDate")
            `apply` optional (parseSchemaType "AbsenceRepEndDate")
            `apply` optional (parseSchemaType "UnpaidAbsence")
            `apply` optional (parseSchemaType "PaidAbsence")
    schemaTypeToXML s x@Absence{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "AbsenceRepStartDate") $ absence_repStartDate x
            , maybe [] (schemaTypeToXML "AbsenceRepEndDate") $ absence_repEndDate x
            , maybe [] (schemaTypeToXML "UnpaidAbsence") $ absence_unpaidAbsence x
            , maybe [] (schemaTypeToXML "PaidAbsence") $ absence_paidAbsence x
            ]
 
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
 
data Addresses = Addresses
        { addresses_typedAddress :: [TypedAddress]
        }
        deriving (Eq,Show)
instance SchemaType Addresses where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Addresses
            `apply` many1 (parseSchemaType "TypedAddress")
    schemaTypeToXML s x@Addresses{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "TypedAddress") $ addresses_typedAddress x
            ]
 
data CarBenefit = CarBenefit
        { carBenefit_code :: Xs.Int
        , carBenefit_ageGroupCode :: Xs.Int
        , carBenefit_kilometers :: Maybe Xs.Int
        , carBenefit_emissionsValue :: Maybe Irct.Decimal2
        }
        deriving (Eq,Show)
instance SchemaType CarBenefit where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return CarBenefit
            `apply` parseSchemaType "CarBenefitCode"
            `apply` parseSchemaType "AgeGroupCode"
            `apply` optional (parseSchemaType "Kilometers")
            `apply` optional (parseSchemaType "EmissionsValue")
    schemaTypeToXML s x@CarBenefit{} =
        toXMLElement s []
            [ schemaTypeToXML "CarBenefitCode" $ carBenefit_code x
            , schemaTypeToXML "AgeGroupCode" $ carBenefit_ageGroupCode x
            , maybe [] (schemaTypeToXML "Kilometers") $ carBenefit_kilometers x
            , maybe [] (schemaTypeToXML "EmissionsValue") $ carBenefit_emissionsValue x
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
 
data DailyAllowance = DailyAllowance
        { dailyAllow_allowanceCode :: [AllowanceCode]
        }
        deriving (Eq,Show)
instance SchemaType DailyAllowance where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DailyAllowance
            `apply` many1 (parseSchemaType "AllowanceCode")
    schemaTypeToXML s x@DailyAllowance{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "AllowanceCode") $ dailyAllow_allowanceCode x
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
        , deliveryData_paymentPeriod :: PaymentPeriod
        , deliveryData_contactPersons :: ContactPersons
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
            `apply` parseSchemaType "PaymentPeriod"
            `apply` parseSchemaType "ContactPersons"
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
            , schemaTypeToXML "PaymentPeriod" $ deliveryData_paymentPeriod x
            , schemaTypeToXML "ContactPersons" $ deliveryData_contactPersons x
            , schemaTypeToXML "Payer" $ deliveryData_payer x
            , schemaTypeToXML "Reports" $ deliveryData_reports x
            ]
 
data EarningPeriod = EarningPeriod
        { earningPeriod_startDate :: Xs.Date
        , earningPeriod_endDate :: Xs.Date
        }
        deriving (Eq,Show)
instance SchemaType EarningPeriod where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return EarningPeriod
            `apply` parseSchemaType "StartDate"
            `apply` parseSchemaType "EndDate"
    schemaTypeToXML s x@EarningPeriod{} =
        toXMLElement s []
            [ schemaTypeToXML "StartDate" $ earningPeriod_startDate x
            , schemaTypeToXML "EndDate" $ earningPeriod_endDate x
            ]
 
data EarningPeriods = EarningPeriods
        { earningPeriods_earningPeriod :: [EarningPeriod]
        }
        deriving (Eq,Show)
instance SchemaType EarningPeriods where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return EarningPeriods
            `apply` many1 (parseSchemaType "EarningPeriod")
    schemaTypeToXML s x@EarningPeriods{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "EarningPeriod") $ earningPeriods_earningPeriod x
            ]
 
data EmployeeMax183d = EmployeeMax183d
        { emploMax183d_countryCode :: Irct.String2
        }
        deriving (Eq,Show)
instance SchemaType EmployeeMax183d where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return EmployeeMax183d
            `apply` parseSchemaType "CountryCode"
    schemaTypeToXML s x@EmployeeMax183d{} =
        toXMLElement s []
            [ schemaTypeToXML "CountryCode" $ emploMax183d_countryCode x
            ]
 
data Employment = Employment
        { employment_employed :: Maybe Irct.TrueOrFalse
        , employment_code :: Maybe Xs.Int
        , employment_termCode :: Maybe Xs.Int
        , employment_partTime :: Maybe Irct.Decimal2
        , employment_hoursPerWeek :: Maybe Irct.Decimal2
        , employment_paymentTypes :: Maybe PaymentTypes
        , employment_periods :: Maybe EmploymentPeriods
        , employment_endings :: Maybe EmploymentEndings
        }
        deriving (Eq,Show)
instance SchemaType Employment where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Employment
            `apply` optional (parseSchemaType "Employed")
            `apply` optional (parseSchemaType "EmploymentCode")
            `apply` optional (parseSchemaType "TermCode")
            `apply` optional (parseSchemaType "PartTime")
            `apply` optional (parseSchemaType "HoursPerWeek")
            `apply` optional (parseSchemaType "PaymentTypes")
            `apply` optional (parseSchemaType "EmploymentPeriods")
            `apply` optional (parseSchemaType "EmploymentEndings")
    schemaTypeToXML s x@Employment{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "Employed") $ employment_employed x
            , maybe [] (schemaTypeToXML "EmploymentCode") $ employment_code x
            , maybe [] (schemaTypeToXML "TermCode") $ employment_termCode x
            , maybe [] (schemaTypeToXML "PartTime") $ employment_partTime x
            , maybe [] (schemaTypeToXML "HoursPerWeek") $ employment_hoursPerWeek x
            , maybe [] (schemaTypeToXML "PaymentTypes") $ employment_paymentTypes x
            , maybe [] (schemaTypeToXML "EmploymentPeriods") $ employment_periods x
            , maybe [] (schemaTypeToXML "EmploymentEndings") $ employment_endings x
            ]
 
data EmploymentEnding = EmploymentEnding
        { emploEnding_type :: Xs.Int
        , emploEnding_code :: Irct.String20
        }
        deriving (Eq,Show)
instance SchemaType EmploymentEnding where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return EmploymentEnding
            `apply` parseSchemaType "Type"
            `apply` parseSchemaType "Code"
    schemaTypeToXML s x@EmploymentEnding{} =
        toXMLElement s []
            [ schemaTypeToXML "Type" $ emploEnding_type x
            , schemaTypeToXML "Code" $ emploEnding_code x
            ]
 
data EmploymentEndings = EmploymentEndings
        { emploEndings_employmentEnding :: [EmploymentEnding]
        }
        deriving (Eq,Show)
instance SchemaType EmploymentEndings where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return EmploymentEndings
            `apply` many1 (parseSchemaType "EmploymentEnding")
    schemaTypeToXML s x@EmploymentEndings{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "EmploymentEnding") $ emploEndings_employmentEnding x
            ]
 
data EmploymentPeriods = EmploymentPeriods
        { emploPeriods_period :: [Period]
        }
        deriving (Eq,Show)
instance SchemaType EmploymentPeriods where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return EmploymentPeriods
            `apply` many1 (parseSchemaType "Period")
    schemaTypeToXML s x@EmploymentPeriods{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Period") $ emploPeriods_period x
            ]
 
data EmploymentReg = EmploymentReg
        { emploReg_type :: Xs.Int
        , emploReg_code :: Irct.String20
        }
        deriving (Eq,Show)
instance SchemaType EmploymentReg where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return EmploymentReg
            `apply` parseSchemaType "Type"
            `apply` parseSchemaType "Code"
    schemaTypeToXML s x@EmploymentReg{} =
        toXMLElement s []
            [ schemaTypeToXML "Type" $ emploReg_type x
            , schemaTypeToXML "Code" $ emploReg_code x
            ]
 
data EmploymentRegs = EmploymentRegs
        { emploRegs_employmentReg :: [EmploymentReg]
        }
        deriving (Eq,Show)
instance SchemaType EmploymentRegs where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return EmploymentRegs
            `apply` many1 (parseSchemaType "EmploymentReg")
    schemaTypeToXML s x@EmploymentRegs{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "EmploymentReg") $ emploRegs_employmentReg x
            ]
 
data FinServiceRecipient = FinServiceRecipient
        { finServiceRecip_ids :: FinServiceRecipientIds
        }
        deriving (Eq,Show)
instance SchemaType FinServiceRecipient where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return FinServiceRecipient
            `apply` parseSchemaType "FinServiceRecipientIds"
    schemaTypeToXML s x@FinServiceRecipient{} =
        toXMLElement s []
            [ schemaTypeToXML "FinServiceRecipientIds" $ finServiceRecip_ids x
            ]
 
data FinServiceRecipientIds = FinServiceRecipientIds
        { finServiceRecipIds_id :: [Id]
        }
        deriving (Eq,Show)
instance SchemaType FinServiceRecipientIds where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return FinServiceRecipientIds
            `apply` many1 (parseSchemaType "Id")
    schemaTypeToXML s x@FinServiceRecipientIds{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Id") $ finServiceRecipIds_id x
            ]
 
data ForeignLeasedWork = ForeignLeasedWork
        { foreignLeasedWork_representative :: Maybe Representative
        , foreignLeasedWork_estAmount :: Maybe Irct.Decimal2
        , foreignLeasedWork_finServiceRecipient :: Maybe FinServiceRecipient
        }
        deriving (Eq,Show)
instance SchemaType ForeignLeasedWork where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return ForeignLeasedWork
            `apply` optional (parseSchemaType "Representative")
            `apply` optional (parseSchemaType "EstAmount")
            `apply` optional (parseSchemaType "FinServiceRecipient")
    schemaTypeToXML s x@ForeignLeasedWork{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "Representative") $ foreignLeasedWork_representative x
            , maybe [] (schemaTypeToXML "EstAmount") $ foreignLeasedWork_estAmount x
            , maybe [] (schemaTypeToXML "FinServiceRecipient") $ foreignLeasedWork_finServiceRecipient x
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
 
data IncomeEarner = IncomeEarner
        { incomeEarner_ids :: Maybe IncomeEarnerIds
        , incomeEarner_basic :: Maybe IncomeEarnerBasic
        , incomeEarner_addresses :: Maybe Addresses
        , incomeEarner_subOrgs :: Maybe SubOrgs
        , incomeEarner_employment :: Maybe Employment
        , incomeEarner_professions :: Maybe Professions
        , incomeEarner_employmentRegs :: Maybe EmploymentRegs
        , incomeEarner_placeOfBusiness :: Maybe PlaceOfBusiness
        , incomeEarner_pensionInsurance :: Maybe PensionInsurance
        , incomeEarner_accidentInsurance :: Maybe AccidentInsurance
        , incomeEarner_insuranceExceptions :: Maybe InsuranceExceptions
        , incomeEarner_internationalData :: Maybe InternationalData
        , incomeEarner_other :: Maybe IncomeEarnerOther
        }
        deriving (Eq,Show)
instance SchemaType IncomeEarner where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return IncomeEarner
            `apply` optional (parseSchemaType "IncomeEarnerIds")
            `apply` optional (parseSchemaType "IncomeEarnerBasic")
            `apply` optional (parseSchemaType "Addresses")
            `apply` optional (parseSchemaType "SubOrgs")
            `apply` optional (parseSchemaType "Employment")
            `apply` optional (parseSchemaType "Professions")
            `apply` optional (parseSchemaType "EmploymentRegs")
            `apply` optional (parseSchemaType "PlaceOfBusiness")
            `apply` optional (parseSchemaType "PensionInsurance")
            `apply` optional (parseSchemaType "AccidentInsurance")
            `apply` optional (parseSchemaType "InsuranceExceptions")
            `apply` optional (parseSchemaType "InternationalData")
            `apply` optional (parseSchemaType "IncomeEarnerOther")
    schemaTypeToXML s x@IncomeEarner{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "IncomeEarnerIds") $ incomeEarner_ids x
            , maybe [] (schemaTypeToXML "IncomeEarnerBasic") $ incomeEarner_basic x
            , maybe [] (schemaTypeToXML "Addresses") $ incomeEarner_addresses x
            , maybe [] (schemaTypeToXML "SubOrgs") $ incomeEarner_subOrgs x
            , maybe [] (schemaTypeToXML "Employment") $ incomeEarner_employment x
            , maybe [] (schemaTypeToXML "Professions") $ incomeEarner_professions x
            , maybe [] (schemaTypeToXML "EmploymentRegs") $ incomeEarner_employmentRegs x
            , maybe [] (schemaTypeToXML "PlaceOfBusiness") $ incomeEarner_placeOfBusiness x
            , maybe [] (schemaTypeToXML "PensionInsurance") $ incomeEarner_pensionInsurance x
            , maybe [] (schemaTypeToXML "AccidentInsurance") $ incomeEarner_accidentInsurance x
            , maybe [] (schemaTypeToXML "InsuranceExceptions") $ incomeEarner_insuranceExceptions x
            , maybe [] (schemaTypeToXML "InternationalData") $ incomeEarner_internationalData x
            , maybe [] (schemaTypeToXML "IncomeEarnerOther") $ incomeEarner_other x
            ]
 
data IncomeEarnerBasic = IncomeEarnerBasic
        { incomeEarnerBasic_missingId :: Maybe Irct.True
        , incomeEarnerBasic_companyName :: Maybe Irct.String200
        , incomeEarnerBasic_lastName :: Maybe Irct.String200
        , incomeEarnerBasic_firstName :: Maybe Irct.String100
        , incomeEarnerBasic_birthDate :: Maybe Xs.Date
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
 
data IncomeEarnerOther = IncomeEarnerOther
        { incomeEarnerOther_cBACode :: Maybe Xs.Int
        , incomeEarnerOther_incomeEarnerTypes :: Maybe IncomeEarnerTypes
        , incomeEarnerOther_payments :: Maybe Payments
        }
        deriving (Eq,Show)
instance SchemaType IncomeEarnerOther where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return IncomeEarnerOther
            `apply` optional (parseSchemaType "CBACode")
            `apply` optional (parseSchemaType "IncomeEarnerTypes")
            `apply` optional (parseSchemaType "Payments")
    schemaTypeToXML s x@IncomeEarnerOther{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "CBACode") $ incomeEarnerOther_cBACode x
            , maybe [] (schemaTypeToXML "IncomeEarnerTypes") $ incomeEarnerOther_incomeEarnerTypes x
            , maybe [] (schemaTypeToXML "Payments") $ incomeEarnerOther_payments x
            ]
 
data IncomeEarnerTypes = IncomeEarnerTypes
        { incomeEarnerTypes_incomeEarnerType :: [IncomeEarnerType]
        }
        deriving (Eq,Show)
instance SchemaType IncomeEarnerTypes where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return IncomeEarnerTypes
            `apply` many1 (parseSchemaType "IncomeEarnerType")
    schemaTypeToXML s x@IncomeEarnerTypes{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "IncomeEarnerType") $ incomeEarnerTypes_incomeEarnerType x
            ]
 
data InsuranceData = InsuranceData
        { insurData_transactionInclusion :: [TransactionInclusion]
        }
        deriving (Eq,Show)
instance SchemaType InsuranceData where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return InsuranceData
            `apply` many1 (parseSchemaType "TransactionInclusion")
    schemaTypeToXML s x@InsuranceData{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "TransactionInclusion") $ insurData_transactionInclusion x
            ]
 
data InsuranceExceptions = InsuranceExceptions
        { insurExcept_exceptionCode :: [ExceptionCode]
        }
        deriving (Eq,Show)
instance SchemaType InsuranceExceptions where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return InsuranceExceptions
            `apply` many1 (parseSchemaType "ExceptionCode")
    schemaTypeToXML s x@InsuranceExceptions{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "ExceptionCode") $ insurExcept_exceptionCode x
            ]
 
data InternationalData = InternationalData
        { internData_postedCertCode :: Maybe Xs.Int
        , internData_nonResident :: Maybe Irct.True
        , internData_nonResidentCountryCode :: Maybe Irct.String2
        , internData_nonResidentCountryName :: Maybe Irct.String70
        , internData_subToWithhold :: Maybe Irct.True
        , internData_taxTreatyCountryCode :: Maybe Irct.String2
        , internData_workForm :: Maybe WorkForm
        }
        deriving (Eq,Show)
instance SchemaType InternationalData where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return InternationalData
            `apply` optional (parseSchemaType "PostedCertCode")
            `apply` optional (parseSchemaType "NonResident")
            `apply` optional (parseSchemaType "NonResidentCountryCode")
            `apply` optional (parseSchemaType "NonResidentCountryName")
            `apply` optional (parseSchemaType "SubToWithhold")
            `apply` optional (parseSchemaType "TaxTreatyCountryCode")
            `apply` optional (parseSchemaType "WorkForm")
    schemaTypeToXML s x@InternationalData{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "PostedCertCode") $ internData_postedCertCode x
            , maybe [] (schemaTypeToXML "NonResident") $ internData_nonResident x
            , maybe [] (schemaTypeToXML "NonResidentCountryCode") $ internData_nonResidentCountryCode x
            , maybe [] (schemaTypeToXML "NonResidentCountryName") $ internData_nonResidentCountryName x
            , maybe [] (schemaTypeToXML "SubToWithhold") $ internData_subToWithhold x
            , maybe [] (schemaTypeToXML "TaxTreatyCountryCode") $ internData_taxTreatyCountryCode x
            , maybe [] (schemaTypeToXML "WorkForm") $ internData_workForm x
            ]
 
data KmAllowance = KmAllowance
        { kmAllowance_kilometers :: Xs.Int
        }
        deriving (Eq,Show)
instance SchemaType KmAllowance where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return KmAllowance
            `apply` parseSchemaType "Kilometers"
    schemaTypeToXML s x@KmAllowance{} =
        toXMLElement s []
            [ schemaTypeToXML "Kilometers" $ kmAllowance_kilometers x
            ]
 
data MealBenefit = MealBenefit
        { mealBenefit_taxValue :: Irct.True
        }
        deriving (Eq,Show)
instance SchemaType MealBenefit where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return MealBenefit
            `apply` parseSchemaType "TaxValue"
    schemaTypeToXML s x@MealBenefit{} =
        toXMLElement s []
            [ schemaTypeToXML "TaxValue" $ mealBenefit_taxValue x
            ]
 
data NT1Address = NT1Address
        { nT1Address_co :: Maybe Irct.String70
        , nT1Address_countryCode :: Maybe Irct.String2
        , nT1Address_countryName :: Maybe Irct.String70
        , nT1Address_pOBox :: Maybe Irct.String10
        , nT1Address_postalCode :: Maybe Irct.String20
        , nT1Address_postOffice :: Maybe Irct.String200
        , nT1Address_street :: Maybe Irct.String100
        }
        deriving (Eq,Show)
instance SchemaType NT1Address where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return NT1Address
            `apply` optional (parseSchemaType "Co")
            `apply` optional (parseSchemaType "CountryCode")
            `apply` optional (parseSchemaType "CountryName")
            `apply` optional (parseSchemaType "POBox")
            `apply` optional (parseSchemaType "PostalCode")
            `apply` optional (parseSchemaType "PostOffice")
            `apply` optional (parseSchemaType "Street")
    schemaTypeToXML s x@NT1Address{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "Co") $ nT1Address_co x
            , maybe [] (schemaTypeToXML "CountryCode") $ nT1Address_countryCode x
            , maybe [] (schemaTypeToXML "CountryName") $ nT1Address_countryName x
            , maybe [] (schemaTypeToXML "POBox") $ nT1Address_pOBox x
            , maybe [] (schemaTypeToXML "PostalCode") $ nT1Address_postalCode x
            , maybe [] (schemaTypeToXML "PostOffice") $ nT1Address_postOffice x
            , maybe [] (schemaTypeToXML "Street") $ nT1Address_street x
            ]
 
data OrigPaymentPeriod = OrigPaymentPeriod
        { origPaymentPeriod_paymentDate :: Maybe Xs.Date
        , origPaymentPeriod_startDate :: Xs.Date
        , origPaymentPeriod_endDate :: Xs.Date
        }
        deriving (Eq,Show)
instance SchemaType OrigPaymentPeriod where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return OrigPaymentPeriod
            `apply` optional (parseSchemaType "PaymentDate")
            `apply` parseSchemaType "StartDate"
            `apply` parseSchemaType "EndDate"
    schemaTypeToXML s x@OrigPaymentPeriod{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "PaymentDate") $ origPaymentPeriod_paymentDate x
            , schemaTypeToXML "StartDate" $ origPaymentPeriod_startDate x
            , schemaTypeToXML "EndDate" $ origPaymentPeriod_endDate x
            ]
 
data OrigPaymentPeriods = OrigPaymentPeriods
        { origPaymentPeriods_origPaymentPeriod :: [OrigPaymentPeriod]
        }
        deriving (Eq,Show)
instance SchemaType OrigPaymentPeriods where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return OrigPaymentPeriods
            `apply` many1 (parseSchemaType "OrigPaymentPeriod")
    schemaTypeToXML s x@OrigPaymentPeriods{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "OrigPaymentPeriod") $ origPaymentPeriods_origPaymentPeriod x
            ]
 
data OtherBenefit = OtherBenefit
        { otherBenefit_benefitCode :: [BenefitCode]
        }
        deriving (Eq,Show)
instance SchemaType OtherBenefit where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return OtherBenefit
            `apply` many1 (parseSchemaType "BenefitCode")
    schemaTypeToXML s x@OtherBenefit{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "BenefitCode") $ otherBenefit_benefitCode x
            ]
 
data PaidAbsence = PaidAbsence
        { paidAbsence_periods :: PaidAbsencePeriods
        }
        deriving (Eq,Show)
instance SchemaType PaidAbsence where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PaidAbsence
            `apply` parseSchemaType "PaidAbsencePeriods"
    schemaTypeToXML s x@PaidAbsence{} =
        toXMLElement s []
            [ schemaTypeToXML "PaidAbsencePeriods" $ paidAbsence_periods x
            ]
 
data PaidAbsencePeriod = PaidAbsencePeriod
        { paidAbsencePeriod_startDate :: Xs.Date
        , paidAbsencePeriod_endDate :: Maybe Xs.Date
        , paidAbsencePeriod_absenceDays :: Maybe Xs.Int
        , paidAbsencePeriod_absenceUntil :: Maybe Xs.Date
        , paidAbsencePeriod_causeCode :: Xs.Int
        , paidAbsencePeriod_amount :: Irct.Decimal2
        , paidAbsencePeriod_reimbApp :: Maybe ReimbApp
        , paidAbsencePeriod_info :: Maybe Irct.String850
        }
        deriving (Eq,Show)
instance SchemaType PaidAbsencePeriod where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PaidAbsencePeriod
            `apply` parseSchemaType "StartDate"
            `apply` optional (parseSchemaType "EndDate")
            `apply` optional (parseSchemaType "AbsenceDays")
            `apply` optional (parseSchemaType "AbsenceUntil")
            `apply` parseSchemaType "CauseCode"
            `apply` parseSchemaType "Amount"
            `apply` optional (parseSchemaType "ReimbApp")
            `apply` optional (parseSchemaType "PaidAbsencePeriodInfo")
    schemaTypeToXML s x@PaidAbsencePeriod{} =
        toXMLElement s []
            [ schemaTypeToXML "StartDate" $ paidAbsencePeriod_startDate x
            , maybe [] (schemaTypeToXML "EndDate") $ paidAbsencePeriod_endDate x
            , maybe [] (schemaTypeToXML "AbsenceDays") $ paidAbsencePeriod_absenceDays x
            , maybe [] (schemaTypeToXML "AbsenceUntil") $ paidAbsencePeriod_absenceUntil x
            , schemaTypeToXML "CauseCode" $ paidAbsencePeriod_causeCode x
            , schemaTypeToXML "Amount" $ paidAbsencePeriod_amount x
            , maybe [] (schemaTypeToXML "ReimbApp") $ paidAbsencePeriod_reimbApp x
            , maybe [] (schemaTypeToXML "PaidAbsencePeriodInfo") $ paidAbsencePeriod_info x
            ]
 
data PaidAbsencePeriods = PaidAbsencePeriods
        { paidAbsencePeriods_paidAbsencePeriod :: [PaidAbsencePeriod]
        }
        deriving (Eq,Show)
instance SchemaType PaidAbsencePeriods where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PaidAbsencePeriods
            `apply` many1 (parseSchemaType "PaidAbsencePeriod")
    schemaTypeToXML s x@PaidAbsencePeriods{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "PaidAbsencePeriod") $ paidAbsencePeriods_paidAbsencePeriod x
            ]
 
data Payer = Payer
        { payer_ids :: Maybe PayerIds
        , payer_basic :: Maybe PayerBasic
        , payer_address :: Maybe Address
        , payer_subOrgs :: Maybe SubOrgs
        , payer_other :: Maybe PayerOther
        , payer_substitutePayer :: Maybe SubstitutePayer
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
            `apply` optional (parseSchemaType "PayerOther")
            `apply` optional (parseSchemaType "SubstitutePayer")
    schemaTypeToXML s x@Payer{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "PayerIds") $ payer_ids x
            , maybe [] (schemaTypeToXML "PayerBasic") $ payer_basic x
            , maybe [] (schemaTypeToXML "Address") $ payer_address x
            , maybe [] (schemaTypeToXML "SubOrgs") $ payer_subOrgs x
            , maybe [] (schemaTypeToXML "PayerOther") $ payer_other x
            , maybe [] (schemaTypeToXML "SubstitutePayer") $ payer_substitutePayer x
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
 
data Payment = Payment
        { payment_type :: Xs.Int
        , payment_ref :: Maybe Irct.String50
        , payment_specifier :: Maybe Irct.String200
        }
        deriving (Eq,Show)
instance SchemaType Payment where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Payment
            `apply` parseSchemaType "PaymentType"
            `apply` optional (parseSchemaType "PaymentRef")
            `apply` optional (parseSchemaType "PaymentSpecifier")
    schemaTypeToXML s x@Payment{} =
        toXMLElement s []
            [ schemaTypeToXML "PaymentType" $ payment_type x
            , maybe [] (schemaTypeToXML "PaymentRef") $ payment_ref x
            , maybe [] (schemaTypeToXML "PaymentSpecifier") $ payment_specifier x
            ]
 
data PaymentPeriod = PaymentPeriod
        { paymentPeriod_paymentDate :: Xs.Date
        , paymentPeriod_startDate :: Xs.Date
        , paymentPeriod_endDate :: Xs.Date
        }
        deriving (Eq,Show)
instance SchemaType PaymentPeriod where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PaymentPeriod
            `apply` parseSchemaType "PaymentDate"
            `apply` parseSchemaType "StartDate"
            `apply` parseSchemaType "EndDate"
    schemaTypeToXML s x@PaymentPeriod{} =
        toXMLElement s []
            [ schemaTypeToXML "PaymentDate" $ paymentPeriod_paymentDate x
            , schemaTypeToXML "StartDate" $ paymentPeriod_startDate x
            , schemaTypeToXML "EndDate" $ paymentPeriod_endDate x
            ]
 
data Payments = Payments
        { payments_payment :: [Payment]
        }
        deriving (Eq,Show)
instance SchemaType Payments where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Payments
            `apply` many1 (parseSchemaType "Payment")
    schemaTypeToXML s x@Payments{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Payment") $ payments_payment x
            ]
 
data PaymentTypes = PaymentTypes
        { paymentTypes_paymentType :: [PaymentType]
        }
        deriving (Eq,Show)
instance SchemaType PaymentTypes where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PaymentTypes
            `apply` many1 (parseSchemaType "PaymentType")
    schemaTypeToXML s x@PaymentTypes{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "PaymentType") $ paymentTypes_paymentType x
            ]
 
data PensionInsurance = PensionInsurance
        { pensionInsur_pensionActCode :: Xs.Int
        , pensionInsur_pensionProvIdCode :: Maybe Xs.Int
        , pensionInsur_pensionPolicyNo :: Maybe Irct.PensionPolicyNo
        }
        deriving (Eq,Show)
instance SchemaType PensionInsurance where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PensionInsurance
            `apply` parseSchemaType "PensionActCode"
            `apply` optional (parseSchemaType "PensionProvIdCode")
            `apply` optional (parseSchemaType "PensionPolicyNo")
    schemaTypeToXML s x@PensionInsurance{} =
        toXMLElement s []
            [ schemaTypeToXML "PensionActCode" $ pensionInsur_pensionActCode x
            , maybe [] (schemaTypeToXML "PensionProvIdCode") $ pensionInsur_pensionProvIdCode x
            , maybe [] (schemaTypeToXML "PensionPolicyNo") $ pensionInsur_pensionPolicyNo x
            ]
 
data Period = Period
        { period_startDate :: Xs.Date
        , period_endDate :: Maybe Xs.Date
        }
        deriving (Eq,Show)
instance SchemaType Period where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Period
            `apply` parseSchemaType "StartDate"
            `apply` optional (parseSchemaType "EndDate")
    schemaTypeToXML s x@Period{} =
        toXMLElement s []
            [ schemaTypeToXML "StartDate" $ period_startDate x
            , maybe [] (schemaTypeToXML "EndDate") $ period_endDate x
            ]
 
data PlaceOfBusiness = PlaceOfBusiness
        { placeOfBus_code :: Maybe Irct.String20
        , placeOfBus_street :: Maybe Irct.String100
        , placeOfBus_postalCode :: Maybe Irct.String20
        , placeOfBus_postOffice :: Maybe Irct.String200
        , placeOfBus_countryCode :: Maybe Irct.String2
        , placeOfBus_countryName :: Maybe Irct.String70
        }
        deriving (Eq,Show)
instance SchemaType PlaceOfBusiness where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PlaceOfBusiness
            `apply` optional (parseSchemaType "Code")
            `apply` optional (parseSchemaType "Street")
            `apply` optional (parseSchemaType "PostalCode")
            `apply` optional (parseSchemaType "PostOffice")
            `apply` optional (parseSchemaType "CountryCode")
            `apply` optional (parseSchemaType "CountryName")
    schemaTypeToXML s x@PlaceOfBusiness{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "Code") $ placeOfBus_code x
            , maybe [] (schemaTypeToXML "Street") $ placeOfBus_street x
            , maybe [] (schemaTypeToXML "PostalCode") $ placeOfBus_postalCode x
            , maybe [] (schemaTypeToXML "PostOffice") $ placeOfBus_postOffice x
            , maybe [] (schemaTypeToXML "CountryCode") $ placeOfBus_countryCode x
            , maybe [] (schemaTypeToXML "CountryName") $ placeOfBus_countryName x
            ]
 
data Profession = Profession
        { profession_type :: Xs.Int
        , profession_code :: Irct.String20
        , profession_title :: Maybe Irct.String200
        }
        deriving (Eq,Show)
instance SchemaType Profession where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Profession
            `apply` parseSchemaType "Type"
            `apply` parseSchemaType "Code"
            `apply` optional (parseSchemaType "Title")
    schemaTypeToXML s x@Profession{} =
        toXMLElement s []
            [ schemaTypeToXML "Type" $ profession_type x
            , schemaTypeToXML "Code" $ profession_code x
            , maybe [] (schemaTypeToXML "Title") $ profession_title x
            ]
 
data Professions = Professions
        { professions_profession :: [Profession]
        }
        deriving (Eq,Show)
instance SchemaType Professions where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Professions
            `apply` many1 (parseSchemaType "Profession")
    schemaTypeToXML s x@Professions{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Profession") $ professions_profession x
            ]
 
data RecoveryData = RecoveryData
        { recoveryData_recoveryDate :: Xs.Date
        , recoveryData_withhold :: Maybe Irct.Decimal2
        , recoveryData_taxAtSource :: Maybe Irct.Decimal2
        , recoveryData_origPaymentPeriods :: OrigPaymentPeriods
        }
        deriving (Eq,Show)
instance SchemaType RecoveryData where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return RecoveryData
            `apply` parseSchemaType "RecoveryDate"
            `apply` optional (parseSchemaType "Withhold")
            `apply` optional (parseSchemaType "TaxAtSource")
            `apply` parseSchemaType "OrigPaymentPeriods"
    schemaTypeToXML s x@RecoveryData{} =
        toXMLElement s []
            [ schemaTypeToXML "RecoveryDate" $ recoveryData_recoveryDate x
            , maybe [] (schemaTypeToXML "Withhold") $ recoveryData_withhold x
            , maybe [] (schemaTypeToXML "TaxAtSource") $ recoveryData_taxAtSource x
            , schemaTypeToXML "OrigPaymentPeriods" $ recoveryData_origPaymentPeriods x
            ]
 
data ReimbApp = ReimbApp
        { reimbApp_reimbPayment :: [ReimbPayment]
        }
        deriving (Eq,Show)
instance SchemaType ReimbApp where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return ReimbApp
            `apply` many1 (parseSchemaType "ReimbPayment")
    schemaTypeToXML s x@ReimbApp{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "ReimbPayment") $ reimbApp_reimbPayment x
            ]
 
data ReimbPayment = ReimbPayment
        { reimbPayment_paymentType :: Xs.Int
        , reimbPayment_paymentRef :: Maybe Irct.String50
        , reimbPayment_paymentSpecifier :: Maybe Irct.String200
        }
        deriving (Eq,Show)
instance SchemaType ReimbPayment where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return ReimbPayment
            `apply` parseSchemaType "PaymentType"
            `apply` optional (parseSchemaType "PaymentRef")
            `apply` optional (parseSchemaType "PaymentSpecifier")
    schemaTypeToXML s x@ReimbPayment{} =
        toXMLElement s []
            [ schemaTypeToXML "PaymentType" $ reimbPayment_paymentType x
            , maybe [] (schemaTypeToXML "PaymentRef") $ reimbPayment_paymentRef x
            , maybe [] (schemaTypeToXML "PaymentSpecifier") $ reimbPayment_paymentSpecifier x
            ]
 
data Remunerations = Remunerations
        { remun_remunerationCode :: [RemunerationCode]
        }
        deriving (Eq,Show)
instance SchemaType Remunerations where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Remunerations
            `apply` many1 (parseSchemaType "RemunerationCode")
    schemaTypeToXML s x@Remunerations{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "RemunerationCode") $ remun_remunerationCode x
            ]
 
data Report = Report
        { report_data :: ReportData
        , report_incomeEarner :: IncomeEarner
        , report_transactions :: Maybe Transactions
        , report_foreignLeasedWork :: Maybe ForeignLeasedWork
        , report_stayPeriodsInFinland :: Maybe StayPeriodsInFinland
        , report_workPeriodsInFinland :: Maybe WorkPeriodsInFinland
        , report_workCountries :: Maybe WorkCountries
        , report_absence :: Maybe Absence
        }
        deriving (Eq,Show)
instance SchemaType Report where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Report
            `apply` parseSchemaType "ReportData"
            `apply` parseSchemaType "IncomeEarner"
            `apply` optional (parseSchemaType "Transactions")
            `apply` optional (parseSchemaType "ForeignLeasedWork")
            `apply` optional (parseSchemaType "StayPeriodsInFinland")
            `apply` optional (parseSchemaType "WorkPeriodsInFinland")
            `apply` optional (parseSchemaType "WorkCountries")
            `apply` optional (parseSchemaType "Absence")
    schemaTypeToXML s x@Report{} =
        toXMLElement s []
            [ schemaTypeToXML "ReportData" $ report_data x
            , schemaTypeToXML "IncomeEarner" $ report_incomeEarner x
            , maybe [] (schemaTypeToXML "Transactions") $ report_transactions x
            , maybe [] (schemaTypeToXML "ForeignLeasedWork") $ report_foreignLeasedWork x
            , maybe [] (schemaTypeToXML "StayPeriodsInFinland") $ report_stayPeriodsInFinland x
            , maybe [] (schemaTypeToXML "WorkPeriodsInFinland") $ report_workPeriodsInFinland x
            , maybe [] (schemaTypeToXML "WorkCountries") $ report_workCountries x
            , maybe [] (schemaTypeToXML "Absence") $ report_absence x
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
 
data Representative = Representative
        { repres_ids :: RepresentativeIds
        , repres_name :: Irct.String200
        , repres_address :: Maybe Address
        , repres_reports :: Maybe Irct.True
        }
        deriving (Eq,Show)
instance SchemaType Representative where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Representative
            `apply` parseSchemaType "RepresentativeIds"
            `apply` parseSchemaType "Name"
            `apply` optional (parseSchemaType "Address")
            `apply` optional (parseSchemaType "RepresentativeReports")
    schemaTypeToXML s x@Representative{} =
        toXMLElement s []
            [ schemaTypeToXML "RepresentativeIds" $ repres_ids x
            , schemaTypeToXML "Name" $ repres_name x
            , maybe [] (schemaTypeToXML "Address") $ repres_address x
            , maybe [] (schemaTypeToXML "RepresentativeReports") $ repres_reports x
            ]
 
data RepresentativeIds = RepresentativeIds
        { represIds_id :: [Id]
        }
        deriving (Eq,Show)
instance SchemaType RepresentativeIds where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return RepresentativeIds
            `apply` many1 (parseSchemaType "Id")
    schemaTypeToXML s x@RepresentativeIds{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Id") $ represIds_id x
            ]
 
data SailorIncome = SailorIncome
        { sailorIncome :: Irct.True
        , sailorIncome_crossTradeTime :: Maybe Xs.Int
        , sailorIncome_withdrawalPeriod :: Maybe WithdrawalPeriod
        }
        deriving (Eq,Show)
instance SchemaType SailorIncome where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return SailorIncome
            `apply` parseSchemaType "SailorIncome"
            `apply` optional (parseSchemaType "CrossTradeTime")
            `apply` optional (parseSchemaType "WithdrawalPeriod")
    schemaTypeToXML s x@SailorIncome{} =
        toXMLElement s []
            [ schemaTypeToXML "SailorIncome" $ sailorIncome x
            , maybe [] (schemaTypeToXML "CrossTradeTime") $ sailorIncome_crossTradeTime x
            , maybe [] (schemaTypeToXML "WithdrawalPeriod") $ sailorIncome_withdrawalPeriod x
            ]
 
data ServiceRecipient = ServiceRecipient
        { serviceRecip_ids :: ServiceRecipientIds
        , serviceRecip_name :: Irct.String200
        , serviceRecip_address :: Maybe ServiceRecipientAddress
        }
        deriving (Eq,Show)
instance SchemaType ServiceRecipient where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return ServiceRecipient
            `apply` parseSchemaType "ServiceRecipientIds"
            `apply` parseSchemaType "Name"
            `apply` optional (parseSchemaType "ServiceRecipientAddress")
    schemaTypeToXML s x@ServiceRecipient{} =
        toXMLElement s []
            [ schemaTypeToXML "ServiceRecipientIds" $ serviceRecip_ids x
            , schemaTypeToXML "Name" $ serviceRecip_name x
            , maybe [] (schemaTypeToXML "ServiceRecipientAddress") $ serviceRecip_address x
            ]
 
data ServiceRecipientAddress = ServiceRecipientAddress
        { serviceRecipAddress_co :: Maybe Irct.String70
        , serviceRecipAddress_street :: Maybe Irct.String100
        , serviceRecipAddress_pOBox :: Maybe Irct.String10
        , serviceRecipAddress_postalCode :: Irct.String20
        , serviceRecipAddress_postOffice :: Irct.String200
        , serviceRecipAddress_countryCode :: Irct.String2
        , serviceRecipAddress_countryName :: Maybe Irct.String70
        }
        deriving (Eq,Show)
instance SchemaType ServiceRecipientAddress where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return ServiceRecipientAddress
            `apply` optional (parseSchemaType "Co")
            `apply` optional (parseSchemaType "Street")
            `apply` optional (parseSchemaType "POBox")
            `apply` parseSchemaType "PostalCode"
            `apply` parseSchemaType "PostOffice"
            `apply` parseSchemaType "CountryCode"
            `apply` optional (parseSchemaType "CountryName")
    schemaTypeToXML s x@ServiceRecipientAddress{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "Co") $ serviceRecipAddress_co x
            , maybe [] (schemaTypeToXML "Street") $ serviceRecipAddress_street x
            , maybe [] (schemaTypeToXML "POBox") $ serviceRecipAddress_pOBox x
            , schemaTypeToXML "PostalCode" $ serviceRecipAddress_postalCode x
            , schemaTypeToXML "PostOffice" $ serviceRecipAddress_postOffice x
            , schemaTypeToXML "CountryCode" $ serviceRecipAddress_countryCode x
            , maybe [] (schemaTypeToXML "CountryName") $ serviceRecipAddress_countryName x
            ]
 
data ServiceRecipientIds = ServiceRecipientIds
        { serviceRecipIds_id :: [Id]
        }
        deriving (Eq,Show)
instance SchemaType ServiceRecipientIds where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return ServiceRecipientIds
            `apply` many1 (parseSchemaType "Id")
    schemaTypeToXML s x@ServiceRecipientIds{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Id") $ serviceRecipIds_id x
            ]
 
data SixMonthRule = SixMonthRule
        { sixMonthRule_applicable :: Irct.TrueOrFalse
        , sixMonthRule_countryCode :: Maybe Irct.String2
        , sixMonthRule_countryName :: Maybe Irct.String70
        }
        deriving (Eq,Show)
instance SchemaType SixMonthRule where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return SixMonthRule
            `apply` parseSchemaType "Applicable"
            `apply` optional (parseSchemaType "CountryCode")
            `apply` optional (parseSchemaType "CountryName")
    schemaTypeToXML s x@SixMonthRule{} =
        toXMLElement s []
            [ schemaTypeToXML "Applicable" $ sixMonthRule_applicable x
            , maybe [] (schemaTypeToXML "CountryCode") $ sixMonthRule_countryCode x
            , maybe [] (schemaTypeToXML "CountryName") $ sixMonthRule_countryName x
            ]
 
data StayPeriod = StayPeriod
        { stayPeriod_startDate :: Xs.Date
        , stayPeriod_endDate :: Xs.Date
        }
        deriving (Eq,Show)
instance SchemaType StayPeriod where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return StayPeriod
            `apply` parseSchemaType "StartDate"
            `apply` parseSchemaType "EndDate"
    schemaTypeToXML s x@StayPeriod{} =
        toXMLElement s []
            [ schemaTypeToXML "StartDate" $ stayPeriod_startDate x
            , schemaTypeToXML "EndDate" $ stayPeriod_endDate x
            ]
 
data StayPeriodsAbroad = StayPeriodsAbroad
        { stayPeriodsAbroad_period :: [StayPeriod]
        }
        deriving (Eq,Show)
instance SchemaType StayPeriodsAbroad where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return StayPeriodsAbroad
            `apply` many1 (parseSchemaType "Period")
    schemaTypeToXML s x@StayPeriodsAbroad{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Period") $ stayPeriodsAbroad_period x
            ]
 
data StayPeriodsInFinland = StayPeriodsInFinland
        { stayPeriodsInFinland_noStayPeriods :: Maybe Irct.True
        , stayPeriodsInFinland_period :: [StayPeriod]
        }
        deriving (Eq,Show)
instance SchemaType StayPeriodsInFinland where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return StayPeriodsInFinland
            `apply` optional (parseSchemaType "NoStayPeriods")
            `apply` many (parseSchemaType "Period")
    schemaTypeToXML s x@StayPeriodsInFinland{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "NoStayPeriods") $ stayPeriodsInFinland_noStayPeriods x
            , concatMap (schemaTypeToXML "Period") $ stayPeriodsInFinland_period x
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
 
data SubstitutePayer = SubstitutePayer
        { substPayer_acts :: Irct.True
        , substPayer_employerId :: Id
        , substPayer_employerName :: Maybe Irct.String200
        , substPayer_wageSec :: Maybe Irct.True
        }
        deriving (Eq,Show)
instance SchemaType SubstitutePayer where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return SubstitutePayer
            `apply` parseSchemaType "Acts"
            `apply` parseSchemaType "EmployerId"
            `apply` optional (parseSchemaType "EmployerName")
            `apply` optional (parseSchemaType "WageSec")
    schemaTypeToXML s x@SubstitutePayer{} =
        toXMLElement s []
            [ schemaTypeToXML "Acts" $ substPayer_acts x
            , schemaTypeToXML "EmployerId" $ substPayer_employerId x
            , maybe [] (schemaTypeToXML "EmployerName") $ substPayer_employerName x
            , maybe [] (schemaTypeToXML "WageSec") $ substPayer_wageSec x
            ]
 
data Transaction = Transaction
        { transaction_basic :: TransactionBasic
        , transaction_insuranceData :: Maybe InsuranceData
        , transaction_earningPeriods :: Maybe EarningPeriods
        , transaction_unitWages :: Maybe UnitWages
        , transaction_carBenefit :: Maybe CarBenefit
        , transaction_mealBenefit :: Maybe MealBenefit
        , transaction_otherBenefit :: Maybe OtherBenefit
        , transaction_sailorIncome :: Maybe SailorIncome
        , transaction_recoveryData :: Maybe RecoveryData
        , transaction_dailyAllowance :: Maybe DailyAllowance
        , transaction_kmAllowance :: Maybe KmAllowance
        , transaction_sixMonthRule :: Maybe SixMonthRule
        }
        deriving (Eq,Show)
instance SchemaType Transaction where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Transaction
            `apply` parseSchemaType "TransactionBasic"
            `apply` optional (parseSchemaType "InsuranceData")
            `apply` optional (parseSchemaType "EarningPeriods")
            `apply` optional (parseSchemaType "UnitWages")
            `apply` optional (parseSchemaType "CarBenefit")
            `apply` optional (parseSchemaType "MealBenefit")
            `apply` optional (parseSchemaType "OtherBenefit")
            `apply` optional (parseSchemaType "SailorIncome")
            `apply` optional (parseSchemaType "RecoveryData")
            `apply` optional (parseSchemaType "DailyAllowance")
            `apply` optional (parseSchemaType "KmAllowance")
            `apply` optional (parseSchemaType "SixMonthRule")
    schemaTypeToXML s x@Transaction{} =
        toXMLElement s []
            [ schemaTypeToXML "TransactionBasic" $ transaction_basic x
            , maybe [] (schemaTypeToXML "InsuranceData") $ transaction_insuranceData x
            , maybe [] (schemaTypeToXML "EarningPeriods") $ transaction_earningPeriods x
            , maybe [] (schemaTypeToXML "UnitWages") $ transaction_unitWages x
            , maybe [] (schemaTypeToXML "CarBenefit") $ transaction_carBenefit x
            , maybe [] (schemaTypeToXML "MealBenefit") $ transaction_mealBenefit x
            , maybe [] (schemaTypeToXML "OtherBenefit") $ transaction_otherBenefit x
            , maybe [] (schemaTypeToXML "SailorIncome") $ transaction_sailorIncome x
            , maybe [] (schemaTypeToXML "RecoveryData") $ transaction_recoveryData x
            , maybe [] (schemaTypeToXML "DailyAllowance") $ transaction_dailyAllowance x
            , maybe [] (schemaTypeToXML "KmAllowance") $ transaction_kmAllowance x
            , maybe [] (schemaTypeToXML "SixMonthRule") $ transaction_sixMonthRule x
            ]
 
data TransactionBasic = TransactionBasic
        { transBasic_transactionCode :: Xs.Int
        , transBasic_amount :: Maybe Irct.Decimal2
        , transBasic_noMoney :: Maybe Irct.True
        , transBasic_oneOff :: Maybe Irct.True
        , transBasic_unjustEnrichment :: Maybe Irct.True
        , transBasic_recovery :: Maybe Irct.True
        }
        deriving (Eq,Show)
instance SchemaType TransactionBasic where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return TransactionBasic
            `apply` parseSchemaType "TransactionCode"
            `apply` optional (parseSchemaType "Amount")
            `apply` optional (parseSchemaType "NoMoney")
            `apply` optional (parseSchemaType "OneOff")
            `apply` optional (parseSchemaType "UnjustEnrichment")
            `apply` optional (parseSchemaType "Recovery")
    schemaTypeToXML s x@TransactionBasic{} =
        toXMLElement s []
            [ schemaTypeToXML "TransactionCode" $ transBasic_transactionCode x
            , maybe [] (schemaTypeToXML "Amount") $ transBasic_amount x
            , maybe [] (schemaTypeToXML "NoMoney") $ transBasic_noMoney x
            , maybe [] (schemaTypeToXML "OneOff") $ transBasic_oneOff x
            , maybe [] (schemaTypeToXML "UnjustEnrichment") $ transBasic_unjustEnrichment x
            , maybe [] (schemaTypeToXML "Recovery") $ transBasic_recovery x
            ]
 
data TransactionInclusion = TransactionInclusion
        { transInclus_insuranceCode :: Xs.Int
        , transInclus_included :: Irct.TrueOrFalse
        }
        deriving (Eq,Show)
instance SchemaType TransactionInclusion where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return TransactionInclusion
            `apply` parseSchemaType "InsuranceCode"
            `apply` parseSchemaType "Included"
    schemaTypeToXML s x@TransactionInclusion{} =
        toXMLElement s []
            [ schemaTypeToXML "InsuranceCode" $ transInclus_insuranceCode x
            , schemaTypeToXML "Included" $ transInclus_included x
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
 
data TypedAddress = TypedAddress
        { typedAddress_addressType :: Xs.Int
        , typedAddress_co :: Maybe Irct.String70
        , typedAddress_street :: Maybe Irct.String100
        , typedAddress_pOBox :: Maybe Irct.String10
        , typedAddress_postalCode :: Irct.String20
        , typedAddress_postOffice :: Irct.String200
        , typedAddress_countryCode :: Maybe Irct.String2
        , typedAddress_countryName :: Maybe Irct.String70
        }
        deriving (Eq,Show)
instance SchemaType TypedAddress where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return TypedAddress
            `apply` parseSchemaType "AddressType"
            `apply` optional (parseSchemaType "Co")
            `apply` optional (parseSchemaType "Street")
            `apply` optional (parseSchemaType "POBox")
            `apply` parseSchemaType "PostalCode"
            `apply` parseSchemaType "PostOffice"
            `apply` optional (parseSchemaType "CountryCode")
            `apply` optional (parseSchemaType "CountryName")
    schemaTypeToXML s x@TypedAddress{} =
        toXMLElement s []
            [ schemaTypeToXML "AddressType" $ typedAddress_addressType x
            , maybe [] (schemaTypeToXML "Co") $ typedAddress_co x
            , maybe [] (schemaTypeToXML "Street") $ typedAddress_street x
            , maybe [] (schemaTypeToXML "POBox") $ typedAddress_pOBox x
            , schemaTypeToXML "PostalCode" $ typedAddress_postalCode x
            , schemaTypeToXML "PostOffice" $ typedAddress_postOffice x
            , maybe [] (schemaTypeToXML "CountryCode") $ typedAddress_countryCode x
            , maybe [] (schemaTypeToXML "CountryName") $ typedAddress_countryName x
            ]
 
data UnitWage = UnitWage
        { unitWage_unitPrice :: Irct.Decimal2
        , unitWage_unitAmount :: Irct.Decimal2
        , unitWage_unitCode :: Xs.Int
        }
        deriving (Eq,Show)
instance SchemaType UnitWage where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return UnitWage
            `apply` parseSchemaType "UnitPrice"
            `apply` parseSchemaType "UnitAmount"
            `apply` parseSchemaType "UnitCode"
    schemaTypeToXML s x@UnitWage{} =
        toXMLElement s []
            [ schemaTypeToXML "UnitPrice" $ unitWage_unitPrice x
            , schemaTypeToXML "UnitAmount" $ unitWage_unitAmount x
            , schemaTypeToXML "UnitCode" $ unitWage_unitCode x
            ]
 
data UnitWages = UnitWages
        { unitWages_unitWage :: [UnitWage]
        }
        deriving (Eq,Show)
instance SchemaType UnitWages where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return UnitWages
            `apply` many1 (parseSchemaType "UnitWage")
    schemaTypeToXML s x@UnitWages{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "UnitWage") $ unitWages_unitWage x
            ]
 
data UnpaidAbsence = UnpaidAbsence
        { unpaidAbsence_periods :: UnpaidAbsencePeriods
        }
        deriving (Eq,Show)
instance SchemaType UnpaidAbsence where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return UnpaidAbsence
            `apply` parseSchemaType "UnpaidAbsencePeriods"
    schemaTypeToXML s x@UnpaidAbsence{} =
        toXMLElement s []
            [ schemaTypeToXML "UnpaidAbsencePeriods" $ unpaidAbsence_periods x
            ]
 
data UnpaidAbsencePeriod = UnpaidAbsencePeriod
        { unpaidAbsencePeriod_startDate :: Xs.Date
        , unpaidAbsencePeriod_endDate :: Maybe Xs.Date
        , unpaidAbsencePeriod_absenceDays :: Maybe Xs.Int
        , unpaidAbsencePeriod_causeCode :: Xs.Int
        }
        deriving (Eq,Show)
instance SchemaType UnpaidAbsencePeriod where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return UnpaidAbsencePeriod
            `apply` parseSchemaType "StartDate"
            `apply` optional (parseSchemaType "EndDate")
            `apply` optional (parseSchemaType "AbsenceDays")
            `apply` parseSchemaType "CauseCode"
    schemaTypeToXML s x@UnpaidAbsencePeriod{} =
        toXMLElement s []
            [ schemaTypeToXML "StartDate" $ unpaidAbsencePeriod_startDate x
            , maybe [] (schemaTypeToXML "EndDate") $ unpaidAbsencePeriod_endDate x
            , maybe [] (schemaTypeToXML "AbsenceDays") $ unpaidAbsencePeriod_absenceDays x
            , schemaTypeToXML "CauseCode" $ unpaidAbsencePeriod_causeCode x
            ]
 
data UnpaidAbsencePeriods = UnpaidAbsencePeriods
        { unpaidAbsencePeriods_unpaidAbsencePeriod :: [UnpaidAbsencePeriod]
        }
        deriving (Eq,Show)
instance SchemaType UnpaidAbsencePeriods where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return UnpaidAbsencePeriods
            `apply` many1 (parseSchemaType "UnpaidAbsencePeriod")
    schemaTypeToXML s x@UnpaidAbsencePeriods{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "UnpaidAbsencePeriod") $ unpaidAbsencePeriods_unpaidAbsencePeriod x
            ]
 
data WageReportsToIR = WageReportsToIR
        { wageReportsToIR_deliveryData :: DeliveryData
        , wageReportsToIR_signature :: Maybe SignatureType
        }
        deriving (Eq,Show)
instance SchemaType WageReportsToIR where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return WageReportsToIR
            `apply` parseSchemaType "DeliveryData"
            `apply` optional (elementSignature)
    schemaTypeToXML s x@WageReportsToIR{} =
        toXMLElement s []
            [ schemaTypeToXML "DeliveryData" $ wageReportsToIR_deliveryData x
            , maybe [] (elementToXMLSignature) $ wageReportsToIR_signature x
            ]
 
data WithdrawalPeriod = WithdrawalPeriod
        { withdrPeriod_startDate :: Xs.Date
        , withdrPeriod_endDate :: Xs.Date
        }
        deriving (Eq,Show)
instance SchemaType WithdrawalPeriod where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return WithdrawalPeriod
            `apply` parseSchemaType "StartDate"
            `apply` parseSchemaType "EndDate"
    schemaTypeToXML s x@WithdrawalPeriod{} =
        toXMLElement s []
            [ schemaTypeToXML "StartDate" $ withdrPeriod_startDate x
            , schemaTypeToXML "EndDate" $ withdrPeriod_endDate x
            ]
 
data WorkCountries = WorkCountries
        { workCountr_workCountry :: [WorkCountry]
        }
        deriving (Eq,Show)
instance SchemaType WorkCountries where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return WorkCountries
            `apply` many1 (parseSchemaType "WorkCountry")
    schemaTypeToXML s x@WorkCountries{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "WorkCountry") $ workCountr_workCountry x
            ]
 
data WorkCountry = WorkCountry
        { workCountry_code :: Irct.String2
        , workCountry_name :: Maybe Irct.String70
        , workCountry_workMunicipalities :: Maybe WorkMunicipalities
        , workCountry_address :: Maybe WorkCountryAddress
        , workCountry_serviceRecipient :: Maybe ServiceRecipient
        , workCountry_stayPeriodsAbroad :: Maybe StayPeriodsAbroad
        , workCountry_workPeriodsAbroad :: Maybe WorkPeriodsAbroad
        , workCountry_taxingRight :: Maybe Irct.TrueOrFalse
        , workCountry_burdensResultOfPE :: Maybe Irct.TrueOrFalse
        , workCountry_wagePerMonth :: Maybe Irct.Decimal2
        , workCountry_remunerations :: Maybe Remunerations
        }
        deriving (Eq,Show)
instance SchemaType WorkCountry where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return WorkCountry
            `apply` parseSchemaType "WorkCountryCode"
            `apply` optional (parseSchemaType "WorkCountryName")
            `apply` optional (parseSchemaType "WorkMunicipalities")
            `apply` optional (parseSchemaType "WorkCountryAddress")
            `apply` optional (parseSchemaType "ServiceRecipient")
            `apply` optional (parseSchemaType "StayPeriodsAbroad")
            `apply` optional (parseSchemaType "WorkPeriodsAbroad")
            `apply` optional (parseSchemaType "TaxingRight")
            `apply` optional (parseSchemaType "BurdensResultOfPE")
            `apply` optional (parseSchemaType "WagePerMonth")
            `apply` optional (parseSchemaType "Remunerations")
    schemaTypeToXML s x@WorkCountry{} =
        toXMLElement s []
            [ schemaTypeToXML "WorkCountryCode" $ workCountry_code x
            , maybe [] (schemaTypeToXML "WorkCountryName") $ workCountry_name x
            , maybe [] (schemaTypeToXML "WorkMunicipalities") $ workCountry_workMunicipalities x
            , maybe [] (schemaTypeToXML "WorkCountryAddress") $ workCountry_address x
            , maybe [] (schemaTypeToXML "ServiceRecipient") $ workCountry_serviceRecipient x
            , maybe [] (schemaTypeToXML "StayPeriodsAbroad") $ workCountry_stayPeriodsAbroad x
            , maybe [] (schemaTypeToXML "WorkPeriodsAbroad") $ workCountry_workPeriodsAbroad x
            , maybe [] (schemaTypeToXML "TaxingRight") $ workCountry_taxingRight x
            , maybe [] (schemaTypeToXML "BurdensResultOfPE") $ workCountry_burdensResultOfPE x
            , maybe [] (schemaTypeToXML "WagePerMonth") $ workCountry_wagePerMonth x
            , maybe [] (schemaTypeToXML "Remunerations") $ workCountry_remunerations x
            ]
 
data WorkCountryAddress = WorkCountryAddress
        { workCountryAddress_co :: Maybe Irct.String70
        , workCountryAddress_street :: Maybe Irct.String100
        , workCountryAddress_pOBox :: Maybe Irct.String10
        , workCountryAddress_postalCode :: Irct.String20
        , workCountryAddress_postOffice :: Irct.String200
        , workCountryAddress_countryCode :: Irct.String2
        , workCountryAddress_countryName :: Maybe Irct.String70
        }
        deriving (Eq,Show)
instance SchemaType WorkCountryAddress where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return WorkCountryAddress
            `apply` optional (parseSchemaType "Co")
            `apply` optional (parseSchemaType "Street")
            `apply` optional (parseSchemaType "POBox")
            `apply` parseSchemaType "PostalCode"
            `apply` parseSchemaType "PostOffice"
            `apply` parseSchemaType "CountryCode"
            `apply` optional (parseSchemaType "CountryName")
    schemaTypeToXML s x@WorkCountryAddress{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "Co") $ workCountryAddress_co x
            , maybe [] (schemaTypeToXML "Street") $ workCountryAddress_street x
            , maybe [] (schemaTypeToXML "POBox") $ workCountryAddress_pOBox x
            , schemaTypeToXML "PostalCode" $ workCountryAddress_postalCode x
            , schemaTypeToXML "PostOffice" $ workCountryAddress_postOffice x
            , schemaTypeToXML "CountryCode" $ workCountryAddress_countryCode x
            , maybe [] (schemaTypeToXML "CountryName") $ workCountryAddress_countryName x
            ]
 
data WorkForm = WorkForm
        { workForm_formCode :: [FormCode]
        }
        deriving (Eq,Show)
instance SchemaType WorkForm where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return WorkForm
            `apply` many1 (parseSchemaType "FormCode")
    schemaTypeToXML s x@WorkForm{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "FormCode") $ workForm_formCode x
            ]
 
data WorkMunicipalities = WorkMunicipalities
        { workMunic_workMunicipality :: [WorkMunicipality]
        }
        deriving (Eq,Show)
instance SchemaType WorkMunicipalities where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return WorkMunicipalities
            `apply` many1 (parseSchemaType "WorkMunicipality")
    schemaTypeToXML s x@WorkMunicipalities{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "WorkMunicipality") $ workMunic_workMunicipality x
            ]
 
data WorkPeriodInFinland = WorkPeriodInFinland
        { workPeriodInFinland_startDate :: Maybe Xs.Date
        , workPeriodInFinland_endDate :: Maybe Xs.Date
        , workPeriodInFinland_workDays :: Maybe Xs.Int
        }
        deriving (Eq,Show)
instance SchemaType WorkPeriodInFinland where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return WorkPeriodInFinland
            `apply` optional (parseSchemaType "StartDate")
            `apply` optional (parseSchemaType "EndDate")
            `apply` optional (parseSchemaType "WorkDays")
    schemaTypeToXML s x@WorkPeriodInFinland{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "StartDate") $ workPeriodInFinland_startDate x
            , maybe [] (schemaTypeToXML "EndDate") $ workPeriodInFinland_endDate x
            , maybe [] (schemaTypeToXML "WorkDays") $ workPeriodInFinland_workDays x
            ]
 
data WorkPeriodsAbroad = WorkPeriodsAbroad
        { workPeriodsAbroad_period :: [Period]
        }
        deriving (Eq,Show)
instance SchemaType WorkPeriodsAbroad where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return WorkPeriodsAbroad
            `apply` many1 (parseSchemaType "Period")
    schemaTypeToXML s x@WorkPeriodsAbroad{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Period") $ workPeriodsAbroad_period x
            ]
 
data WorkPeriodsInFinland = WorkPeriodsInFinland
        { workPeriodsInFinland_workPeriodInFinland :: [WorkPeriodInFinland]
        }
        deriving (Eq,Show)
instance SchemaType WorkPeriodsInFinland where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return WorkPeriodsInFinland
            `apply` many1 (parseSchemaType "WorkPeriodInFinland")
    schemaTypeToXML s x@WorkPeriodsInFinland{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "WorkPeriodInFinland") $ workPeriodsInFinland_workPeriodInFinland x
            ]
