{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.WageReportsToIRTypes
  ( module Data.WageReportsToIRTypes
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.IRCommonTypes as Irct
 
newtype AllowanceCode = AllowanceCode Xs.Int
instance Eq AllowanceCode
instance Show AllowanceCode
instance Restricts AllowanceCode Xs.Int
instance SchemaType AllowanceCode
instance SimpleType AllowanceCode
 
newtype BenefitCode = BenefitCode Xs.Int
instance Eq BenefitCode
instance Show BenefitCode
instance Restricts BenefitCode Xs.Int
instance SchemaType BenefitCode
instance SimpleType BenefitCode
 
newtype ExceptionCode = ExceptionCode Xs.Int
instance Eq ExceptionCode
instance Show ExceptionCode
instance Restricts ExceptionCode Xs.Int
instance SchemaType ExceptionCode
instance SimpleType ExceptionCode
 
newtype FormCode = FormCode Xs.Int
instance Eq FormCode
instance Show FormCode
instance Restricts FormCode Xs.Int
instance SchemaType FormCode
instance SimpleType FormCode
 
newtype IncomeEarnerType = IncomeEarnerType Xs.Int
instance Eq IncomeEarnerType
instance Show IncomeEarnerType
instance Restricts IncomeEarnerType Xs.Int
instance SchemaType IncomeEarnerType
instance SimpleType IncomeEarnerType
 
newtype PaymentType = PaymentType Xs.Int
instance Eq PaymentType
instance Show PaymentType
instance Restricts PaymentType Xs.Int
instance SchemaType PaymentType
instance SimpleType PaymentType
 
newtype RemunerationCode = RemunerationCode Xs.Int
instance Eq RemunerationCode
instance Show RemunerationCode
instance Restricts RemunerationCode Xs.Int
instance SchemaType RemunerationCode
instance SimpleType RemunerationCode
 
newtype WorkMunicipality = WorkMunicipality Irct.String200
instance Eq WorkMunicipality
instance Show WorkMunicipality
instance Restricts WorkMunicipality Irct.String200
instance SchemaType WorkMunicipality
instance SimpleType WorkMunicipality
 
data Absence
instance Eq Absence
instance Show Absence
instance SchemaType Absence
 
data AccidentInsurance
instance Eq AccidentInsurance
instance Show AccidentInsurance
instance SchemaType AccidentInsurance
 
data Address
instance Eq Address
instance Show Address
instance SchemaType Address
 
data Addresses
instance Eq Addresses
instance Show Addresses
instance SchemaType Addresses
 
data CarBenefit
instance Eq CarBenefit
instance Show CarBenefit
instance SchemaType CarBenefit
 
data ContactPerson
instance Eq ContactPerson
instance Show ContactPerson
instance SchemaType ContactPerson
 
data ContactPersons
instance Eq ContactPersons
instance Show ContactPersons
instance SchemaType ContactPersons
 
data DailyAllowance
instance Eq DailyAllowance
instance Show DailyAllowance
instance SchemaType DailyAllowance
 
data DeliveryData
instance Eq DeliveryData
instance Show DeliveryData
instance SchemaType DeliveryData
 
data EarningPeriod
instance Eq EarningPeriod
instance Show EarningPeriod
instance SchemaType EarningPeriod
 
data EarningPeriods
instance Eq EarningPeriods
instance Show EarningPeriods
instance SchemaType EarningPeriods
 
data EmployeeMax183d
instance Eq EmployeeMax183d
instance Show EmployeeMax183d
instance SchemaType EmployeeMax183d
 
data Employment
instance Eq Employment
instance Show Employment
instance SchemaType Employment
 
data EmploymentEnding
instance Eq EmploymentEnding
instance Show EmploymentEnding
instance SchemaType EmploymentEnding
 
data EmploymentEndings
instance Eq EmploymentEndings
instance Show EmploymentEndings
instance SchemaType EmploymentEndings
 
data EmploymentPeriods
instance Eq EmploymentPeriods
instance Show EmploymentPeriods
instance SchemaType EmploymentPeriods
 
data EmploymentReg
instance Eq EmploymentReg
instance Show EmploymentReg
instance SchemaType EmploymentReg
 
data EmploymentRegs
instance Eq EmploymentRegs
instance Show EmploymentRegs
instance SchemaType EmploymentRegs
 
data FinServiceRecipient
instance Eq FinServiceRecipient
instance Show FinServiceRecipient
instance SchemaType FinServiceRecipient
 
data FinServiceRecipientIds
instance Eq FinServiceRecipientIds
instance Show FinServiceRecipientIds
instance SchemaType FinServiceRecipientIds
 
data ForeignLeasedWork
instance Eq ForeignLeasedWork
instance Show ForeignLeasedWork
instance SchemaType ForeignLeasedWork
 
data Id
instance Eq Id
instance Show Id
instance SchemaType Id
 
data IncomeEarner
instance Eq IncomeEarner
instance Show IncomeEarner
instance SchemaType IncomeEarner
 
data IncomeEarnerBasic
instance Eq IncomeEarnerBasic
instance Show IncomeEarnerBasic
instance SchemaType IncomeEarnerBasic
 
data IncomeEarnerIds
instance Eq IncomeEarnerIds
instance Show IncomeEarnerIds
instance SchemaType IncomeEarnerIds
 
data IncomeEarnerOther
instance Eq IncomeEarnerOther
instance Show IncomeEarnerOther
instance SchemaType IncomeEarnerOther
 
data IncomeEarnerTypes
instance Eq IncomeEarnerTypes
instance Show IncomeEarnerTypes
instance SchemaType IncomeEarnerTypes
 
data InsuranceData
instance Eq InsuranceData
instance Show InsuranceData
instance SchemaType InsuranceData
 
data InsuranceExceptions
instance Eq InsuranceExceptions
instance Show InsuranceExceptions
instance SchemaType InsuranceExceptions
 
data InternationalData
instance Eq InternationalData
instance Show InternationalData
instance SchemaType InternationalData
 
data KmAllowance
instance Eq KmAllowance
instance Show KmAllowance
instance SchemaType KmAllowance
 
data MealBenefit
instance Eq MealBenefit
instance Show MealBenefit
instance SchemaType MealBenefit
 
data NT1Address
instance Eq NT1Address
instance Show NT1Address
instance SchemaType NT1Address
 
data OrigPaymentPeriod
instance Eq OrigPaymentPeriod
instance Show OrigPaymentPeriod
instance SchemaType OrigPaymentPeriod
 
data OrigPaymentPeriods
instance Eq OrigPaymentPeriods
instance Show OrigPaymentPeriods
instance SchemaType OrigPaymentPeriods
 
data OtherBenefit
instance Eq OtherBenefit
instance Show OtherBenefit
instance SchemaType OtherBenefit
 
data PaidAbsence
instance Eq PaidAbsence
instance Show PaidAbsence
instance SchemaType PaidAbsence
 
data PaidAbsencePeriod
instance Eq PaidAbsencePeriod
instance Show PaidAbsencePeriod
instance SchemaType PaidAbsencePeriod
 
data PaidAbsencePeriods
instance Eq PaidAbsencePeriods
instance Show PaidAbsencePeriods
instance SchemaType PaidAbsencePeriods
 
data Payer
instance Eq Payer
instance Show Payer
instance SchemaType Payer
 
data PayerBasic
instance Eq PayerBasic
instance Show PayerBasic
instance SchemaType PayerBasic
 
data PayerIds
instance Eq PayerIds
instance Show PayerIds
instance SchemaType PayerIds
 
data PayerOther
instance Eq PayerOther
instance Show PayerOther
instance SchemaType PayerOther
 
data PayerTypes
instance Eq PayerTypes
instance Show PayerTypes
instance SchemaType PayerTypes
 
data Payment
instance Eq Payment
instance Show Payment
instance SchemaType Payment
 
data PaymentPeriod
instance Eq PaymentPeriod
instance Show PaymentPeriod
instance SchemaType PaymentPeriod
 
data Payments
instance Eq Payments
instance Show Payments
instance SchemaType Payments
 
data PaymentTypes
instance Eq PaymentTypes
instance Show PaymentTypes
instance SchemaType PaymentTypes
 
data PensionInsurance
instance Eq PensionInsurance
instance Show PensionInsurance
instance SchemaType PensionInsurance
 
data Period
instance Eq Period
instance Show Period
instance SchemaType Period
 
data PlaceOfBusiness
instance Eq PlaceOfBusiness
instance Show PlaceOfBusiness
instance SchemaType PlaceOfBusiness
 
data Profession
instance Eq Profession
instance Show Profession
instance SchemaType Profession
 
data Professions
instance Eq Professions
instance Show Professions
instance SchemaType Professions
 
data RecoveryData
instance Eq RecoveryData
instance Show RecoveryData
instance SchemaType RecoveryData
 
data ReimbApp
instance Eq ReimbApp
instance Show ReimbApp
instance SchemaType ReimbApp
 
data ReimbPayment
instance Eq ReimbPayment
instance Show ReimbPayment
instance SchemaType ReimbPayment
 
data Remunerations
instance Eq Remunerations
instance Show Remunerations
instance SchemaType Remunerations
 
data Report
instance Eq Report
instance Show Report
instance SchemaType Report
 
data ReportData
instance Eq ReportData
instance Show ReportData
instance SchemaType ReportData
 
data Reports
instance Eq Reports
instance Show Reports
instance SchemaType Reports
 
data Representative
instance Eq Representative
instance Show Representative
instance SchemaType Representative
 
data RepresentativeIds
instance Eq RepresentativeIds
instance Show RepresentativeIds
instance SchemaType RepresentativeIds
 
data SailorIncome
instance Eq SailorIncome
instance Show SailorIncome
instance SchemaType SailorIncome
 
data ServiceRecipient
instance Eq ServiceRecipient
instance Show ServiceRecipient
instance SchemaType ServiceRecipient
 
data ServiceRecipientAddress
instance Eq ServiceRecipientAddress
instance Show ServiceRecipientAddress
instance SchemaType ServiceRecipientAddress
 
data ServiceRecipientIds
instance Eq ServiceRecipientIds
instance Show ServiceRecipientIds
instance SchemaType ServiceRecipientIds
 
data SixMonthRule
instance Eq SixMonthRule
instance Show SixMonthRule
instance SchemaType SixMonthRule
 
data StayPeriod
instance Eq StayPeriod
instance Show StayPeriod
instance SchemaType StayPeriod
 
data StayPeriodsAbroad
instance Eq StayPeriodsAbroad
instance Show StayPeriodsAbroad
instance SchemaType StayPeriodsAbroad
 
data StayPeriodsInFinland
instance Eq StayPeriodsInFinland
instance Show StayPeriodsInFinland
instance SchemaType StayPeriodsInFinland
 
data SubOrg
instance Eq SubOrg
instance Show SubOrg
instance SchemaType SubOrg
 
data SubOrgs
instance Eq SubOrgs
instance Show SubOrgs
instance SchemaType SubOrgs
 
data SubstitutePayer
instance Eq SubstitutePayer
instance Show SubstitutePayer
instance SchemaType SubstitutePayer
 
data Transaction
instance Eq Transaction
instance Show Transaction
instance SchemaType Transaction
 
data TransactionBasic
instance Eq TransactionBasic
instance Show TransactionBasic
instance SchemaType TransactionBasic
 
data TransactionInclusion
instance Eq TransactionInclusion
instance Show TransactionInclusion
instance SchemaType TransactionInclusion
 
data Transactions
instance Eq Transactions
instance Show Transactions
instance SchemaType Transactions
 
data TypedAddress
instance Eq TypedAddress
instance Show TypedAddress
instance SchemaType TypedAddress
 
data UnitWage
instance Eq UnitWage
instance Show UnitWage
instance SchemaType UnitWage
 
data UnitWages
instance Eq UnitWages
instance Show UnitWages
instance SchemaType UnitWages
 
data UnpaidAbsence
instance Eq UnpaidAbsence
instance Show UnpaidAbsence
instance SchemaType UnpaidAbsence
 
data UnpaidAbsencePeriod
instance Eq UnpaidAbsencePeriod
instance Show UnpaidAbsencePeriod
instance SchemaType UnpaidAbsencePeriod
 
data UnpaidAbsencePeriods
instance Eq UnpaidAbsencePeriods
instance Show UnpaidAbsencePeriods
instance SchemaType UnpaidAbsencePeriods
 
data WageReportsToIR
instance Eq WageReportsToIR
instance Show WageReportsToIR
instance SchemaType WageReportsToIR
 
data WithdrawalPeriod
instance Eq WithdrawalPeriod
instance Show WithdrawalPeriod
instance SchemaType WithdrawalPeriod
 
data WorkCountries
instance Eq WorkCountries
instance Show WorkCountries
instance SchemaType WorkCountries
 
data WorkCountry
instance Eq WorkCountry
instance Show WorkCountry
instance SchemaType WorkCountry
 
data WorkCountryAddress
instance Eq WorkCountryAddress
instance Show WorkCountryAddress
instance SchemaType WorkCountryAddress
 
data WorkForm
instance Eq WorkForm
instance Show WorkForm
instance SchemaType WorkForm
 
data WorkMunicipalities
instance Eq WorkMunicipalities
instance Show WorkMunicipalities
instance SchemaType WorkMunicipalities
 
data WorkPeriodInFinland
instance Eq WorkPeriodInFinland
instance Show WorkPeriodInFinland
instance SchemaType WorkPeriodInFinland
 
data WorkPeriodsAbroad
instance Eq WorkPeriodsAbroad
instance Show WorkPeriodsAbroad
instance SchemaType WorkPeriodsAbroad
 
data WorkPeriodsInFinland
instance Eq WorkPeriodsInFinland
instance Show WorkPeriodsInFinland
instance SchemaType WorkPeriodsInFinland
