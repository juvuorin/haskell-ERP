{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.BenefitReportsToIRTypes'xsd
  ( module Data.BenefitReportsToIRTypes'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
data Address
instance Eq Address
instance Show Address
instance SchemaType Address
 
data BenefitReportsToIR
instance Eq BenefitReportsToIR
instance Show BenefitReportsToIR
instance SchemaType BenefitReportsToIR
 
data BenefitUnit
instance Eq BenefitUnit
instance Show BenefitUnit
instance SchemaType BenefitUnit
 
data ContactPerson
instance Eq ContactPerson
instance Show ContactPerson
instance SchemaType ContactPerson
 
data ContactPersons
instance Eq ContactPersons
instance Show ContactPersons
instance SchemaType ContactPersons
 
data Deduction
instance Eq Deduction
instance Show Deduction
instance SchemaType Deduction
 
data Deductions
instance Eq Deductions
instance Show Deductions
instance SchemaType Deductions
 
data DeliveryData
instance Eq DeliveryData
instance Show DeliveryData
instance SchemaType DeliveryData
 
data EarningPeriod
instance Eq EarningPeriod
instance Show EarningPeriod
instance SchemaType EarningPeriod
 
data Id
instance Eq Id
instance Show Id
instance SchemaType Id
 
data IncomeBeneficiary
instance Eq IncomeBeneficiary
instance Show IncomeBeneficiary
instance SchemaType IncomeBeneficiary
 
data IncomeBeneficiaryBasic
instance Eq IncomeBeneficiaryBasic
instance Show IncomeBeneficiaryBasic
instance SchemaType IncomeBeneficiaryBasic
 
data IncomeBeneficiaryIds
instance Eq IncomeBeneficiaryIds
instance Show IncomeBeneficiaryIds
instance SchemaType IncomeBeneficiaryIds
 
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
 
data Insurance
instance Eq Insurance
instance Show Insurance
instance SchemaType Insurance
 
data InsurancePolicyHolder
instance Eq InsurancePolicyHolder
instance Show InsurancePolicyHolder
instance SchemaType InsurancePolicyHolder
 
data InsurancePolicyHolderBasic
instance Eq InsurancePolicyHolderBasic
instance Show InsurancePolicyHolderBasic
instance SchemaType InsurancePolicyHolderBasic
 
data InsurancePolicyHolderIds
instance Eq InsurancePolicyHolderIds
instance Show InsurancePolicyHolderIds
instance SchemaType InsurancePolicyHolderIds
 
data InternationalData
instance Eq InternationalData
instance Show InternationalData
instance SchemaType InternationalData
 
data OrigBenefitEarningPeriod
instance Eq OrigBenefitEarningPeriod
instance Show OrigBenefitEarningPeriod
instance SchemaType OrigBenefitEarningPeriod
 
data OrigEarningPeriod
instance Eq OrigEarningPeriod
instance Show OrigEarningPeriod
instance SchemaType OrigEarningPeriod
 
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
 
data RecourseData
instance Eq RecourseData
instance Show RecourseData
instance SchemaType RecourseData
 
data RecoveryData
instance Eq RecoveryData
instance Show RecoveryData
instance SchemaType RecoveryData
 
data RemittancePeriod
instance Eq RemittancePeriod
instance Show RemittancePeriod
instance SchemaType RemittancePeriod
 
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
 
data SubOrgs
instance Eq SubOrgs
instance Show SubOrgs
instance SchemaType SubOrgs
 
data SubOrg
instance Eq SubOrg
instance Show SubOrg
instance SchemaType SubOrg
 
data Transaction
instance Eq Transaction
instance Show Transaction
instance SchemaType Transaction
 
data TransactionBasic
instance Eq TransactionBasic
instance Show TransactionBasic
instance SchemaType TransactionBasic
 
data TransactionOther
instance Eq TransactionOther
instance Show TransactionOther
instance SchemaType TransactionOther
 
data Transactions
instance Eq Transactions
instance Show Transactions
instance SchemaType Transactions
