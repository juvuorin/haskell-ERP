{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.WageReportSummaryFromIRTypes'xsd
  ( module Data.WageReportSummaryFromIRTypes'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
data AccidentInsurance
instance Eq AccidentInsurance
instance Show AccidentInsurance
instance SchemaType AccidentInsurance
 
data AccidentInsuranceIncome
instance Eq AccidentInsuranceIncome
instance Show AccidentInsuranceIncome
instance SchemaType AccidentInsuranceIncome
 
data AccInsPolicyNumbersSummary
instance Eq AccInsPolicyNumbersSummary
instance Show AccInsPolicyNumbersSummary
instance SchemaType AccInsPolicyNumbersSummary
 
data AccInsPolicyNumberSummary
instance Eq AccInsPolicyNumberSummary
instance Show AccInsPolicyNumberSummary
instance SchemaType AccInsPolicyNumberSummary
 
data Id
instance Eq Id
instance Show Id
instance SchemaType Id
 
data Income
instance Eq Income
instance Show Income
instance SchemaType Income
 
data IncomeEarner
instance Eq IncomeEarner
instance Show IncomeEarner
instance SchemaType IncomeEarner
 
data IncomeEarnerIncome
instance Eq IncomeEarnerIncome
instance Show IncomeEarnerIncome
instance SchemaType IncomeEarnerIncome
 
data IncomeEarnersSummary
instance Eq IncomeEarnersSummary
instance Show IncomeEarnersSummary
instance SchemaType IncomeEarnersSummary
 
data IncomeEarnerSummary
instance Eq IncomeEarnerSummary
instance Show IncomeEarnerSummary
instance SchemaType IncomeEarnerSummary
 
data IncomeSummary
instance Eq IncomeSummary
instance Show IncomeSummary
instance SchemaType IncomeSummary
 
data Payer
instance Eq Payer
instance Show Payer
instance SchemaType Payer
 
data PaymentDateSummary
instance Eq PaymentDateSummary
instance Show PaymentDateSummary
instance SchemaType PaymentDateSummary
 
data PensionInsurance
instance Eq PensionInsurance
instance Show PensionInsurance
instance SchemaType PensionInsurance
 
data PensionInsuranceIncome
instance Eq PensionInsuranceIncome
instance Show PensionInsuranceIncome
instance SchemaType PensionInsuranceIncome
 
data PensionPolicyNumbersSummary
instance Eq PensionPolicyNumbersSummary
instance Show PensionPolicyNumbersSummary
instance SchemaType PensionPolicyNumbersSummary
 
data PensionPolicyNumberSummary
instance Eq PensionPolicyNumberSummary
instance Show PensionPolicyNumberSummary
instance SchemaType PensionPolicyNumberSummary
 
data Query
instance Eq Query
instance Show Query
instance SchemaType Query
 
data Recovery
instance Eq Recovery
instance Show Recovery
instance SchemaType Recovery
 
data ReporterSubTypeIncome
instance Eq ReporterSubTypeIncome
instance Show ReporterSubTypeIncome
instance SchemaType ReporterSubTypeIncome
 
data SubOrg
instance Eq SubOrg
instance Show SubOrg
instance SchemaType SubOrg
 
data Subscription
instance Eq Subscription
instance Show Subscription
instance SchemaType Subscription
 
data Summary
instance Eq Summary
instance Show Summary
instance SchemaType Summary
 
data UnjustEnrichment
instance Eq UnjustEnrichment
instance Show UnjustEnrichment
instance SchemaType UnjustEnrichment
 
data WageReportPaymentDatesSummary
instance Eq WageReportPaymentDatesSummary
instance Show WageReportPaymentDatesSummary
instance SchemaType WageReportPaymentDatesSummary
 
data WageReportSummaryFromIR
instance Eq WageReportSummaryFromIR
instance Show WageReportSummaryFromIR
instance SchemaType WageReportSummaryFromIR
