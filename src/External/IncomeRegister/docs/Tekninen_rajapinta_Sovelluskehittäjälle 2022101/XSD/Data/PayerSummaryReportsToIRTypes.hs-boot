{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.PayerSummaryReportsToIRTypes'xsd
  ( module Data.PayerSummaryReportsToIRTypes'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
data AccidentInsurance
instance Eq AccidentInsurance
instance Show AccidentInsurance
instance SchemaType AccidentInsurance
 
data AccidentInsurances
instance Eq AccidentInsurances
instance Show AccidentInsurances
instance SchemaType AccidentInsurances
 
data Address
instance Eq Address
instance Show Address
instance SchemaType Address
 
data ContactPerson
instance Eq ContactPerson
instance Show ContactPerson
instance SchemaType ContactPerson
 
data ContactPersons
instance Eq ContactPersons
instance Show ContactPersons
instance SchemaType ContactPersons
 
data DeliveryData
instance Eq DeliveryData
instance Show DeliveryData
instance SchemaType DeliveryData
 
data Id
instance Eq Id
instance Show Id
instance SchemaType Id
 
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
 
data PayerSummaryReportsToIR
instance Eq PayerSummaryReportsToIR
instance Show PayerSummaryReportsToIR
instance SchemaType PayerSummaryReportsToIR
 
data PayerTypes
instance Eq PayerTypes
instance Show PayerTypes
instance SchemaType PayerTypes
 
data PaymentMonth
instance Eq PaymentMonth
instance Show PaymentMonth
instance SchemaType PaymentMonth
 
data PensionInsurance
instance Eq PensionInsurance
instance Show PensionInsurance
instance SchemaType PensionInsurance
 
data PensionInsurances
instance Eq PensionInsurances
instance Show PensionInsurances
instance SchemaType PensionInsurances
 
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
 
data SubOrg
instance Eq SubOrg
instance Show SubOrg
instance SchemaType SubOrg
 
data SubOrgs
instance Eq SubOrgs
instance Show SubOrgs
instance SchemaType SubOrgs
 
data Transaction
instance Eq Transaction
instance Show Transaction
instance SchemaType Transaction
 
data TransactionBasic
instance Eq TransactionBasic
instance Show TransactionBasic
instance SchemaType TransactionBasic
 
data Transactions
instance Eq Transactions
instance Show Transactions
instance SchemaType Transactions
