{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.DataRequestToIR'xsd
  ( module Data.DataRequestToIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.MainSubscriptionTypes'xsd as Mst
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementMessagesRequestToIR :: XMLParser Mst.DataRequestToIR
elementMessagesRequestToIR = parseSchemaType "MessagesRequestToIR"
elementToXMLMessagesRequestToIR :: Mst.DataRequestToIR -> [Content ()]
elementToXMLMessagesRequestToIR = schemaTypeToXML "MessagesRequestToIR"
 
elementPayerSummaryReportsOnePayerRequestToIR :: XMLParser Mst.DataRequestToIR
elementPayerSummaryReportsOnePayerRequestToIR = parseSchemaType "PayerSummaryReportsOnePayerRequestToIR"
elementToXMLPayerSummaryReportsOnePayerRequestToIR :: Mst.DataRequestToIR -> [Content ()]
elementToXMLPayerSummaryReportsOnePayerRequestToIR = schemaTypeToXML "PayerSummaryReportsOnePayerRequestToIR"
 
elementPayerSummaryReportsOnePolicyNoRequestToIR :: XMLParser Mst.DataRequestToIR
elementPayerSummaryReportsOnePolicyNoRequestToIR = parseSchemaType "PayerSummaryReportsOnePolicyNoRequestToIR"
elementToXMLPayerSummaryReportsOnePolicyNoRequestToIR :: Mst.DataRequestToIR -> [Content ()]
elementToXMLPayerSummaryReportsOnePolicyNoRequestToIR = schemaTypeToXML "PayerSummaryReportsOnePolicyNoRequestToIR"
 
elementWageReportsOneIncomeEarnerRequestToIR :: XMLParser Mst.DataRequestToIR
elementWageReportsOneIncomeEarnerRequestToIR = parseSchemaType "WageReportsOneIncomeEarnerRequestToIR"
elementToXMLWageReportsOneIncomeEarnerRequestToIR :: Mst.DataRequestToIR -> [Content ()]
elementToXMLWageReportsOneIncomeEarnerRequestToIR = schemaTypeToXML "WageReportsOneIncomeEarnerRequestToIR"
 
elementWageReportsOnePayerRequestToIR :: XMLParser Mst.DataRequestToIR
elementWageReportsOnePayerRequestToIR = parseSchemaType "WageReportsOnePayerRequestToIR"
elementToXMLWageReportsOnePayerRequestToIR :: Mst.DataRequestToIR -> [Content ()]
elementToXMLWageReportsOnePayerRequestToIR = schemaTypeToXML "WageReportsOnePayerRequestToIR"
 
elementWageReportsOnePayerOneIncomeEarnerRequestToIR :: XMLParser Mst.DataRequestToIR
elementWageReportsOnePayerOneIncomeEarnerRequestToIR = parseSchemaType "WageReportsOnePayerOneIncomeEarnerRequestToIR"
elementToXMLWageReportsOnePayerOneIncomeEarnerRequestToIR :: Mst.DataRequestToIR -> [Content ()]
elementToXMLWageReportsOnePayerOneIncomeEarnerRequestToIR = schemaTypeToXML "WageReportsOnePayerOneIncomeEarnerRequestToIR"
 
elementWageReportsOnePolicyNoRequestToIR :: XMLParser Mst.DataRequestToIR
elementWageReportsOnePolicyNoRequestToIR = parseSchemaType "WageReportsOnePolicyNoRequestToIR"
elementToXMLWageReportsOnePolicyNoRequestToIR :: Mst.DataRequestToIR -> [Content ()]
elementToXMLWageReportsOnePolicyNoRequestToIR = schemaTypeToXML "WageReportsOnePolicyNoRequestToIR"
 
elementBenefitReportsOneIncomeEarnerRequestToIR :: XMLParser Mst.DataRequestToIR
elementBenefitReportsOneIncomeEarnerRequestToIR = parseSchemaType "BenefitReportsOneIncomeEarnerRequestToIR"
elementToXMLBenefitReportsOneIncomeEarnerRequestToIR :: Mst.DataRequestToIR -> [Content ()]
elementToXMLBenefitReportsOneIncomeEarnerRequestToIR = schemaTypeToXML "BenefitReportsOneIncomeEarnerRequestToIR"
 
elementBenefitReportsOnePayerOneIncomeEarnerRequestToIR :: XMLParser Mst.DataRequestToIR
elementBenefitReportsOnePayerOneIncomeEarnerRequestToIR = parseSchemaType "BenefitReportsOnePayerOneIncomeEarnerRequestToIR"
elementToXMLBenefitReportsOnePayerOneIncomeEarnerRequestToIR :: Mst.DataRequestToIR -> [Content ()]
elementToXMLBenefitReportsOnePayerOneIncomeEarnerRequestToIR = schemaTypeToXML "BenefitReportsOnePayerOneIncomeEarnerRequestToIR"
 
elementBenefitReportsOneIRReportIdRequestToIR :: XMLParser Mst.DataRequestToIR
elementBenefitReportsOneIRReportIdRequestToIR = parseSchemaType "BenefitReportsOneIRReportIdRequestToIR"
elementToXMLBenefitReportsOneIRReportIdRequestToIR :: Mst.DataRequestToIR -> [Content ()]
elementToXMLBenefitReportsOneIRReportIdRequestToIR = schemaTypeToXML "BenefitReportsOneIRReportIdRequestToIR"
