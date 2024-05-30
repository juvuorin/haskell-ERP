{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.DataRequestToIR'xsd
  ( module Data.DataRequestToIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
elementMessagesRequestToIR :: XMLParser Mst.DataRequestToIR
elementToXMLMessagesRequestToIR :: Mst.DataRequestToIR -> [Content ()]
 
elementPayerSummaryReportsOnePayerRequestToIR :: XMLParser Mst.DataRequestToIR
elementToXMLPayerSummaryReportsOnePayerRequestToIR :: Mst.DataRequestToIR -> [Content ()]
 
elementPayerSummaryReportsOnePolicyNoRequestToIR :: XMLParser Mst.DataRequestToIR
elementToXMLPayerSummaryReportsOnePolicyNoRequestToIR :: Mst.DataRequestToIR -> [Content ()]
 
elementWageReportsOneIncomeEarnerRequestToIR :: XMLParser Mst.DataRequestToIR
elementToXMLWageReportsOneIncomeEarnerRequestToIR :: Mst.DataRequestToIR -> [Content ()]
 
elementWageReportsOnePayerRequestToIR :: XMLParser Mst.DataRequestToIR
elementToXMLWageReportsOnePayerRequestToIR :: Mst.DataRequestToIR -> [Content ()]
 
elementWageReportsOnePayerOneIncomeEarnerRequestToIR :: XMLParser Mst.DataRequestToIR
elementToXMLWageReportsOnePayerOneIncomeEarnerRequestToIR :: Mst.DataRequestToIR -> [Content ()]
 
elementWageReportsOnePolicyNoRequestToIR :: XMLParser Mst.DataRequestToIR
elementToXMLWageReportsOnePolicyNoRequestToIR :: Mst.DataRequestToIR -> [Content ()]
 
elementBenefitReportsOneIncomeEarnerRequestToIR :: XMLParser Mst.DataRequestToIR
elementToXMLBenefitReportsOneIncomeEarnerRequestToIR :: Mst.DataRequestToIR -> [Content ()]
 
elementBenefitReportsOnePayerOneIncomeEarnerRequestToIR :: XMLParser Mst.DataRequestToIR
elementToXMLBenefitReportsOnePayerOneIncomeEarnerRequestToIR :: Mst.DataRequestToIR -> [Content ()]
 
elementBenefitReportsOneIRReportIdRequestToIR :: XMLParser Mst.DataRequestToIR
elementToXMLBenefitReportsOneIRReportIdRequestToIR :: Mst.DataRequestToIR -> [Content ()]
