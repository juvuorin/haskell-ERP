{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.PayerSummaryReportsToIR
  ( module Data.PayerSummaryReportsToIR
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.PayerSummaryReportsToIRTypes as Psrtirt
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementPayerSummaryReportRequestToIR :: XMLParser Psrtirt.PayerSummaryReportsToIR
elementPayerSummaryReportRequestToIR = parseSchemaType "PayerSummaryReportRequestToIR"
elementToXMLPayerSummaryReportRequestToIR :: Psrtirt.PayerSummaryReportsToIR -> [Content ()]
elementToXMLPayerSummaryReportRequestToIR = schemaTypeToXML "PayerSummaryReportRequestToIR"
 
elementPayerSummaryReportsRequestToIR :: XMLParser Psrtirt.PayerSummaryReportsToIR
elementPayerSummaryReportsRequestToIR = parseSchemaType "PayerSummaryReportsRequestToIR"
elementToXMLPayerSummaryReportsRequestToIR :: Psrtirt.PayerSummaryReportsToIR -> [Content ()]
elementToXMLPayerSummaryReportsRequestToIR = schemaTypeToXML "PayerSummaryReportsRequestToIR"
