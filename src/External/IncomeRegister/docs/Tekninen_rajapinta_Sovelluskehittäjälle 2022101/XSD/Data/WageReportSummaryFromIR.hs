{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.WageReportSummaryFromIR'xsd
  ( module Data.WageReportSummaryFromIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.WageReportSummaryFromIRTypes'xsd as Wrsfirt
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementWageReportSummaryFromIR :: XMLParser Wrsfirt.WageReportSummaryFromIR
elementWageReportSummaryFromIR = parseSchemaType "WageReportSummaryFromIR"
elementToXMLWageReportSummaryFromIR :: Wrsfirt.WageReportSummaryFromIR -> [Content ()]
elementToXMLWageReportSummaryFromIR = schemaTypeToXML "WageReportSummaryFromIR"
