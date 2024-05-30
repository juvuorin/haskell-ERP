{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.WageReportsToIR'xsd
  ( module Data.WageReportsToIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.WageReportsToIRTypes'xsd as Wrtirt
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementWageReportRequestToIR :: XMLParser Wrtirt.WageReportsToIR
elementWageReportRequestToIR = parseSchemaType "WageReportRequestToIR"
elementToXMLWageReportRequestToIR :: Wrtirt.WageReportsToIR -> [Content ()]
elementToXMLWageReportRequestToIR = schemaTypeToXML "WageReportRequestToIR"
 
elementWageReportsRequestToIR :: XMLParser Wrtirt.WageReportsToIR
elementWageReportsRequestToIR = parseSchemaType "WageReportsRequestToIR"
elementToXMLWageReportsRequestToIR :: Wrtirt.WageReportsToIR -> [Content ()]
elementToXMLWageReportsRequestToIR = schemaTypeToXML "WageReportsRequestToIR"
