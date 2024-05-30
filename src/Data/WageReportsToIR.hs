{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.WageReportsToIR
  ( module Data.WageReportsToIR
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.WageReportsToIRTypes as Wrtirt
 
-- Some hs-boot imports are required, for fwd-declaring types.

--elementFinvoice :: XMLParser Finvoice
--elementFinvoice = parseSchemaType "Finvoice"
--elementToXMLFinvoice :: Finvoice -> [Content ()]
--elementToXMLFinvoice = schemaTypeToXML "Finvoice"


elementWageReportRequestToIR :: XMLParser Wrtirt.WageReportsToIR
elementWageReportRequestToIR = parseSchemaType "WageReportRequestToIR"
elementToXMLWageReportRequestToIR :: Wrtirt.WageReportsToIR -> [Content ()]
elementToXMLWageReportRequestToIR = schemaTypeToXML "WageReportRequestToIR"
 
elementWageReportsRequestToIR :: XMLParser Wrtirt.WageReportsToIR
elementWageReportsRequestToIR = parseSchemaType "WageReportsRequestToIR"
elementToXMLWageReportsRequestToIR :: Wrtirt.WageReportsToIR -> [Content ()]
elementToXMLWageReportsRequestToIR = schemaTypeToXML "WageReportsRequestToIR"
