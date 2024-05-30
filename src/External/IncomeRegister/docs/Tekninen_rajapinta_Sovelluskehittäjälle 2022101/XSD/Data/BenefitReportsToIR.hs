{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.BenefitReportsToIR'xsd
  ( module Data.BenefitReportsToIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.BenefitReportsToIRTypes'xsd as Brtirt
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementBenefitReportsRequestToIR :: XMLParser Brtirt.BenefitReportsToIR
elementBenefitReportsRequestToIR = parseSchemaType "BenefitReportsRequestToIR"
elementToXMLBenefitReportsRequestToIR :: Brtirt.BenefitReportsToIR -> [Content ()]
elementToXMLBenefitReportsRequestToIR = schemaTypeToXML "BenefitReportsRequestToIR"
 
elementBenefitReportRequestToIR :: XMLParser Brtirt.BenefitReportsToIR
elementBenefitReportRequestToIR = parseSchemaType "BenefitReportRequestToIR"
elementToXMLBenefitReportRequestToIR :: Brtirt.BenefitReportsToIR -> [Content ()]
elementToXMLBenefitReportRequestToIR = schemaTypeToXML "BenefitReportRequestToIR"
