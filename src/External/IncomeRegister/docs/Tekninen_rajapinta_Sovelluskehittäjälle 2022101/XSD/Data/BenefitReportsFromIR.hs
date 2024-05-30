{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.BenefitReportsFromIR'xsd
  ( module Data.BenefitReportsFromIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.BenefitReportsFromIRTypes'xsd as Brfirt
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementBenefitReportsFromIR :: XMLParser Brfirt.BenefitReportsFromIR
elementBenefitReportsFromIR = parseSchemaType "BenefitReportsFromIR"
elementToXMLBenefitReportsFromIR :: Brfirt.BenefitReportsFromIR -> [Content ()]
elementToXMLBenefitReportsFromIR = schemaTypeToXML "BenefitReportsFromIR"
