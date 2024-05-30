{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.WageReportsFromIR'xsd
  ( module Data.WageReportsFromIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.WageReportsFromIRTypes'xsd as Wrfirt
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementWageReportsFromIR :: XMLParser Wrfirt.WageReportsFromIR
elementWageReportsFromIR = parseSchemaType "WageReportsFromIR"
elementToXMLWageReportsFromIR :: Wrfirt.WageReportsFromIR -> [Content ()]
elementToXMLWageReportsFromIR = schemaTypeToXML "WageReportsFromIR"
