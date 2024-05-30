{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.PayerSummaryReportsFromIR'xsd
  ( module Data.PayerSummaryReportsFromIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.PayerSummaryReportsFromIRTypes'xsd as Psrfirt
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementPayerSummaryReportsFromIR :: XMLParser Psrfirt.PayerSummaryReportsFromIR
elementPayerSummaryReportsFromIR = parseSchemaType "PayerSummaryReportsFromIR"
elementToXMLPayerSummaryReportsFromIR :: Psrfirt.PayerSummaryReportsFromIR -> [Content ()]
elementToXMLPayerSummaryReportsFromIR = schemaTypeToXML "PayerSummaryReportsFromIR"
