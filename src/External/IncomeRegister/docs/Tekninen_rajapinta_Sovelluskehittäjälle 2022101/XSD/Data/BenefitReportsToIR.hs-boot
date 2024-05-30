{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.BenefitReportsToIR'xsd
  ( module Data.BenefitReportsToIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
elementBenefitReportsRequestToIR :: XMLParser Brtirt.BenefitReportsToIR
elementToXMLBenefitReportsRequestToIR :: Brtirt.BenefitReportsToIR -> [Content ()]
 
elementBenefitReportRequestToIR :: XMLParser Brtirt.BenefitReportsToIR
elementToXMLBenefitReportRequestToIR :: Brtirt.BenefitReportsToIR -> [Content ()]
