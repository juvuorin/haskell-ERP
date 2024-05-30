{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.BenefitReportsFromIR'xsd
  ( module Data.BenefitReportsFromIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
elementBenefitReportsFromIR :: XMLParser Brfirt.BenefitReportsFromIR
elementToXMLBenefitReportsFromIR :: Brfirt.BenefitReportsFromIR -> [Content ()]
