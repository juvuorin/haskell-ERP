{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.WageReportSummaryFromIR'xsd
  ( module Data.WageReportSummaryFromIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
elementWageReportSummaryFromIR :: XMLParser Wrsfirt.WageReportSummaryFromIR
elementToXMLWageReportSummaryFromIR :: Wrsfirt.WageReportSummaryFromIR -> [Content ()]
