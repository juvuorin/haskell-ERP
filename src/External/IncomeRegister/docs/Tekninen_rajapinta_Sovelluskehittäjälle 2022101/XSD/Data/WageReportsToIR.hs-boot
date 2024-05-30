{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.WageReportsToIR'xsd
  ( module Data.WageReportsToIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
elementWageReportRequestToIR :: XMLParser Wrtirt.WageReportsToIR
elementToXMLWageReportRequestToIR :: Wrtirt.WageReportsToIR -> [Content ()]
 
elementWageReportsRequestToIR :: XMLParser Wrtirt.WageReportsToIR
elementToXMLWageReportsRequestToIR :: Wrtirt.WageReportsToIR -> [Content ()]
