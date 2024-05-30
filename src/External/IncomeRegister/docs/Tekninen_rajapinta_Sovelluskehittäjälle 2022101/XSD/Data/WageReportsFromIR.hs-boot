{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.WageReportsFromIR'xsd
  ( module Data.WageReportsFromIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
elementWageReportsFromIR :: XMLParser Wrfirt.WageReportsFromIR
elementToXMLWageReportsFromIR :: Wrfirt.WageReportsFromIR -> [Content ()]
