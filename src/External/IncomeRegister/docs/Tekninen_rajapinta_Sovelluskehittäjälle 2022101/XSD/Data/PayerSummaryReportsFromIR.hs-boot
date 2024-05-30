{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.PayerSummaryReportsFromIR'xsd
  ( module Data.PayerSummaryReportsFromIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
elementPayerSummaryReportsFromIR :: XMLParser Psrfirt.PayerSummaryReportsFromIR
elementToXMLPayerSummaryReportsFromIR :: Psrfirt.PayerSummaryReportsFromIR -> [Content ()]
