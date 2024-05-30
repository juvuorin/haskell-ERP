{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.PayerSummaryReportsToIR
  ( module Data.PayerSummaryReportsToIR
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.PayerSummaryReportsToIRTypes as Psrtirt  
elementPayerSummaryReportRequestToIR :: XMLParser Psrtirt.PayerSummaryReportsToIR
elementToXMLPayerSummaryReportRequestToIR :: Psrtirt.PayerSummaryReportsToIR -> [Content ()]
 
elementPayerSummaryReportsRequestToIR :: XMLParser Psrtirt.PayerSummaryReportsToIR
elementToXMLPayerSummaryReportsRequestToIR :: Psrtirt.PayerSummaryReportsToIR -> [Content ()]
