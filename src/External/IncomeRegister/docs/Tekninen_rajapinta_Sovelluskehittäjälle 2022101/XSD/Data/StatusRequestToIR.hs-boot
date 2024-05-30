{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.StatusRequestToIR'xsd
  ( module Data.StatusRequestToIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
elementStatusRequestToIR :: XMLParser Smt.StatusRequestToIR
elementToXMLStatusRequestToIR :: Smt.StatusRequestToIR -> [Content ()]
