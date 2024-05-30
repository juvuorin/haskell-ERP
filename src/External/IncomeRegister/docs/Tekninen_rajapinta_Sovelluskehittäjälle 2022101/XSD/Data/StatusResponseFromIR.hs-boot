{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.StatusResponseFromIR'xsd
  ( module Data.StatusResponseFromIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
elementStatusResponseFromIR :: XMLParser Smt.StatusResponseFromIR
elementToXMLStatusResponseFromIR :: Smt.StatusResponseFromIR -> [Content ()]
