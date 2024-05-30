{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.LogDataFromIR'xsd
  ( module Data.LogDataFromIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
elementLogDataFromIR :: XMLParser Ldt.LogDataFromIR
elementToXMLLogDataFromIR :: Ldt.LogDataFromIR -> [Content ()]
