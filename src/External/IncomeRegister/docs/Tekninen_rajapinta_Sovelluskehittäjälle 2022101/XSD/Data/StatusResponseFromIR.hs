{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.StatusResponseFromIR'xsd
  ( module Data.StatusResponseFromIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.StatusMessageTypes'xsd as Smt
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementStatusResponseFromIR :: XMLParser Smt.StatusResponseFromIR
elementStatusResponseFromIR = parseSchemaType "StatusResponseFromIR"
elementToXMLStatusResponseFromIR :: Smt.StatusResponseFromIR -> [Content ()]
elementToXMLStatusResponseFromIR = schemaTypeToXML "StatusResponseFromIR"
