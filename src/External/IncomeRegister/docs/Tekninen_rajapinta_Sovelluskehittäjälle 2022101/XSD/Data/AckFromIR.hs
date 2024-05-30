{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.AckFromIR'xsd
  ( module Data.AckFromIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.StatusMessageTypes'xsd as Smt
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementAckFromIR :: XMLParser Smt.AckFromIR
elementAckFromIR = parseSchemaType "AckFromIR"
elementToXMLAckFromIR :: Smt.AckFromIR -> [Content ()]
elementToXMLAckFromIR = schemaTypeToXML "AckFromIR"
