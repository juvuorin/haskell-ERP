{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.StatusRequestToIR'xsd
  ( module Data.StatusRequestToIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.StatusMessageTypes'xsd as Smt
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementStatusRequestToIR :: XMLParser Smt.StatusRequestToIR
elementStatusRequestToIR = parseSchemaType "StatusRequestToIR"
elementToXMLStatusRequestToIR :: Smt.StatusRequestToIR -> [Content ()]
elementToXMLStatusRequestToIR = schemaTypeToXML "StatusRequestToIR"
