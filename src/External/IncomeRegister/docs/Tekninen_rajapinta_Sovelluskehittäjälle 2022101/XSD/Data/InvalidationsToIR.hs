{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.InvalidationsToIR'xsd
  ( module Data.InvalidationsToIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.InvalidationsToIRTypes'xsd as Itirt
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementInvalidationRequestToIR :: XMLParser Itirt.InvalidationsToIR
elementInvalidationRequestToIR = parseSchemaType "InvalidationRequestToIR"
elementToXMLInvalidationRequestToIR :: Itirt.InvalidationsToIR -> [Content ()]
elementToXMLInvalidationRequestToIR = schemaTypeToXML "InvalidationRequestToIR"
 
elementInvalidationsRequestToIR :: XMLParser Itirt.InvalidationsToIR
elementInvalidationsRequestToIR = parseSchemaType "InvalidationsRequestToIR"
elementToXMLInvalidationsRequestToIR :: Itirt.InvalidationsToIR -> [Content ()]
elementToXMLInvalidationsRequestToIR = schemaTypeToXML "InvalidationsRequestToIR"
