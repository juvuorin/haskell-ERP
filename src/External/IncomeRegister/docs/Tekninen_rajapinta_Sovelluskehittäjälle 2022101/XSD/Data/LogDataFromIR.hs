{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.LogDataFromIR'xsd
  ( module Data.LogDataFromIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.LogDataTypes'xsd as Ldt
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementLogDataFromIR :: XMLParser Ldt.LogDataFromIR
elementLogDataFromIR = parseSchemaType "LogDataFromIR"
elementToXMLLogDataFromIR :: Ldt.LogDataFromIR -> [Content ()]
elementToXMLLogDataFromIR = schemaTypeToXML "LogDataFromIR"
