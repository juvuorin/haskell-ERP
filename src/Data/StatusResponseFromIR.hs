{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.StatusResponseFromIR
  ( module Data.StatusResponseFromIR
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.StatusMessageTypes as Smt
import Data.AXmldsigA (SignatureType(SignatureType))
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementStatusResponseFromIR :: XMLParser Smt.StatusResponseFromIR
elementStatusResponseFromIR = parseSchemaType "StatusResponseFromIR"
elementStatusResponseFromIRDeliveryData :: XMLParser Smt.DeliveryData
elementStatusResponseFromIRDeliveryData = parseSchemaType "DeliveryData"
elementStatusResponseFromIRSignature :: XMLParser SignatureType
elementStatusResponseFromIRSignature = parseSchemaType "Signature"
elementStatusResponseFromIRStatusResponse :: XMLParser StatusResponse
elementStatusResponseFromIRStatusResponse = parseSchemaType "StatusResponse"



elementToXMLStatusResponseFromIR :: Smt.StatusResponseFromIR -> [Content ()]
elementToXMLStatusResponseFromIR = schemaTypeToXML "StatusResponseFromIR"
