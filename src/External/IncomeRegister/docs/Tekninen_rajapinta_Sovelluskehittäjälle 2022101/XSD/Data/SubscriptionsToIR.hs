{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.SubscriptionsToIR'xsd
  ( module Data.SubscriptionsToIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.MainSubscriptionTypes'xsd as Mst
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementSubscriptionsRequestToIR :: XMLParser Mst.SubscriptionsToIR
elementSubscriptionsRequestToIR = parseSchemaType "SubscriptionsRequestToIR"
elementToXMLSubscriptionsRequestToIR :: Mst.SubscriptionsToIR -> [Content ()]
elementToXMLSubscriptionsRequestToIR = schemaTypeToXML "SubscriptionsRequestToIR"
 
elementSubscriptionsRequestToIRAsync :: XMLParser Mst.SubscriptionsToIR
elementSubscriptionsRequestToIRAsync = parseSchemaType "SubscriptionsRequestToIRAsync"
elementToXMLSubscriptionsRequestToIRAsync :: Mst.SubscriptionsToIR -> [Content ()]
elementToXMLSubscriptionsRequestToIRAsync = schemaTypeToXML "SubscriptionsRequestToIRAsync"
