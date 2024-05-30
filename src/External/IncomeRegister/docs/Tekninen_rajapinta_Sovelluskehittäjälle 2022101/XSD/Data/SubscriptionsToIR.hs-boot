{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.SubscriptionsToIR'xsd
  ( module Data.SubscriptionsToIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
elementSubscriptionsRequestToIR :: XMLParser Mst.SubscriptionsToIR
elementToXMLSubscriptionsRequestToIR :: Mst.SubscriptionsToIR -> [Content ()]
 
elementSubscriptionsRequestToIRAsync :: XMLParser Mst.SubscriptionsToIR
elementToXMLSubscriptionsRequestToIRAsync :: Mst.SubscriptionsToIR -> [Content ()]
