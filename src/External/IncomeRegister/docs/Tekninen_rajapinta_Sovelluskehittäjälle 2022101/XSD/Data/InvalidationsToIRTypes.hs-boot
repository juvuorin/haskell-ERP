{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.InvalidationsToIRTypes'xsd
  ( module Data.InvalidationsToIRTypes'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
data DeliveryData
instance Eq DeliveryData
instance Show DeliveryData
instance SchemaType DeliveryData
 
data Id
instance Eq Id
instance Show Id
instance SchemaType Id
 
data InvalidationsToIR
instance Eq InvalidationsToIR
instance Show InvalidationsToIR
instance SchemaType InvalidationsToIR
 
data Item
instance Eq Item
instance Show Item
instance SchemaType Item
 
data Items
instance Eq Items
instance Show Items
instance SchemaType Items
