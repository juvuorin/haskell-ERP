{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.LogDataTypes'xsd
  ( module Data.LogDataTypes'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
newtype TargetItemType = TargetItemType Xs.Int
instance Eq TargetItemType
instance Show TargetItemType
instance Restricts TargetItemType Xs.Int
instance SchemaType TargetItemType
instance SimpleType TargetItemType
 
data DeliveryTargetItem
instance Eq DeliveryTargetItem
instance Show DeliveryTargetItem
instance SchemaType DeliveryTargetItem
 
data IdCodeTargetItem
instance Eq IdCodeTargetItem
instance Show IdCodeTargetItem
instance SchemaType IdCodeTargetItem
 
data LogDataFromIR
instance Eq LogDataFromIR
instance Show LogDataFromIR
instance SchemaType LogDataFromIR
 
data LogEvent
instance Eq LogEvent
instance Show LogEvent
instance SchemaType LogEvent
 
data LogEvents
instance Eq LogEvents
instance Show LogEvents
instance SchemaType LogEvents
 
data MainSubscriptionTargetItem
instance Eq MainSubscriptionTargetItem
instance Show MainSubscriptionTargetItem
instance SchemaType MainSubscriptionTargetItem
 
data MessageTargetItem
instance Eq MessageTargetItem
instance Show MessageTargetItem
instance SchemaType MessageTargetItem
 
data OtherTargetItem
instance Eq OtherTargetItem
instance Show OtherTargetItem
instance SchemaType OtherTargetItem
 
data Query
instance Eq Query
instance Show Query
instance SchemaType Query
 
data QueryTargetItem
instance Eq QueryTargetItem
instance Show QueryTargetItem
instance SchemaType QueryTargetItem
 
data ReportTargetItem
instance Eq ReportTargetItem
instance Show ReportTargetItem
instance SchemaType ReportTargetItem
 
data Subscription
instance Eq Subscription
instance Show Subscription
instance SchemaType Subscription
 
data Summary
instance Eq Summary
instance Show Summary
instance SchemaType Summary
 
data TargetItem
instance Eq TargetItem
instance Show TargetItem
instance SchemaType TargetItem
 
data TargetItems
instance Eq TargetItems
instance Show TargetItems
instance SchemaType TargetItems
