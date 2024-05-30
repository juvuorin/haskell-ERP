{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.StatusMessageTypes
  ( module Data.StatusMessageTypes
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
data AckData
instance Eq AckData
instance Show AckData
instance SchemaType AckData
instance Extension AckData Message
 
data AckFromIR
instance Eq AckFromIR
instance Show AckFromIR
instance SchemaType AckFromIR
 
data DeliveryData
instance Eq DeliveryData
instance Show DeliveryData
instance SchemaType DeliveryData
 
data DeliveryErrors
instance Eq DeliveryErrors
instance Show DeliveryErrors
instance SchemaType DeliveryErrors
 
data ErrorInfo
instance Eq ErrorInfo
instance Show ErrorInfo
instance SchemaType ErrorInfo
 
data Id
instance Eq Id
instance Show Id
instance SchemaType Id
 
data InvalidItems
instance Eq InvalidItems
instance Show InvalidItems
instance SchemaType InvalidItems
 
data Item
instance Eq Item
instance Show Item
instance SchemaType Item
 
data ItemErrors
instance Eq ItemErrors
instance Show ItemErrors
instance SchemaType ItemErrors
 
data Message
instance Eq Message
instance Show Message
instance SchemaType Message
 
data MessageErrors
instance Eq MessageErrors
instance Show MessageErrors
instance SchemaType MessageErrors
 
data StatusRequestToIR
instance Eq StatusRequestToIR
instance Show StatusRequestToIR
instance SchemaType StatusRequestToIR
 
data StatusResponse
instance Eq StatusResponse
instance Show StatusResponse
instance SchemaType StatusResponse
instance Extension StatusResponse Message
 
data StatusResponseFromIR
instance Eq StatusResponseFromIR
instance Show StatusResponseFromIR
instance SchemaType StatusResponseFromIR
 
data ValidItems
instance Eq ValidItems
instance Show ValidItems
instance SchemaType ValidItems
