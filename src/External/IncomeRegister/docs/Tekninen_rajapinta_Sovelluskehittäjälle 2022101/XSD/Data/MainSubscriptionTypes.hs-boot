{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.MainSubscriptionTypes'xsd
  ( module Data.MainSubscriptionTypes'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
newtype Day = Day DaysType
instance Eq Day
instance Show Day
instance Restricts Day DaysType
instance SchemaType Day
instance SimpleType Day
 
data DaysType
instance Eq DaysType
instance Show DaysType
instance Enum DaysType
instance SchemaType DaysType
instance SimpleType DaysType
 
newtype Month = Month Irct.MonthsType
instance Eq Month
instance Show Month
instance Restricts Month Irct.MonthsType
instance SchemaType Month
instance SimpleType Month
 
data CodeItem
instance Eq CodeItem
instance Show CodeItem
instance SchemaType CodeItem
 
data CodeItems
instance Eq CodeItems
instance Show CodeItems
instance SchemaType CodeItems
 
data DailySchedule
instance Eq DailySchedule
instance Show DailySchedule
instance SchemaType DailySchedule
 
data DataRequestToIR
instance Eq DataRequestToIR
instance Show DataRequestToIR
instance SchemaType DataRequestToIR
 
data DataRequestParameters
instance Eq DataRequestParameters
instance Show DataRequestParameters
instance SchemaType DataRequestParameters
 
data SubscriptionsToIR
instance Eq SubscriptionsToIR
instance Show SubscriptionsToIR
instance SchemaType SubscriptionsToIR
 
data Days
instance Eq Days
instance Show Days
instance SchemaType Days
 
data DeliveryData
instance Eq DeliveryData
instance Show DeliveryData
instance SchemaType DeliveryData
 
data DeliverySchedule
instance Eq DeliverySchedule
instance Show DeliverySchedule
instance SchemaType DeliverySchedule
 
data Id
instance Eq Id
instance Show Id
instance SchemaType Id
 
data IdCodeList
instance Eq IdCodeList
instance Show IdCodeList
instance SchemaType IdCodeList
 
data IdCodeParameter
instance Eq IdCodeParameter
instance Show IdCodeParameter
instance SchemaType IdCodeParameter
 
data MainSubscription
instance Eq MainSubscription
instance Show MainSubscription
instance SchemaType MainSubscription
 
data MonthlySchedule
instance Eq MonthlySchedule
instance Show MonthlySchedule
instance SchemaType MonthlySchedule
instance Extension MonthlySchedule DeliverySchedule
 
data Months
instance Eq Months
instance Show Months
instance SchemaType Months
 
data OnetimeDeliverySchedule
instance Eq OnetimeDeliverySchedule
instance Show OnetimeDeliverySchedule
instance SchemaType OnetimeDeliverySchedule
 
data QueryStartTime
instance Eq QueryStartTime
instance Show QueryStartTime
instance SchemaType QueryStartTime
 
data RecurringDeliverySchedule
instance Eq RecurringDeliverySchedule
instance Show RecurringDeliverySchedule
instance SchemaType RecurringDeliverySchedule
 
data Schedule
instance Eq Schedule
instance Show Schedule
instance SchemaType Schedule
 
data Subscription
instance Eq Subscription
instance Show Subscription
instance SchemaType Subscription
 
data SubscriptionParameters
instance Eq SubscriptionParameters
instance Show SubscriptionParameters
instance SchemaType SubscriptionParameters
 
data Subscriptions
instance Eq Subscriptions
instance Show Subscriptions
instance SchemaType Subscriptions
 
data TimespanParameter
instance Eq TimespanParameter
instance Show TimespanParameter
instance SchemaType TimespanParameter
 
data TimespanTimeParameter
instance Eq TimespanTimeParameter
instance Show TimespanTimeParameter
instance SchemaType TimespanTimeParameter
 
data ValueParameter
instance Eq ValueParameter
instance Show ValueParameter
instance SchemaType ValueParameter
 
data WeeklySchedule
instance Eq WeeklySchedule
instance Show WeeklySchedule
instance SchemaType WeeklySchedule
instance Extension WeeklySchedule DeliverySchedule
