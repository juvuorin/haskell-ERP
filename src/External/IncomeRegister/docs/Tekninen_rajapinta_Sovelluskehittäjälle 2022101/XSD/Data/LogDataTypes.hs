{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.LogDataTypes'xsd
  ( module Data.LogDataTypes'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.IRCommonTypes'xsd as Irct
import Data.Xmldsig'core'schema'xsd as Ds
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
newtype TargetItemType = TargetItemType Xs.Int deriving (Eq,Show)
instance Restricts TargetItemType Xs.Int where
    restricts (TargetItemType x) = x
instance SchemaType TargetItemType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (TargetItemType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType TargetItemType where
    acceptingParser = fmap TargetItemType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (TargetItemType x) = simpleTypeText x
 
data DeliveryTargetItem = DeliveryTargetItem
        { delivTargetItem_targetItemType :: TargetItemType
        , delivTargetItem_deliveryId :: Irct.String40
        , delivTargetItem_iRDeliveryId :: Irct.Guid
        }
        deriving (Eq,Show)
instance SchemaType DeliveryTargetItem where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DeliveryTargetItem
            `apply` parseSchemaType "TargetItemType"
            `apply` parseSchemaType "DeliveryId"
            `apply` parseSchemaType "IRDeliveryId"
    schemaTypeToXML s x@DeliveryTargetItem{} =
        toXMLElement s []
            [ schemaTypeToXML "TargetItemType" $ delivTargetItem_targetItemType x
            , schemaTypeToXML "DeliveryId" $ delivTargetItem_deliveryId x
            , schemaTypeToXML "IRDeliveryId" $ delivTargetItem_iRDeliveryId x
            ]
 
data IdCodeTargetItem = IdCodeTargetItem
        { idCodeTargetItem_type :: Xs.Int
        , idCodeTargetItem_code :: Irct.String30
        , idCodeTargetItem_countryCode :: Maybe Irct.String2
        , idCodeTargetItem_countryName :: Maybe Irct.String70
        }
        deriving (Eq,Show)
instance SchemaType IdCodeTargetItem where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return IdCodeTargetItem
            `apply` parseSchemaType "Type"
            `apply` parseSchemaType "Code"
            `apply` optional (parseSchemaType "CountryCode")
            `apply` optional (parseSchemaType "CountryName")
    schemaTypeToXML s x@IdCodeTargetItem{} =
        toXMLElement s []
            [ schemaTypeToXML "Type" $ idCodeTargetItem_type x
            , schemaTypeToXML "Code" $ idCodeTargetItem_code x
            , maybe [] (schemaTypeToXML "CountryCode") $ idCodeTargetItem_countryCode x
            , maybe [] (schemaTypeToXML "CountryName") $ idCodeTargetItem_countryName x
            ]
 
data LogDataFromIR = LogDataFromIR
        { logDataFromIR_subscription :: Subscription
        , logDataFromIR_query :: Query
        , logDataFromIR_summary :: Summary
        , logDataFromIR_logEvents :: Maybe LogEvents
        , logDataFromIR_signature :: SignatureType
        }
        deriving (Eq,Show)
instance SchemaType LogDataFromIR where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return LogDataFromIR
            `apply` parseSchemaType "Subscription"
            `apply` parseSchemaType "Query"
            `apply` parseSchemaType "Summary"
            `apply` optional (parseSchemaType "LogEvents")
            `apply` elementSignature
    schemaTypeToXML s x@LogDataFromIR{} =
        toXMLElement s []
            [ schemaTypeToXML "Subscription" $ logDataFromIR_subscription x
            , schemaTypeToXML "Query" $ logDataFromIR_query x
            , schemaTypeToXML "Summary" $ logDataFromIR_summary x
            , maybe [] (schemaTypeToXML "LogEvents") $ logDataFromIR_logEvents x
            , elementToXMLSignature $ logDataFromIR_signature x
            ]
 
data LogEvent = LogEvent
        { logEvent_activityType :: Xs.Int
        , logEvent_iRLogEventId :: Irct.Guid
        , logEvent_timestamp :: Xs.DateTime
        , logEvent_uIView :: Irct.String30
        , logEvent_queryProfile :: Maybe Irct.String40
        , logEvent_userIdCode :: Irct.String40
        , logEvent_userOrganisation :: Irct.String30
        , logEvent_targetItems :: Maybe TargetItems
        }
        deriving (Eq,Show)
instance SchemaType LogEvent where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return LogEvent
            `apply` parseSchemaType "ActivityType"
            `apply` parseSchemaType "IRLogEventId"
            `apply` parseSchemaType "Timestamp"
            `apply` parseSchemaType "UIView"
            `apply` optional (parseSchemaType "QueryProfile")
            `apply` parseSchemaType "UserIdCode"
            `apply` parseSchemaType "UserOrganisation"
            `apply` optional (parseSchemaType "TargetItems")
    schemaTypeToXML s x@LogEvent{} =
        toXMLElement s []
            [ schemaTypeToXML "ActivityType" $ logEvent_activityType x
            , schemaTypeToXML "IRLogEventId" $ logEvent_iRLogEventId x
            , schemaTypeToXML "Timestamp" $ logEvent_timestamp x
            , schemaTypeToXML "UIView" $ logEvent_uIView x
            , maybe [] (schemaTypeToXML "QueryProfile") $ logEvent_queryProfile x
            , schemaTypeToXML "UserIdCode" $ logEvent_userIdCode x
            , schemaTypeToXML "UserOrganisation" $ logEvent_userOrganisation x
            , maybe [] (schemaTypeToXML "TargetItems") $ logEvent_targetItems x
            ]
 
data LogEvents = LogEvents
        { logEvents_logEvent :: [LogEvent]
        }
        deriving (Eq,Show)
instance SchemaType LogEvents where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return LogEvents
            `apply` many1 (parseSchemaType "LogEvent")
    schemaTypeToXML s x@LogEvents{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "LogEvent") $ logEvents_logEvent x
            ]
 
data MainSubscriptionTargetItem = MainSubscriptionTargetItem
        { mainSubscrTargetItem_mainSubscriptionId :: Irct.String40
        , mainSubscrTargetItem_iRMainSubscriptionId :: Irct.Guid
        }
        deriving (Eq,Show)
instance SchemaType MainSubscriptionTargetItem where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return MainSubscriptionTargetItem
            `apply` parseSchemaType "MainSubscriptionId"
            `apply` parseSchemaType "IRMainSubscriptionId"
    schemaTypeToXML s x@MainSubscriptionTargetItem{} =
        toXMLElement s []
            [ schemaTypeToXML "MainSubscriptionId" $ mainSubscrTargetItem_mainSubscriptionId x
            , schemaTypeToXML "IRMainSubscriptionId" $ mainSubscrTargetItem_iRMainSubscriptionId x
            ]
 
data MessageTargetItem = MessageTargetItem
        { messageTargetItem_messageId :: Irct.String40
        , messageTargetItem_iRMessageId :: Irct.Guid
        }
        deriving (Eq,Show)
instance SchemaType MessageTargetItem where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return MessageTargetItem
            `apply` parseSchemaType "MessageId"
            `apply` parseSchemaType "IRMessageId"
    schemaTypeToXML s x@MessageTargetItem{} =
        toXMLElement s []
            [ schemaTypeToXML "MessageId" $ messageTargetItem_messageId x
            , schemaTypeToXML "IRMessageId" $ messageTargetItem_iRMessageId x
            ]
 
data OtherTargetItem = OtherTargetItem
        { otherTargetItem_name :: Irct.String40
        , otherTargetItem_value :: Irct.String200
        }
        deriving (Eq,Show)
instance SchemaType OtherTargetItem where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return OtherTargetItem
            `apply` parseSchemaType "Name"
            `apply` parseSchemaType "Value"
    schemaTypeToXML s x@OtherTargetItem{} =
        toXMLElement s []
            [ schemaTypeToXML "Name" $ otherTargetItem_name x
            , schemaTypeToXML "Value" $ otherTargetItem_value x
            ]
 
data Query = Query
        { query_iRQueryId :: Irct.Guid
        , query_timestamp :: Xs.DateTime
        , query_timespanStart :: Xs.DateTime
        , query_timespanEnd :: Xs.DateTime
        }
        deriving (Eq,Show)
instance SchemaType Query where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Query
            `apply` parseSchemaType "IRQueryId"
            `apply` parseSchemaType "QueryTimestamp"
            `apply` parseSchemaType "QueryTimespanStart"
            `apply` parseSchemaType "QueryTimespanEnd"
    schemaTypeToXML s x@Query{} =
        toXMLElement s []
            [ schemaTypeToXML "IRQueryId" $ query_iRQueryId x
            , schemaTypeToXML "QueryTimestamp" $ query_timestamp x
            , schemaTypeToXML "QueryTimespanStart" $ query_timespanStart x
            , schemaTypeToXML "QueryTimespanEnd" $ query_timespanEnd x
            ]
 
data QueryTargetItem = QueryTargetItem
        { queryTargetItem_targetItemType :: TargetItemType
        , queryTargetItem_iRQueryId :: Irct.Guid
        }
        deriving (Eq,Show)
instance SchemaType QueryTargetItem where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return QueryTargetItem
            `apply` parseSchemaType "TargetItemType"
            `apply` parseSchemaType "IRQueryId"
    schemaTypeToXML s x@QueryTargetItem{} =
        toXMLElement s []
            [ schemaTypeToXML "TargetItemType" $ queryTargetItem_targetItemType x
            , schemaTypeToXML "IRQueryId" $ queryTargetItem_iRQueryId x
            ]
 
data ReportTargetItem = ReportTargetItem
        { reportTargetItem_targetItemType :: TargetItemType
        , reportTargetItem_reportId :: Irct.String40
        , reportTargetItem_iRReportId :: Irct.Guid
        , reportTargetItem_reportVersion :: Xs.Int
        }
        deriving (Eq,Show)
instance SchemaType ReportTargetItem where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return ReportTargetItem
            `apply` parseSchemaType "TargetItemType"
            `apply` parseSchemaType "ReportId"
            `apply` parseSchemaType "IRReportId"
            `apply` parseSchemaType "ReportVersion"
    schemaTypeToXML s x@ReportTargetItem{} =
        toXMLElement s []
            [ schemaTypeToXML "TargetItemType" $ reportTargetItem_targetItemType x
            , schemaTypeToXML "ReportId" $ reportTargetItem_reportId x
            , schemaTypeToXML "IRReportId" $ reportTargetItem_iRReportId x
            , schemaTypeToXML "ReportVersion" $ reportTargetItem_reportVersion x
            ]
 
data Subscription = Subscription
        { subscription_queryDataType :: Xs.Int
        , subscription_productionEnvironment :: Irct.TrueOrFalse
        , subscription_iRMainSubscriptionId :: Irct.Guid
        , subscription_iRSubscriptionId :: Irct.Guid
        , subscription_mainSubscriptionId :: Irct.String40
        , subscription_id :: Irct.String40
        }
        deriving (Eq,Show)
instance SchemaType Subscription where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Subscription
            `apply` parseSchemaType "QueryDataType"
            `apply` parseSchemaType "ProductionEnvironment"
            `apply` parseSchemaType "IRMainSubscriptionId"
            `apply` parseSchemaType "IRSubscriptionId"
            `apply` parseSchemaType "MainSubscriptionId"
            `apply` parseSchemaType "SubscriptionId"
    schemaTypeToXML s x@Subscription{} =
        toXMLElement s []
            [ schemaTypeToXML "QueryDataType" $ subscription_queryDataType x
            , schemaTypeToXML "ProductionEnvironment" $ subscription_productionEnvironment x
            , schemaTypeToXML "IRMainSubscriptionId" $ subscription_iRMainSubscriptionId x
            , schemaTypeToXML "IRSubscriptionId" $ subscription_iRSubscriptionId x
            , schemaTypeToXML "MainSubscriptionId" $ subscription_mainSubscriptionId x
            , schemaTypeToXML "SubscriptionId" $ subscription_id x
            ]
 
data Summary = Summary
        { summary_nrOfEvents :: Xs.Int
        }
        deriving (Eq,Show)
instance SchemaType Summary where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Summary
            `apply` parseSchemaType "NrOfEvents"
    schemaTypeToXML s x@Summary{} =
        toXMLElement s []
            [ schemaTypeToXML "NrOfEvents" $ summary_nrOfEvents x
            ]
 
data TargetItem = TargetItem
        { targetItem_choice0 :: (Maybe (OneOf7 (Maybe (IdCodeTargetItem)) (Maybe (ReportTargetItem)) (Maybe (MessageTargetItem)) (Maybe (DeliveryTargetItem)) (Maybe (QueryTargetItem)) (Maybe (MainSubscriptionTargetItem)) (Maybe (OtherTargetItem))))
          -- ^ Choice between:
          --   
          --   (1) IdCodeTargetItem
          --   
          --   (2) ReportTargetItem
          --   
          --   (3) MessageTargetItem
          --   
          --   (4) DeliveryTargetItem
          --   
          --   (5) QueryTargetItem
          --   
          --   (6) MainSubscriptionTargetItem
          --   
          --   (7) OtherTargetItem
        }
        deriving (Eq,Show)
instance SchemaType TargetItem where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return TargetItem
            `apply` optional (oneOf' [ ("Maybe IdCodeTargetItem", fmap OneOf7 (optional (parseSchemaType "IdCodeTargetItem")))
                                     , ("Maybe ReportTargetItem", fmap TwoOf7 (optional (parseSchemaType "ReportTargetItem")))
                                     , ("Maybe MessageTargetItem", fmap ThreeOf7 (optional (parseSchemaType "MessageTargetItem")))
                                     , ("Maybe DeliveryTargetItem", fmap FourOf7 (optional (parseSchemaType "DeliveryTargetItem")))
                                     , ("Maybe QueryTargetItem", fmap FiveOf7 (optional (parseSchemaType "QueryTargetItem")))
                                     , ("Maybe MainSubscriptionTargetItem", fmap SixOf7 (optional (parseSchemaType "MainSubscriptionTargetItem")))
                                     , ("Maybe OtherTargetItem", fmap SevenOf7 (optional (parseSchemaType "OtherTargetItem")))
                                     ])
    schemaTypeToXML s x@TargetItem{} =
        toXMLElement s []
            [ maybe [] (foldOneOf7  (maybe [] (schemaTypeToXML "IdCodeTargetItem"))
                                    (maybe [] (schemaTypeToXML "ReportTargetItem"))
                                    (maybe [] (schemaTypeToXML "MessageTargetItem"))
                                    (maybe [] (schemaTypeToXML "DeliveryTargetItem"))
                                    (maybe [] (schemaTypeToXML "QueryTargetItem"))
                                    (maybe [] (schemaTypeToXML "MainSubscriptionTargetItem"))
                                    (maybe [] (schemaTypeToXML "OtherTargetItem"))
                                   ) $ targetItem_choice0 x
            ]
 
data TargetItems = TargetItems
        { targetItems_targetItem :: [TargetItem]
        }
        deriving (Eq,Show)
instance SchemaType TargetItems where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return TargetItems
            `apply` many1 (parseSchemaType "TargetItem")
    schemaTypeToXML s x@TargetItems{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "TargetItem") $ targetItems_targetItem x
            ]
