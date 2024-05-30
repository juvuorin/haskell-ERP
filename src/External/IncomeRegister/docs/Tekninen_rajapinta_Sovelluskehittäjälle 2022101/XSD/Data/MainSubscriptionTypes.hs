{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.MainSubscriptionTypes'xsd
  ( module Data.MainSubscriptionTypes'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.IRCommonTypes'xsd as Irct
import Data.Xmldsig'core'schema'xsd as Ds
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
newtype Day = Day DaysType deriving (Eq,Show)
instance Restricts Day DaysType where
    restricts (Day x) = x
instance SchemaType Day where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (Day x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Day where
    acceptingParser = fmap Day acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (Day x) = simpleTypeText x
 
data DaysType
    = DaysType_V1
    | DaysType_V2
    | DaysType_V3
    | DaysType_V4
    | DaysType_V5
    | DaysType_V6
    | DaysType_V7
    | DaysType_V8
    | DaysType_V9
    | DaysType_V10
    | DaysType_V11
    | DaysType_V12
    | DaysType_V13
    | DaysType_V14
    | DaysType_V15
    | DaysType_V16
    | DaysType_V17
    | DaysType_V18
    | DaysType_V19
    | DaysType_V20
    | DaysType_V21
    | DaysType_V22
    | DaysType_V23
    | DaysType_V24
    | DaysType_V25
    | DaysType_V26
    | DaysType_V27
    | DaysType_V28
    | DaysType_V29
    | DaysType_V30
    | DaysType_V31
    | DaysType_V32
    deriving (Eq,Show,Enum)
instance SchemaType DaysType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DaysType where
    acceptingParser =  do literal "1"; return DaysType_V1
                      `onFail` do literal "2"; return DaysType_V2
                      `onFail` do literal "3"; return DaysType_V3
                      `onFail` do literal "4"; return DaysType_V4
                      `onFail` do literal "5"; return DaysType_V5
                      `onFail` do literal "6"; return DaysType_V6
                      `onFail` do literal "7"; return DaysType_V7
                      `onFail` do literal "8"; return DaysType_V8
                      `onFail` do literal "9"; return DaysType_V9
                      `onFail` do literal "10"; return DaysType_V10
                      `onFail` do literal "11"; return DaysType_V11
                      `onFail` do literal "12"; return DaysType_V12
                      `onFail` do literal "13"; return DaysType_V13
                      `onFail` do literal "14"; return DaysType_V14
                      `onFail` do literal "15"; return DaysType_V15
                      `onFail` do literal "16"; return DaysType_V16
                      `onFail` do literal "17"; return DaysType_V17
                      `onFail` do literal "18"; return DaysType_V18
                      `onFail` do literal "19"; return DaysType_V19
                      `onFail` do literal "20"; return DaysType_V20
                      `onFail` do literal "21"; return DaysType_V21
                      `onFail` do literal "22"; return DaysType_V22
                      `onFail` do literal "23"; return DaysType_V23
                      `onFail` do literal "24"; return DaysType_V24
                      `onFail` do literal "25"; return DaysType_V25
                      `onFail` do literal "26"; return DaysType_V26
                      `onFail` do literal "27"; return DaysType_V27
                      `onFail` do literal "28"; return DaysType_V28
                      `onFail` do literal "29"; return DaysType_V29
                      `onFail` do literal "30"; return DaysType_V30
                      `onFail` do literal "31"; return DaysType_V31
                      `onFail` do literal "32"; return DaysType_V32
                      
    simpleTypeText DaysType_V1 = "1"
    simpleTypeText DaysType_V2 = "2"
    simpleTypeText DaysType_V3 = "3"
    simpleTypeText DaysType_V4 = "4"
    simpleTypeText DaysType_V5 = "5"
    simpleTypeText DaysType_V6 = "6"
    simpleTypeText DaysType_V7 = "7"
    simpleTypeText DaysType_V8 = "8"
    simpleTypeText DaysType_V9 = "9"
    simpleTypeText DaysType_V10 = "10"
    simpleTypeText DaysType_V11 = "11"
    simpleTypeText DaysType_V12 = "12"
    simpleTypeText DaysType_V13 = "13"
    simpleTypeText DaysType_V14 = "14"
    simpleTypeText DaysType_V15 = "15"
    simpleTypeText DaysType_V16 = "16"
    simpleTypeText DaysType_V17 = "17"
    simpleTypeText DaysType_V18 = "18"
    simpleTypeText DaysType_V19 = "19"
    simpleTypeText DaysType_V20 = "20"
    simpleTypeText DaysType_V21 = "21"
    simpleTypeText DaysType_V22 = "22"
    simpleTypeText DaysType_V23 = "23"
    simpleTypeText DaysType_V24 = "24"
    simpleTypeText DaysType_V25 = "25"
    simpleTypeText DaysType_V26 = "26"
    simpleTypeText DaysType_V27 = "27"
    simpleTypeText DaysType_V28 = "28"
    simpleTypeText DaysType_V29 = "29"
    simpleTypeText DaysType_V30 = "30"
    simpleTypeText DaysType_V31 = "31"
    simpleTypeText DaysType_V32 = "32"
 
newtype Month = Month Irct.MonthsType deriving (Eq,Show)
instance Restricts Month Irct.MonthsType where
    restricts (Month x) = x
instance SchemaType Month where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (Month x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Month where
    acceptingParser = fmap Month acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (Month x) = simpleTypeText x
 
data CodeItem = CodeItem
        { codeItem_idType :: Xs.Int
        , codeItem_idCode :: Irct.String30
        , codeItem_idCountryCode :: Maybe Irct.String2
        , codeItem_subscriptionParameters :: Maybe SubscriptionParameters
        }
        deriving (Eq,Show)
instance SchemaType CodeItem where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return CodeItem
            `apply` parseSchemaType "IdType"
            `apply` parseSchemaType "IdCode"
            `apply` optional (parseSchemaType "IdCountryCode")
            `apply` optional (parseSchemaType "SubscriptionParameters")
    schemaTypeToXML s x@CodeItem{} =
        toXMLElement s []
            [ schemaTypeToXML "IdType" $ codeItem_idType x
            , schemaTypeToXML "IdCode" $ codeItem_idCode x
            , maybe [] (schemaTypeToXML "IdCountryCode") $ codeItem_idCountryCode x
            , maybe [] (schemaTypeToXML "SubscriptionParameters") $ codeItem_subscriptionParameters x
            ]
 
data CodeItems = CodeItems
        { codeItems_codeItem :: [CodeItem]
        }
        deriving (Eq,Show)
instance SchemaType CodeItems where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return CodeItems
            `apply` many1 (parseSchemaType "CodeItem")
    schemaTypeToXML s x@CodeItems{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "CodeItem") $ codeItems_codeItem x
            ]
 
data DailySchedule = DailySchedule
        { dailySched_queryStartTime :: [QueryStartTime]
        }
        deriving (Eq,Show)
instance SchemaType DailySchedule where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DailySchedule
            `apply` many1 (parseSchemaType "QueryStartTime")
    schemaTypeToXML s x@DailySchedule{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "QueryStartTime") $ dailySched_queryStartTime x
            ]
 
data DataRequestToIR = DataRequestToIR
        { dataReqToIR_timestamp :: Xs.DateTime
        , dataReqToIR_queryDataType :: Xs.Int
        , dataReqToIR_queryProfile :: Maybe Irct.String40
        , dataReqToIR_includeAllVersions :: Maybe Irct.TrueOrFalse
        , dataReqToIR_productionEnvironment :: Irct.TrueOrFalse
        , dataReqToIR_deliveryDataOwner :: Id
        , dataReqToIR_deliveryDataCreator :: Id
        , dataReqToIR_deliveryDataSender :: Id
        , dataReqToIR_dataRequestParameters :: Maybe DataRequestParameters
        , dataReqToIR_signature :: SignatureType
        }
        deriving (Eq,Show)
instance SchemaType DataRequestToIR where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DataRequestToIR
            `apply` parseSchemaType "Timestamp"
            `apply` parseSchemaType "QueryDataType"
            `apply` optional (parseSchemaType "QueryProfile")
            `apply` optional (parseSchemaType "IncludeAllVersions")
            `apply` parseSchemaType "ProductionEnvironment"
            `apply` parseSchemaType "DeliveryDataOwner"
            `apply` parseSchemaType "DeliveryDataCreator"
            `apply` parseSchemaType "DeliveryDataSender"
            `apply` optional (parseSchemaType "DataRequestParameters")
            `apply` elementSignature
    schemaTypeToXML s x@DataRequestToIR{} =
        toXMLElement s []
            [ schemaTypeToXML "Timestamp" $ dataReqToIR_timestamp x
            , schemaTypeToXML "QueryDataType" $ dataReqToIR_queryDataType x
            , maybe [] (schemaTypeToXML "QueryProfile") $ dataReqToIR_queryProfile x
            , maybe [] (schemaTypeToXML "IncludeAllVersions") $ dataReqToIR_includeAllVersions x
            , schemaTypeToXML "ProductionEnvironment" $ dataReqToIR_productionEnvironment x
            , schemaTypeToXML "DeliveryDataOwner" $ dataReqToIR_deliveryDataOwner x
            , schemaTypeToXML "DeliveryDataCreator" $ dataReqToIR_deliveryDataCreator x
            , schemaTypeToXML "DeliveryDataSender" $ dataReqToIR_deliveryDataSender x
            , maybe [] (schemaTypeToXML "DataRequestParameters") $ dataReqToIR_dataRequestParameters x
            , elementToXMLSignature $ dataReqToIR_signature x
            ]
 
data DataRequestParameters = DataRequestParameters
        { dataReqParam_valueParameter :: [ValueParameter]
        , dataReqParam_timespanParameter :: [TimespanParameter]
        , dataReqParam_timespanTimeParameter :: [TimespanTimeParameter]
        , dataReqParam_idCodeParameter :: [IdCodeParameter]
        }
        deriving (Eq,Show)
instance SchemaType DataRequestParameters where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DataRequestParameters
            `apply` many (parseSchemaType "ValueParameter")
            `apply` many (parseSchemaType "TimespanParameter")
            `apply` many (parseSchemaType "TimespanTimeParameter")
            `apply` many (parseSchemaType "IdCodeParameter")
    schemaTypeToXML s x@DataRequestParameters{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "ValueParameter") $ dataReqParam_valueParameter x
            , concatMap (schemaTypeToXML "TimespanParameter") $ dataReqParam_timespanParameter x
            , concatMap (schemaTypeToXML "TimespanTimeParameter") $ dataReqParam_timespanTimeParameter x
            , concatMap (schemaTypeToXML "IdCodeParameter") $ dataReqParam_idCodeParameter x
            ]
 
data SubscriptionsToIR = SubscriptionsToIR
        { subscrToIR_deliveryData :: DeliveryData
        , subscrToIR_signature :: Maybe SignatureType
        }
        deriving (Eq,Show)
instance SchemaType SubscriptionsToIR where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return SubscriptionsToIR
            `apply` parseSchemaType "DeliveryData"
            `apply` optional (elementSignature)
    schemaTypeToXML s x@SubscriptionsToIR{} =
        toXMLElement s []
            [ schemaTypeToXML "DeliveryData" $ subscrToIR_deliveryData x
            , maybe [] (elementToXMLSignature) $ subscrToIR_signature x
            ]
 
data Days = Days
        { days_day :: [Day]
        }
        deriving (Eq,Show)
instance SchemaType Days where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Days
            `apply` between (Occurs (Just 1) (Just 32))
                            (parseSchemaType "Day")
    schemaTypeToXML s x@Days{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Day") $ days_day x
            ]
 
data DeliveryData = DeliveryData
        { deliveryData_timestamp :: Xs.DateTime
        , deliveryData_type :: Xs.Int
        , deliveryData_deliveryId :: Irct.String40
        , deliveryData_productionEnvironment :: Irct.TrueOrFalse
        , deliveryData_owner :: Id
        , deliveryData_creator :: Id
        , deliveryData_sender :: Id
        , deliveryData_mainSubscription :: MainSubscription
        }
        deriving (Eq,Show)
instance SchemaType DeliveryData where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DeliveryData
            `apply` parseSchemaType "Timestamp"
            `apply` parseSchemaType "DeliveryDataType"
            `apply` parseSchemaType "DeliveryId"
            `apply` parseSchemaType "ProductionEnvironment"
            `apply` parseSchemaType "DeliveryDataOwner"
            `apply` parseSchemaType "DeliveryDataCreator"
            `apply` parseSchemaType "DeliveryDataSender"
            `apply` parseSchemaType "MainSubscription"
    schemaTypeToXML s x@DeliveryData{} =
        toXMLElement s []
            [ schemaTypeToXML "Timestamp" $ deliveryData_timestamp x
            , schemaTypeToXML "DeliveryDataType" $ deliveryData_type x
            , schemaTypeToXML "DeliveryId" $ deliveryData_deliveryId x
            , schemaTypeToXML "ProductionEnvironment" $ deliveryData_productionEnvironment x
            , schemaTypeToXML "DeliveryDataOwner" $ deliveryData_owner x
            , schemaTypeToXML "DeliveryDataCreator" $ deliveryData_creator x
            , schemaTypeToXML "DeliveryDataSender" $ deliveryData_sender x
            , schemaTypeToXML "MainSubscription" $ deliveryData_mainSubscription x
            ]
 
data DeliverySchedule = DeliverySchedule
        { delivSched_queryStartTime :: [QueryStartTime]
        }
        deriving (Eq,Show)
instance SchemaType DeliverySchedule where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DeliverySchedule
            `apply` many1 (parseSchemaType "QueryStartTime")
    schemaTypeToXML s x@DeliverySchedule{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "QueryStartTime") $ delivSched_queryStartTime x
            ]
 
data Id = Id
        { id_type :: Xs.Int
        , id_code :: Irct.String30
        , id_countryCode :: Maybe Irct.String2
        , id_countryName :: Maybe Irct.String70
        }
        deriving (Eq,Show)
instance SchemaType Id where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Id
            `apply` parseSchemaType "Type"
            `apply` parseSchemaType "Code"
            `apply` optional (parseSchemaType "CountryCode")
            `apply` optional (parseSchemaType "CountryName")
    schemaTypeToXML s x@Id{} =
        toXMLElement s []
            [ schemaTypeToXML "Type" $ id_type x
            , schemaTypeToXML "Code" $ id_code x
            , maybe [] (schemaTypeToXML "CountryCode") $ id_countryCode x
            , maybe [] (schemaTypeToXML "CountryName") $ id_countryName x
            ]
 
data IdCodeList = IdCodeList
        { idCodeList_codeListType :: Xs.Int
        , idCodeList_codeItems :: CodeItems
        }
        deriving (Eq,Show)
instance SchemaType IdCodeList where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return IdCodeList
            `apply` parseSchemaType "CodeListType"
            `apply` parseSchemaType "CodeItems"
    schemaTypeToXML s x@IdCodeList{} =
        toXMLElement s []
            [ schemaTypeToXML "CodeListType" $ idCodeList_codeListType x
            , schemaTypeToXML "CodeItems" $ idCodeList_codeItems x
            ]
 
data IdCodeParameter = IdCodeParameter
        { idCodeParam_parameterType :: Xs.Int
        , idCodeParam_idType :: Xs.Int
        , idCodeParam_idCode :: Irct.String30
        , idCodeParam_idCountryCode :: Maybe Irct.String2
        }
        deriving (Eq,Show)
instance SchemaType IdCodeParameter where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return IdCodeParameter
            `apply` parseSchemaType "ParameterType"
            `apply` parseSchemaType "IdType"
            `apply` parseSchemaType "IdCode"
            `apply` optional (parseSchemaType "IdCountryCode")
    schemaTypeToXML s x@IdCodeParameter{} =
        toXMLElement s []
            [ schemaTypeToXML "ParameterType" $ idCodeParam_parameterType x
            , schemaTypeToXML "IdType" $ idCodeParam_idType x
            , schemaTypeToXML "IdCode" $ idCodeParam_idCode x
            , maybe [] (schemaTypeToXML "IdCountryCode") $ idCodeParam_idCountryCode x
            ]
 
data MainSubscription = MainSubscription
        { mainSubscr_id :: Irct.String40
        , mainSubscr_deliveryChannelCode :: Xs.Int
        , mainSubscr_subscriptionType :: Xs.Int
        , mainSubscr_validFrom :: Xsd.Date
        , mainSubscr_validUntil :: Maybe Xsd.Date
        , mainSubscr_modifiedTimespanStart :: Maybe Xs.DateTime
        , mainSubscr_modifiedTimespanEnd :: Maybe Xs.DateTime
        , mainSubscr_partyType :: Maybe Xs.Int
        , mainSubscr_subscriptions :: Subscriptions
        , mainSubscr_schedule :: Schedule
        }
        deriving (Eq,Show)
instance SchemaType MainSubscription where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return MainSubscription
            `apply` parseSchemaType "MainSubscriptionId"
            `apply` parseSchemaType "DeliveryChannelCode"
            `apply` parseSchemaType "SubscriptionType"
            `apply` parseSchemaType "ValidFrom"
            `apply` optional (parseSchemaType "ValidUntil")
            `apply` optional (parseSchemaType "ModifiedTimespanStart")
            `apply` optional (parseSchemaType "ModifiedTimespanEnd")
            `apply` optional (parseSchemaType "PartyType")
            `apply` parseSchemaType "Subscriptions"
            `apply` parseSchemaType "Schedule"
    schemaTypeToXML s x@MainSubscription{} =
        toXMLElement s []
            [ schemaTypeToXML "MainSubscriptionId" $ mainSubscr_id x
            , schemaTypeToXML "DeliveryChannelCode" $ mainSubscr_deliveryChannelCode x
            , schemaTypeToXML "SubscriptionType" $ mainSubscr_subscriptionType x
            , schemaTypeToXML "ValidFrom" $ mainSubscr_validFrom x
            , maybe [] (schemaTypeToXML "ValidUntil") $ mainSubscr_validUntil x
            , maybe [] (schemaTypeToXML "ModifiedTimespanStart") $ mainSubscr_modifiedTimespanStart x
            , maybe [] (schemaTypeToXML "ModifiedTimespanEnd") $ mainSubscr_modifiedTimespanEnd x
            , maybe [] (schemaTypeToXML "PartyType") $ mainSubscr_partyType x
            , schemaTypeToXML "Subscriptions" $ mainSubscr_subscriptions x
            , schemaTypeToXML "Schedule" $ mainSubscr_schedule x
            ]
 
data MonthlySchedule = MonthlySchedule
        { monthlySched_queryStartTime :: [QueryStartTime]
        , monthlySched_days :: Days
        , monthlySched_months :: Months
        }
        deriving (Eq,Show)
instance SchemaType MonthlySchedule where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return MonthlySchedule
            `apply` many1 (parseSchemaType "QueryStartTime")
            `apply` parseSchemaType "Days"
            `apply` parseSchemaType "Months"
    schemaTypeToXML s x@MonthlySchedule{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "QueryStartTime") $ monthlySched_queryStartTime x
            , schemaTypeToXML "Days" $ monthlySched_days x
            , schemaTypeToXML "Months" $ monthlySched_months x
            ]
instance Extension MonthlySchedule DeliverySchedule where
    supertype (MonthlySchedule e0 e1 e2) =
               DeliverySchedule e0
 
data Months = Months
        { months_month :: [Month]
        }
        deriving (Eq,Show)
instance SchemaType Months where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Months
            `apply` between (Occurs (Just 1) (Just 12))
                            (parseSchemaType "Month")
    schemaTypeToXML s x@Months{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Month") $ months_month x
            ]
 
data OnetimeDeliverySchedule = OnetimeDeliverySchedule
        { onetimeDelivSched_time :: Xs.Time
        }
        deriving (Eq,Show)
instance SchemaType OnetimeDeliverySchedule where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return OnetimeDeliverySchedule
            `apply` parseSchemaType "Time"
    schemaTypeToXML s x@OnetimeDeliverySchedule{} =
        toXMLElement s []
            [ schemaTypeToXML "Time" $ onetimeDelivSched_time x
            ]
 
data QueryStartTime = QueryStartTime
        { queryStartTime_time :: Xs.Time
        }
        deriving (Eq,Show)
instance SchemaType QueryStartTime where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return QueryStartTime
            `apply` parseSchemaType "Time"
    schemaTypeToXML s x@QueryStartTime{} =
        toXMLElement s []
            [ schemaTypeToXML "Time" $ queryStartTime_time x
            ]
 
data RecurringDeliverySchedule = RecurringDeliverySchedule
        { recurrDelivSched_choice0 :: OneOf3 MonthlySchedule WeeklySchedule DailySchedule
          -- ^ Choice between:
          --   
          --   (1) MonthlySchedule
          --   
          --   (2) WeeklySchedule
          --   
          --   (3) DailySchedule
        }
        deriving (Eq,Show)
instance SchemaType RecurringDeliverySchedule where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return RecurringDeliverySchedule
            `apply` oneOf' [ ("MonthlySchedule", fmap OneOf3 (parseSchemaType "MonthlySchedule"))
                           , ("WeeklySchedule", fmap TwoOf3 (parseSchemaType "WeeklySchedule"))
                           , ("DailySchedule", fmap ThreeOf3 (parseSchemaType "DailySchedule"))
                           ]
    schemaTypeToXML s x@RecurringDeliverySchedule{} =
        toXMLElement s []
            [ foldOneOf3  (schemaTypeToXML "MonthlySchedule")
                          (schemaTypeToXML "WeeklySchedule")
                          (schemaTypeToXML "DailySchedule")
                          $ recurrDelivSched_choice0 x
            ]
 
data Schedule = Schedule
        { schedule_choice0 :: OneOf2 RecurringDeliverySchedule OnetimeDeliverySchedule
          -- ^ Choice between:
          --   
          --   (1) RecurringDeliverySchedule
          --   
          --   (2) OnetimeDeliverySchedule
        }
        deriving (Eq,Show)
instance SchemaType Schedule where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Schedule
            `apply` oneOf' [ ("RecurringDeliverySchedule", fmap OneOf2 (parseSchemaType "RecurringDeliverySchedule"))
                           , ("OnetimeDeliverySchedule", fmap TwoOf2 (parseSchemaType "OnetimeDeliverySchedule"))
                           ]
    schemaTypeToXML s x@Schedule{} =
        toXMLElement s []
            [ foldOneOf2  (schemaTypeToXML "RecurringDeliverySchedule")
                          (schemaTypeToXML "OnetimeDeliverySchedule")
                          $ schedule_choice0 x
            ]
 
data Subscription = Subscription
        { subscription_id :: Irct.String40
        , subscription_queryDataType :: Xs.Int
        , subscription_queryProfile :: Maybe Irct.String40
        , subscription_includeAllVersions :: Maybe Irct.TrueOrFalse
        , subscription_queryDataSchemaVersion :: Maybe Irct.String200
        , subscription_parameters :: Maybe SubscriptionParameters
        , subscription_idCodeList :: [IdCodeList]
        }
        deriving (Eq,Show)
instance SchemaType Subscription where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Subscription
            `apply` parseSchemaType "SubscriptionId"
            `apply` parseSchemaType "QueryDataType"
            `apply` optional (parseSchemaType "QueryProfile")
            `apply` optional (parseSchemaType "IncludeAllVersions")
            `apply` optional (parseSchemaType "QueryDataSchemaVersion")
            `apply` optional (parseSchemaType "SubscriptionParameters")
            `apply` many (parseSchemaType "IdCodeList")
    schemaTypeToXML s x@Subscription{} =
        toXMLElement s []
            [ schemaTypeToXML "SubscriptionId" $ subscription_id x
            , schemaTypeToXML "QueryDataType" $ subscription_queryDataType x
            , maybe [] (schemaTypeToXML "QueryProfile") $ subscription_queryProfile x
            , maybe [] (schemaTypeToXML "IncludeAllVersions") $ subscription_includeAllVersions x
            , maybe [] (schemaTypeToXML "QueryDataSchemaVersion") $ subscription_queryDataSchemaVersion x
            , maybe [] (schemaTypeToXML "SubscriptionParameters") $ subscription_parameters x
            , concatMap (schemaTypeToXML "IdCodeList") $ subscription_idCodeList x
            ]
 
data SubscriptionParameters = SubscriptionParameters
        { subscrParam_timespanParameter :: [TimespanParameter]
        , subscrParam_idCodeParameter :: [IdCodeParameter]
        , subscrParam_valueParameter :: [ValueParameter]
        }
        deriving (Eq,Show)
instance SchemaType SubscriptionParameters where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return SubscriptionParameters
            `apply` many (parseSchemaType "TimespanParameter")
            `apply` many (parseSchemaType "IdCodeParameter")
            `apply` many (parseSchemaType "ValueParameter")
    schemaTypeToXML s x@SubscriptionParameters{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "TimespanParameter") $ subscrParam_timespanParameter x
            , concatMap (schemaTypeToXML "IdCodeParameter") $ subscrParam_idCodeParameter x
            , concatMap (schemaTypeToXML "ValueParameter") $ subscrParam_valueParameter x
            ]
 
data Subscriptions = Subscriptions
        { subscr_subscription :: [Subscription]
        }
        deriving (Eq,Show)
instance SchemaType Subscriptions where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Subscriptions
            `apply` many1 (parseSchemaType "Subscription")
    schemaTypeToXML s x@Subscriptions{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Subscription") $ subscr_subscription x
            ]
 
data TimespanParameter = TimespanParameter
        { timespParam_parameterType :: Xs.Int
        , timespParam_startDate :: Xsd.Date
        , timespParam_endDate :: Xsd.Date
        }
        deriving (Eq,Show)
instance SchemaType TimespanParameter where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return TimespanParameter
            `apply` parseSchemaType "ParameterType"
            `apply` parseSchemaType "StartDate"
            `apply` parseSchemaType "EndDate"
    schemaTypeToXML s x@TimespanParameter{} =
        toXMLElement s []
            [ schemaTypeToXML "ParameterType" $ timespParam_parameterType x
            , schemaTypeToXML "StartDate" $ timespParam_startDate x
            , schemaTypeToXML "EndDate" $ timespParam_endDate x
            ]
 
data TimespanTimeParameter = TimespanTimeParameter
        { timespTimeParam_parameterType :: Xs.Int
        , timespTimeParam_startDate :: Xs.DateTime
        , timespTimeParam_endDate :: Xs.DateTime
        }
        deriving (Eq,Show)
instance SchemaType TimespanTimeParameter where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return TimespanTimeParameter
            `apply` parseSchemaType "ParameterType"
            `apply` parseSchemaType "StartDate"
            `apply` parseSchemaType "EndDate"
    schemaTypeToXML s x@TimespanTimeParameter{} =
        toXMLElement s []
            [ schemaTypeToXML "ParameterType" $ timespTimeParam_parameterType x
            , schemaTypeToXML "StartDate" $ timespTimeParam_startDate x
            , schemaTypeToXML "EndDate" $ timespTimeParam_endDate x
            ]
 
data ValueParameter = ValueParameter
        { valueParam_parameterType :: Xs.Int
        , valueParam_value :: Irct.String100
        }
        deriving (Eq,Show)
instance SchemaType ValueParameter where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return ValueParameter
            `apply` parseSchemaType "ParameterType"
            `apply` parseSchemaType "Value"
    schemaTypeToXML s x@ValueParameter{} =
        toXMLElement s []
            [ schemaTypeToXML "ParameterType" $ valueParam_parameterType x
            , schemaTypeToXML "Value" $ valueParam_value x
            ]
 
data WeeklySchedule = WeeklySchedule
        { weeklySched_queryStartTime :: [QueryStartTime]
        , weeklySched_monday :: Irct.TrueOrFalse
        , weeklySched_tuesday :: Irct.TrueOrFalse
        , weeklySched_wednesday :: Irct.TrueOrFalse
        , weeklySched_thursday :: Irct.TrueOrFalse
        , weeklySched_friday :: Irct.TrueOrFalse
        , weeklySched_saturday :: Irct.TrueOrFalse
        , weeklySched_sunday :: Irct.TrueOrFalse
        }
        deriving (Eq,Show)
instance SchemaType WeeklySchedule where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return WeeklySchedule
            `apply` many1 (parseSchemaType "QueryStartTime")
            `apply` parseSchemaType "Monday"
            `apply` parseSchemaType "Tuesday"
            `apply` parseSchemaType "Wednesday"
            `apply` parseSchemaType "Thursday"
            `apply` parseSchemaType "Friday"
            `apply` parseSchemaType "Saturday"
            `apply` parseSchemaType "Sunday"
    schemaTypeToXML s x@WeeklySchedule{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "QueryStartTime") $ weeklySched_queryStartTime x
            , schemaTypeToXML "Monday" $ weeklySched_monday x
            , schemaTypeToXML "Tuesday" $ weeklySched_tuesday x
            , schemaTypeToXML "Wednesday" $ weeklySched_wednesday x
            , schemaTypeToXML "Thursday" $ weeklySched_thursday x
            , schemaTypeToXML "Friday" $ weeklySched_friday x
            , schemaTypeToXML "Saturday" $ weeklySched_saturday x
            , schemaTypeToXML "Sunday" $ weeklySched_sunday x
            ]
instance Extension WeeklySchedule DeliverySchedule where
    supertype (WeeklySchedule e0 e1 e2 e3 e4 e5 e6 e7) =
               DeliverySchedule e0
