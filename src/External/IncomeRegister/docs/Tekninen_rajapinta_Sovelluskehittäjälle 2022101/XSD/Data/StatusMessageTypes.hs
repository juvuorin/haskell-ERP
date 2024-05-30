{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.StatusMessageTypes'xsd
  ( module Data.StatusMessageTypes'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.IRCommonTypes'xsd as Irct
import Data.Xmldsig'core'schema'xsd as Ds
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
data AckData = AckData
        { ackData_iRResponseId :: Irct.Guid
        , ackData_iRResponseTimestamp :: Xs.DateTime
        , ackData_deliveryDataStatus :: Xs.Int
        , ackData_iRDeliveryId :: Maybe Irct.Guid
        , ackData_messageErrors :: Maybe MessageErrors
        , ackData_deliveryErrors :: Maybe DeliveryErrors
        }
        deriving (Eq,Show)
instance SchemaType AckData where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return AckData
            `apply` parseSchemaType "IRResponseId"
            `apply` parseSchemaType "IRResponseTimestamp"
            `apply` parseSchemaType "DeliveryDataStatus"
            `apply` optional (parseSchemaType "IRDeliveryId")
            `apply` optional (parseSchemaType "MessageErrors")
            `apply` optional (parseSchemaType "DeliveryErrors")
    schemaTypeToXML s x@AckData{} =
        toXMLElement s []
            [ schemaTypeToXML "IRResponseId" $ ackData_iRResponseId x
            , schemaTypeToXML "IRResponseTimestamp" $ ackData_iRResponseTimestamp x
            , schemaTypeToXML "DeliveryDataStatus" $ ackData_deliveryDataStatus x
            , maybe [] (schemaTypeToXML "IRDeliveryId") $ ackData_iRDeliveryId x
            , maybe [] (schemaTypeToXML "MessageErrors") $ ackData_messageErrors x
            , maybe [] (schemaTypeToXML "DeliveryErrors") $ ackData_deliveryErrors x
            ]
instance Extension AckData Message where
    supertype (AckData e0 e1 e2 e3 e4 e5) =
               Message e0 e1
 
data AckFromIR = AckFromIR
        { ackFromIR_deliveryData :: DeliveryData
        , ackFromIR_ackData :: AckData
        , ackFromIR_signature :: SignatureType
        }
        deriving (Eq,Show)
instance SchemaType AckFromIR where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return AckFromIR
            `apply` parseSchemaType "DeliveryData"
            `apply` parseSchemaType "AckData"
            `apply` elementSignature
    schemaTypeToXML s x@AckFromIR{} =
        toXMLElement s []
            [ schemaTypeToXML "DeliveryData" $ ackFromIR_deliveryData x
            , schemaTypeToXML "AckData" $ ackFromIR_ackData x
            , elementToXMLSignature $ ackFromIR_signature x
            ]
 
data DeliveryData = DeliveryData
        { deliveryData_timestamp :: Xs.DateTime
        , deliveryData_source :: Maybe Irct.String30
        , deliveryData_type :: Xs.Int
        , deliveryData_deliveryId :: Maybe Irct.String40
        , deliveryData_faultyControl :: Maybe Xs.Int
        , deliveryData_productionEnvironment :: Irct.TrueOrFalse
        , deliveryData_owner :: Id
        , deliveryData_creator :: Id
        , deliveryData_sender :: Id
        }
        deriving (Eq,Show)
instance SchemaType DeliveryData where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DeliveryData
            `apply` parseSchemaType "Timestamp"
            `apply` optional (parseSchemaType "Source")
            `apply` parseSchemaType "DeliveryDataType"
            `apply` optional (parseSchemaType "DeliveryId")
            `apply` optional (parseSchemaType "FaultyControl")
            `apply` parseSchemaType "ProductionEnvironment"
            `apply` parseSchemaType "DeliveryDataOwner"
            `apply` parseSchemaType "DeliveryDataCreator"
            `apply` parseSchemaType "DeliveryDataSender"
    schemaTypeToXML s x@DeliveryData{} =
        toXMLElement s []
            [ schemaTypeToXML "Timestamp" $ deliveryData_timestamp x
            , maybe [] (schemaTypeToXML "Source") $ deliveryData_source x
            , schemaTypeToXML "DeliveryDataType" $ deliveryData_type x
            , maybe [] (schemaTypeToXML "DeliveryId") $ deliveryData_deliveryId x
            , maybe [] (schemaTypeToXML "FaultyControl") $ deliveryData_faultyControl x
            , schemaTypeToXML "ProductionEnvironment" $ deliveryData_productionEnvironment x
            , schemaTypeToXML "DeliveryDataOwner" $ deliveryData_owner x
            , schemaTypeToXML "DeliveryDataCreator" $ deliveryData_creator x
            , schemaTypeToXML "DeliveryDataSender" $ deliveryData_sender x
            ]
 
data DeliveryErrors = DeliveryErrors
        { delivErrors_errorInfo :: [ErrorInfo]
        }
        deriving (Eq,Show)
instance SchemaType DeliveryErrors where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DeliveryErrors
            `apply` many1 (parseSchemaType "ErrorInfo")
    schemaTypeToXML s x@DeliveryErrors{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "ErrorInfo") $ delivErrors_errorInfo x
            ]
 
data ErrorInfo = ErrorInfo
        { errorInfo_errorCode :: Irct.String20
        , errorInfo_errorMessage :: Irct.String500
        , errorInfo_errorDetails :: Maybe Irct.String500
        }
        deriving (Eq,Show)
instance SchemaType ErrorInfo where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return ErrorInfo
            `apply` parseSchemaType "ErrorCode"
            `apply` parseSchemaType "ErrorMessage"
            `apply` optional (parseSchemaType "ErrorDetails")
    schemaTypeToXML s x@ErrorInfo{} =
        toXMLElement s []
            [ schemaTypeToXML "ErrorCode" $ errorInfo_errorCode x
            , schemaTypeToXML "ErrorMessage" $ errorInfo_errorMessage x
            , maybe [] (schemaTypeToXML "ErrorDetails") $ errorInfo_errorDetails x
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
 
data InvalidItems = InvalidItems
        { invalidItems_item :: [Item]
        }
        deriving (Eq,Show)
instance SchemaType InvalidItems where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return InvalidItems
            `apply` many1 (parseSchemaType "Item")
    schemaTypeToXML s x@InvalidItems{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Item") $ invalidItems_item x
            ]
 
data Item = Item
        { item_id :: Maybe Irct.String40
        , item_iRItemId :: Maybe Irct.Guid
        , item_version :: Maybe Xs.Int
        , item_errors :: Maybe ItemErrors
        }
        deriving (Eq,Show)
instance SchemaType Item where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Item
            `apply` optional (parseSchemaType "ItemId")
            `apply` optional (parseSchemaType "IRItemId")
            `apply` optional (parseSchemaType "ItemVersion")
            `apply` optional (parseSchemaType "ItemErrors")
    schemaTypeToXML s x@Item{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "ItemId") $ item_id x
            , maybe [] (schemaTypeToXML "IRItemId") $ item_iRItemId x
            , maybe [] (schemaTypeToXML "ItemVersion") $ item_version x
            , maybe [] (schemaTypeToXML "ItemErrors") $ item_errors x
            ]
 
data ItemErrors = ItemErrors
        { itemErrors_errorInfo :: [ErrorInfo]
        }
        deriving (Eq,Show)
instance SchemaType ItemErrors where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return ItemErrors
            `apply` many1 (parseSchemaType "ErrorInfo")
    schemaTypeToXML s x@ItemErrors{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "ErrorInfo") $ itemErrors_errorInfo x
            ]
 
data Message = Message
        { message_iRResponseId :: Irct.Guid
        , message_iRResponseTimestamp :: Xs.DateTime
        }
        deriving (Eq,Show)
instance SchemaType Message where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Message
            `apply` parseSchemaType "IRResponseId"
            `apply` parseSchemaType "IRResponseTimestamp"
    schemaTypeToXML s x@Message{} =
        toXMLElement s []
            [ schemaTypeToXML "IRResponseId" $ message_iRResponseId x
            , schemaTypeToXML "IRResponseTimestamp" $ message_iRResponseTimestamp x
            ]
 
data MessageErrors = MessageErrors
        { messageErrors_errorInfo :: [ErrorInfo]
        }
        deriving (Eq,Show)
instance SchemaType MessageErrors where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return MessageErrors
            `apply` many1 (parseSchemaType "ErrorInfo")
    schemaTypeToXML s x@MessageErrors{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "ErrorInfo") $ messageErrors_errorInfo x
            ]
 
data StatusRequestToIR = StatusRequestToIR
        { statusReqToIR_timestamp :: Xs.DateTime
        , statusReqToIR_deliveryDataType :: Xs.Int
        , statusReqToIR_deliveryId :: Maybe Irct.String40
        , statusReqToIR_iRDeliveryId :: Maybe Irct.Guid
        , statusReqToIR_productionEnvironment :: Irct.TrueOrFalse
        , statusReqToIR_deliveryDataOwner :: Id
        , statusReqToIR_deliveryDataCreator :: Id
        , statusReqToIR_deliveryDataSender :: Id
        , statusReqToIR_signature :: SignatureType
        }
        deriving (Eq,Show)
instance SchemaType StatusRequestToIR where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return StatusRequestToIR
            `apply` parseSchemaType "Timestamp"
            `apply` parseSchemaType "DeliveryDataType"
            `apply` optional (parseSchemaType "DeliveryId")
            `apply` optional (parseSchemaType "IRDeliveryId")
            `apply` parseSchemaType "ProductionEnvironment"
            `apply` parseSchemaType "DeliveryDataOwner"
            `apply` parseSchemaType "DeliveryDataCreator"
            `apply` parseSchemaType "DeliveryDataSender"
            `apply` elementSignature
    schemaTypeToXML s x@StatusRequestToIR{} =
        toXMLElement s []
            [ schemaTypeToXML "Timestamp" $ statusReqToIR_timestamp x
            , schemaTypeToXML "DeliveryDataType" $ statusReqToIR_deliveryDataType x
            , maybe [] (schemaTypeToXML "DeliveryId") $ statusReqToIR_deliveryId x
            , maybe [] (schemaTypeToXML "IRDeliveryId") $ statusReqToIR_iRDeliveryId x
            , schemaTypeToXML "ProductionEnvironment" $ statusReqToIR_productionEnvironment x
            , schemaTypeToXML "DeliveryDataOwner" $ statusReqToIR_deliveryDataOwner x
            , schemaTypeToXML "DeliveryDataCreator" $ statusReqToIR_deliveryDataCreator x
            , schemaTypeToXML "DeliveryDataSender" $ statusReqToIR_deliveryDataSender x
            , elementToXMLSignature $ statusReqToIR_signature x
            ]
 
data StatusResponse = StatusResponse
        { statusRespon_iRResponseId :: Irct.Guid
        , statusRespon_iRResponseTimestamp :: Xs.DateTime
        , statusRespon_deliveryDataStatus :: Xs.Int
        , statusRespon_iRDeliveryId :: Maybe Irct.Guid
        , statusRespon_validItems :: Maybe ValidItems
        , statusRespon_invalidItems :: Maybe InvalidItems
        , statusRespon_messageErrors :: Maybe MessageErrors
        , statusRespon_deliveryErrors :: Maybe DeliveryErrors
        }
        deriving (Eq,Show)
instance SchemaType StatusResponse where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return StatusResponse
            `apply` parseSchemaType "IRResponseId"
            `apply` parseSchemaType "IRResponseTimestamp"
            `apply` parseSchemaType "DeliveryDataStatus"
            `apply` optional (parseSchemaType "IRDeliveryId")
            `apply` optional (parseSchemaType "ValidItems")
            `apply` optional (parseSchemaType "InvalidItems")
            `apply` optional (parseSchemaType "MessageErrors")
            `apply` optional (parseSchemaType "DeliveryErrors")
    schemaTypeToXML s x@StatusResponse{} =
        toXMLElement s []
            [ schemaTypeToXML "IRResponseId" $ statusRespon_iRResponseId x
            , schemaTypeToXML "IRResponseTimestamp" $ statusRespon_iRResponseTimestamp x
            , schemaTypeToXML "DeliveryDataStatus" $ statusRespon_deliveryDataStatus x
            , maybe [] (schemaTypeToXML "IRDeliveryId") $ statusRespon_iRDeliveryId x
            , maybe [] (schemaTypeToXML "ValidItems") $ statusRespon_validItems x
            , maybe [] (schemaTypeToXML "InvalidItems") $ statusRespon_invalidItems x
            , maybe [] (schemaTypeToXML "MessageErrors") $ statusRespon_messageErrors x
            , maybe [] (schemaTypeToXML "DeliveryErrors") $ statusRespon_deliveryErrors x
            ]
instance Extension StatusResponse Message where
    supertype (StatusResponse e0 e1 e2 e3 e4 e5 e6 e7) =
               Message e0 e1
 
data StatusResponseFromIR = StatusResponseFromIR
        { statusResponFromIR_deliveryData :: Maybe DeliveryData
        , statusResponFromIR_statusResponse :: StatusResponse
        , statusResponFromIR_signature :: SignatureType
        }
        deriving (Eq,Show)
instance SchemaType StatusResponseFromIR where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return StatusResponseFromIR
            `apply` optional (parseSchemaType "DeliveryData")
            `apply` parseSchemaType "StatusResponse"
            `apply` elementSignature
    schemaTypeToXML s x@StatusResponseFromIR{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "DeliveryData") $ statusResponFromIR_deliveryData x
            , schemaTypeToXML "StatusResponse" $ statusResponFromIR_statusResponse x
            , elementToXMLSignature $ statusResponFromIR_signature x
            ]
 
data ValidItems = ValidItems
        { validItems_item :: [Item]
        }
        deriving (Eq,Show)
instance SchemaType ValidItems where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return ValidItems
            `apply` many1 (parseSchemaType "Item")
    schemaTypeToXML s x@ValidItems{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Item") $ validItems_item x
            ]
