{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.InvalidationsToIRTypes'xsd
  ( module Data.InvalidationsToIRTypes'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.IRCommonTypes'xsd as Irct
import Data.Xmldsig'core'schema'xsd as Ds
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
data DeliveryData = DeliveryData
        { deliveryData_timestamp :: Xs.DateTime
        , deliveryData_source :: Maybe Irct.String30
        , deliveryData_type :: Xs.Int
        , deliveryData_deliveryId :: Irct.String40
        , deliveryData_faultyControl :: Maybe Xs.Int
        , deliveryData_productionEnvironment :: Irct.TrueOrFalse
        , deliveryData_owner :: Id
        , deliveryData_creator :: Id
        , deliveryData_sender :: Id
        , deliveryData_items :: Items
        }
        deriving (Eq,Show)
instance SchemaType DeliveryData where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DeliveryData
            `apply` parseSchemaType "Timestamp"
            `apply` optional (parseSchemaType "Source")
            `apply` parseSchemaType "DeliveryDataType"
            `apply` parseSchemaType "DeliveryId"
            `apply` optional (parseSchemaType "FaultyControl")
            `apply` parseSchemaType "ProductionEnvironment"
            `apply` parseSchemaType "DeliveryDataOwner"
            `apply` parseSchemaType "DeliveryDataCreator"
            `apply` parseSchemaType "DeliveryDataSender"
            `apply` parseSchemaType "Items"
    schemaTypeToXML s x@DeliveryData{} =
        toXMLElement s []
            [ schemaTypeToXML "Timestamp" $ deliveryData_timestamp x
            , maybe [] (schemaTypeToXML "Source") $ deliveryData_source x
            , schemaTypeToXML "DeliveryDataType" $ deliveryData_type x
            , schemaTypeToXML "DeliveryId" $ deliveryData_deliveryId x
            , maybe [] (schemaTypeToXML "FaultyControl") $ deliveryData_faultyControl x
            , schemaTypeToXML "ProductionEnvironment" $ deliveryData_productionEnvironment x
            , schemaTypeToXML "DeliveryDataOwner" $ deliveryData_owner x
            , schemaTypeToXML "DeliveryDataCreator" $ deliveryData_creator x
            , schemaTypeToXML "DeliveryDataSender" $ deliveryData_sender x
            , schemaTypeToXML "Items" $ deliveryData_items x
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
 
data InvalidationsToIR = InvalidationsToIR
        { invalToIR_deliveryData :: DeliveryData
        , invalToIR_signature :: Maybe SignatureType
        }
        deriving (Eq,Show)
instance SchemaType InvalidationsToIR where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return InvalidationsToIR
            `apply` parseSchemaType "DeliveryData"
            `apply` optional (elementSignature)
    schemaTypeToXML s x@InvalidationsToIR{} =
        toXMLElement s []
            [ schemaTypeToXML "DeliveryData" $ invalToIR_deliveryData x
            , maybe [] (elementToXMLSignature) $ invalToIR_signature x
            ]
 
data Item = Item
        { item_iRItemId :: Maybe Irct.Guid
        , item_id :: Maybe Irct.String40
        , item_version :: Maybe Xs.Int
        }
        deriving (Eq,Show)
instance SchemaType Item where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Item
            `apply` optional (parseSchemaType "IRItemId")
            `apply` optional (parseSchemaType "ItemId")
            `apply` optional (parseSchemaType "ItemVersion")
    schemaTypeToXML s x@Item{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "IRItemId") $ item_iRItemId x
            , maybe [] (schemaTypeToXML "ItemId") $ item_id x
            , maybe [] (schemaTypeToXML "ItemVersion") $ item_version x
            ]
 
data Items = Items
        { items_item :: [Item]
        }
        deriving (Eq,Show)
instance SchemaType Items where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Items
            `apply` many1 (parseSchemaType "Item")
    schemaTypeToXML s x@Items{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Item") $ items_item x
            ]
