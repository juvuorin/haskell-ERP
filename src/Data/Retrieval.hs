{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.Retrieval
  ( module Data.Retrieval
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema hiding (Result)
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
data LanguageType
    = LanguageType_Fi
    | LanguageType_Sv
    | LanguageType_En
    deriving (Eq,Show,Enum)
instance SchemaType LanguageType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType LanguageType where
    acceptingParser =  do literal "fi"; return LanguageType_Fi
                      `onFail` do literal "sv"; return LanguageType_Sv
                      `onFail` do literal "en"; return LanguageType_En
                      
    simpleTypeText LanguageType_Fi = "fi"
    simpleTypeText LanguageType_Sv = "sv"
    simpleTypeText LanguageType_En = "en"
 
data StatusType
    = StatusType_Ok
    | StatusType_Wait
    | StatusType_Error
    deriving (Eq,Show,Enum)
instance SchemaType StatusType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType StatusType where
    acceptingParser =  do literal "Ok"; return StatusType_Ok
                      `onFail` do literal "Wait"; return StatusType_Wait
                      `onFail` do literal "Error"; return StatusType_Error
                      
    simpleTypeText StatusType_Ok = "Ok"
    simpleTypeText StatusType_Wait = "Wait"
    simpleTypeText StatusType_Error = "Error"
 
data Result = Result
        { result_accepted :: Xs.Boolean
          -- ^ Elementissä kerrotaan onko ilmoitusten vastaanotto 
          --   onnistunut (true) vai epäonnistunut (false).
        , result_information :: Maybe Xs.XsdString
          -- ^ Elementissä kerrotaan syy mahdolliseen ilmoitusten 
          --   hylkäämiseen.
        }
        deriving (Eq,Show)
instance SchemaType Result where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Result
            `apply` parseSchemaType "Accepted"
            `apply` optional (parseSchemaType "Information")
    schemaTypeToXML s x@Result{} =
        toXMLElement s []
            [ schemaTypeToXML "Accepted" $ result_accepted x
            , maybe [] (schemaTypeToXML "Information") $ result_information x
            ]
 
data DeliveryDataRetrievalRequest = DeliveryDataRetrievalRequest
        { deliveryDataRetrievalRequest_language :: LanguageType
          -- ^ Vastaussanomassa käytettävä kieli (fi=suomi, sv=ruotsi, 
          --   en=englanti).
        , deliveryDataRetrievalRequest_retrievalId :: Maybe Xs.XsdString
          -- ^ Elementin sisällä lähetetään kyselyä lähetettäessä saatu 
          --   noutotunniste.
        , deliveryDataRetrievalRequest_resultId :: Maybe Xs.XsdString
          -- ^ Elementin sisällä lähetetään kyselyä lähetettäessä saatu 
          --   tulostunniste taustakäsittelyyn jätetyn aineiston 
          --   tarkastustuloksesta.
        }
        deriving (Eq,Show)
instance SchemaType DeliveryDataRetrievalRequest where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DeliveryDataRetrievalRequest
            `apply` parseSchemaType "Language"
            `apply` optional (parseSchemaType "RetrievalId")
            `apply` optional (parseSchemaType "ResultId")
    schemaTypeToXML s x@DeliveryDataRetrievalRequest{} =
        toXMLElement s []
            [ schemaTypeToXML "Language" $ deliveryDataRetrievalRequest_language x
            , maybe [] (schemaTypeToXML "RetrievalId") $ deliveryDataRetrievalRequest_retrievalId x
            , maybe [] (schemaTypeToXML "ResultId") $ deliveryDataRetrievalRequest_resultId x
            ]
 
elementDeliveryDataRetrievalRequest :: XMLParser DeliveryDataRetrievalRequest
elementDeliveryDataRetrievalRequest = parseSchemaType "DeliveryDataRetrievalRequest"
elementToXMLDeliveryDataRetrievalRequest :: DeliveryDataRetrievalRequest -> [Content ()]
elementToXMLDeliveryDataRetrievalRequest = schemaTypeToXML "DeliveryDataRetrievalRequest"
 
data DeliveryDataRetrievalResponse = DeliveryDataRetrievalResponse
        { deliveryDataRetrievalResponse_filing :: Maybe Xs.XsdString
          -- ^ Elementti sisltää lähetetyn aineiston nimen.
        , deliveryDataRetrievalResponse_timestamp :: Xs.DateTime
          -- ^ Elementin sisällä on noudon aikaleiman. Esim. 
          --   2018-10-13T11:05:15.522+03:00
        , deliveryDataRetrievalResponse_status :: StatusType
          -- ^ Elementissä kerrotaan onko noutopyyntö onnistunut (Ok), 
          --   aineisto tai tarkastustulos ole ei vielä noudettavissa 
          --   (Wait) tai epäonnistunut (Error).
        , deliveryDataRetrievalResponse_information :: Xs.XsdString
          -- ^ Elementti on joko tyhjä tai sislätää esim. tiedon 'Aineisto 
          --   ei vielä noudettavissa' tai 'Tuntematon noutotunniste'.
        , deliveryDataRetrievalResponse_retrievalId :: Maybe Xs.XsdString
          -- ^ Elementti kertoo käytetyn ilmoitusaineiston 
          --   noutotunnisteen.
        , deliveryDataRetrievalResponse_resultId :: Maybe Xs.XsdString
          -- ^ Elementti kertoo käytetyn taustakäsittelyyn jätetyn 
          --   ilmoitusaineiston tarkastustuloksen tulostunnisteen.
        , deliveryDataRetrievalResponse_result :: Maybe Result
          -- ^ Taustakäsittelyyn jätetyn aineiston tarkastustuloksen tila.
        , deliveryDataRetrievalResponse_deliveryData :: Maybe Xs.Base64Binary
          -- ^ Elementin sisällä on lähetyserässä olleet ilmoitukset 
          --   täydennettynä Verohallinnon vastaustiedoilla tai 
          --   taustakäsittelyyn jätetyn aineiston tarkastustuloksen 
          --   MTOM/XOP-liitetiedostona.
        }
        deriving (Eq,Show)
instance SchemaType DeliveryDataRetrievalResponse where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DeliveryDataRetrievalResponse
            `apply` optional (parseSchemaType "Filing")
            `apply` parseSchemaType "Timestamp"
            `apply` parseSchemaType "Status"
            `apply` parseSchemaType "Information"
            `apply` optional (parseSchemaType "RetrievalId")
            `apply` optional (parseSchemaType "ResultId")
            `apply` optional (parseSchemaType "Result")
            `apply` optional (parseSchemaType "DeliveryData")
    schemaTypeToXML s x@DeliveryDataRetrievalResponse{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "Filing") $ deliveryDataRetrievalResponse_filing x
            , schemaTypeToXML "Timestamp" $ deliveryDataRetrievalResponse_timestamp x
            , schemaTypeToXML "Status" $ deliveryDataRetrievalResponse_status x
            , schemaTypeToXML "Information" $ deliveryDataRetrievalResponse_information x
            , maybe [] (schemaTypeToXML "RetrievalId") $ deliveryDataRetrievalResponse_retrievalId x
            , maybe [] (schemaTypeToXML "ResultId") $ deliveryDataRetrievalResponse_resultId x
            , maybe [] (schemaTypeToXML "Result") $ deliveryDataRetrievalResponse_result x
            , maybe [] (schemaTypeToXML "DeliveryData") $ deliveryDataRetrievalResponse_deliveryData x
            ]
 
elementDeliveryDataRetrievalResponse :: XMLParser DeliveryDataRetrievalResponse
elementDeliveryDataRetrievalResponse = parseSchemaType "DeliveryDataRetrievalResponse"
elementToXMLDeliveryDataRetrievalResponse :: DeliveryDataRetrievalResponse -> [Content ()]
elementToXMLDeliveryDataRetrievalResponse = schemaTypeToXML "DeliveryDataRetrievalResponse"
 
elementFaultInfo :: XMLParser Xs.XsdString
elementFaultInfo = parseSchemaType "FaultInfo"
elementToXMLFaultInfo :: Xs.XsdString -> [Content ()]
elementToXMLFaultInfo = schemaTypeToXML "FaultInfo"
