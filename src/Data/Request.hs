{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.Request
  ( module Data.Request
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
 
data Emails = Emails
        { emails_email :: [Xs.XsdString]
          -- ^ Elementissä voi antaa sähköpostiosoitteen tai -osoitteita, 
          --   lähetettäessä sellaisia aineistoja, joihin odotetaan 
          --   vastausaineistoja.
        }
        deriving (Eq,Show)
instance SchemaType Emails where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Emails
            `apply` between (Occurs (Just 0) (Just 16))
                            (parseSchemaType "Email")
    schemaTypeToXML s x@Emails{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Email") $ emails_email x
            ]
 
data Attachments = Attachments
        { attachments_attachment :: [Xs.Base64Binary]
          -- ^ Elementti sisältää tuloveroliitetiedoston 
          --   MTOM/XOP-liitteenä. Alkuperäisen tiedoston nimen on oltava 
          --   mukana elementin sisällä.
        }
        deriving (Eq,Show)
instance SchemaType Attachments where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Attachments
            `apply` many (parseSchemaType "Attachment")
    schemaTypeToXML s x@Attachments{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "Attachment") $ attachments_attachment x
            ]
 
data Result = Result
        { result_filing :: Maybe Xs.XsdString
          -- ^ Elementti sisltää lähetetyn aineiston nimen.
        , result_accepted :: Xs.Boolean
          -- ^ Elementissä kerrotaan onko ilmoitusten vastaanotto 
          --   onnistunut (true) vai epäonnistunut (false).
        , result_timestamp :: Xs.DateTime
          -- ^ Elementissä kerrotaan lähetyksen aikaleiman. Esim. 
          --   2018-10-13T11:05:15.522+02:00
        , result_information :: Maybe Xs.XsdString
          -- ^ Elementissä kerrotaan syy mahdolliseen ilmoitusten 
          --   hylkäämiseen.
        , result_checkSum :: Maybe Xs.XsdString
          -- ^ Elementti sisältää SHA1-tarkistussumman ilmoitusaineiston 
          --   sisällöstä.
        , result_deliveryId :: Maybe Xs.XsdString
          -- ^ Elementissä on sisältöä vain jos on lähetetty rakentamisen 
          --   tiedonantomenettelyyn (Raksi) liittyviä perusilmoituksia. 
          --   Tällöin elementin sisältönä on perusilmoituksen yksilöivä 
          --   ilmoitustunniste.
        , result_retrievalId :: Maybe Xs.XsdString
          -- ^ Elementti sisältää tunnisteen, jolla vastausaineisto 
          --   voidaan noutaa.
        , result_resultId :: Maybe Xs.XsdString
          -- ^ Elementti sisältää taustakäsittelyyn jätetyn 
          --   ilmoitusaineiston tarkastustuloksen noutotunnisteen.
        , result_attachments :: Maybe Attachments
          -- ^ Elementti sisältää tiedot lähetetyistä liitetiedostoista.
        }
        deriving (Eq,Show)
instance SchemaType Result where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Result
            `apply` optional (parseSchemaType "Filing")
            `apply` parseSchemaType "Accepted"
            `apply` parseSchemaType "Timestamp"
            `apply` optional (parseSchemaType "Information")
            `apply` optional (parseSchemaType "CheckSum")
            `apply` optional (parseSchemaType "DeliveryId")
            `apply` optional (parseSchemaType "RetrievalId")
            `apply` optional (parseSchemaType "ResultId")
            `apply` optional (parseSchemaType "Attachments")
    schemaTypeToXML s x@Result{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "Filing") $ result_filing x
            , schemaTypeToXML "Accepted" $ result_accepted x
            , schemaTypeToXML "Timestamp" $ result_timestamp x
            , maybe [] (schemaTypeToXML "Information") $ result_information x
            , maybe [] (schemaTypeToXML "CheckSum") $ result_checkSum x
            , maybe [] (schemaTypeToXML "DeliveryId") $ result_deliveryId x
            , maybe [] (schemaTypeToXML "RetrievalId") $ result_retrievalId x
            , maybe [] (schemaTypeToXML "ResultId") $ result_resultId x
            , maybe [] (schemaTypeToXML "Attachments") $ result_attachments x
            ]
 
data DeliveryDataSendRequest = DeliveryDataSendRequest
        { deliveryDataSendRequest_language :: LanguageType
          -- ^ Vastaussanomassa käytettävä kieli (fi=suomi, sv=ruotsi, 
          --   en=englanti).
        , deliveryDataSendRequest_backgroundProcessing :: Maybe Xs.Boolean
          -- ^ Aineiston taustakäsittely (false=normaali käsittely, 
          --   true=taustakäsittely). Jos ei annettu suoritetaan normaali 
          --   käsittely.
        , deliveryDataSendRequest_emails :: Maybe Emails
        , deliveryDataSendRequest_reportingMaterial :: Maybe Xs.XsdString --Base64Binary
          -- ^ Elementti sisältää varsinainen ilmoituksen 
          --   MTOM/XOP-liitteenä. Alkuperäisen tiedoston nimen on oltava 
          --   mukana elementin sisällä.
        , deliveryDataSendRequest_attachments :: Maybe Attachments
          -- ^ Elementti sisältää ilmoituksen tuloveroliitetiedostot.
        }
        deriving (Eq,Show)
instance SchemaType DeliveryDataSendRequest where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DeliveryDataSendRequest
            `apply` parseSchemaType "Language"
            `apply` optional (parseSchemaType "BackgroundProcessing")
            `apply` optional (parseSchemaType "Emails")
            `apply` optional (parseSchemaType "ReportingMaterial")
            `apply` optional (parseSchemaType "Attachments")
    schemaTypeToXML s x@DeliveryDataSendRequest{} =
        toXMLElement s []
            [ schemaTypeToXML "Language" $ deliveryDataSendRequest_language x
            , maybe [] (schemaTypeToXML "BackgroundProcessing") $ deliveryDataSendRequest_backgroundProcessing x
            , maybe [] (schemaTypeToXML "Emails") $ deliveryDataSendRequest_emails x
            , maybe [] (schemaTypeToXML "ReportingMaterial") $ deliveryDataSendRequest_reportingMaterial x
            , maybe [] (schemaTypeToXML "Attachments") $ deliveryDataSendRequest_attachments x
            ]
 
elementDeliveryDataSendRequest :: XMLParser DeliveryDataSendRequest
elementDeliveryDataSendRequest = parseSchemaType "DeliveryDataSendRequest"
elementToXMLDeliveryDataSendRequest :: DeliveryDataSendRequest -> [Content ()]
elementToXMLDeliveryDataSendRequest = schemaTypeToXML "DeliveryDataSendRequest"
 
data DeliveryDataSendResponse = DeliveryDataSendResponse
        { deliveryDataSendResponse_result :: Result
          -- ^ Elementin sisälle on koottu tiedot lähetyserän vastaanoton 
          --   onnistumisesta tai epäonnistumisesta.
        , deliveryDataSendResponse_checkupResult :: Maybe Xs.Base64Binary
          -- ^ Ilmoitusaineiston tarkastustulos MTOM/XOP-liitetiedostona.
        }
        deriving (Eq,Show)
instance SchemaType DeliveryDataSendResponse where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DeliveryDataSendResponse
            `apply` parseSchemaType "Result"
            `apply` optional (parseSchemaType "CheckupResult")
    schemaTypeToXML s x@DeliveryDataSendResponse{} =
        toXMLElement s []
            [ schemaTypeToXML "Result" $ deliveryDataSendResponse_result x
            , maybe [] (schemaTypeToXML "CheckupResult") $ deliveryDataSendResponse_checkupResult x
            ]
 
elementDeliveryDataSendResponse :: XMLParser DeliveryDataSendResponse
elementDeliveryDataSendResponse = parseSchemaType "DeliveryDataSendResponse"
elementToXMLDeliveryDataSendResponse :: DeliveryDataSendResponse -> [Content ()]
elementToXMLDeliveryDataSendResponse = schemaTypeToXML "DeliveryDataSendResponse"
 
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
