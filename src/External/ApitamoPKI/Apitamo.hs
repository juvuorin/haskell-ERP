{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module External.ApitamoPKI.Apitamo
  ( sendVatReportImmediate
  )
where

import Control.Applicative ()
import Control.Exception hiding (Handler)
-- #if MIN_VERSION_conduit(1,1,0)
import Crypto.Hash.SHA256 ()
-- #endif
import qualified Data.ByteString as B
import Data.ByteString.Base64 as Base ()
import Data.ByteString.Base64.Lazy ()
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.ByteString.Lazy.UTF8 as U (fromString)

import Data.List
  ( concat,
  )
import Data.List.Split ()
import Data.Text.Encoding (decodeLatin1)
import Data.Typeable ()
import Database.Persist.Postgresql (fromSqlKey)
import External.Utils (findStringBetween, mkMngr')
import Import
import Network.SOAP (ResponseParser (RawParser), Transport, invokeWS)
import Network.SOAP.Parsing.Stream (Event)
import Network.SOAP.Transport.HTTP (initTransportWithM)
import OpenSSL.EVP.PKey ()
import OpenSSL.PEM ()
import OpenSSL.RSA ()
import System.FilePath ()
import System.IO ()
import Text.XML ()
import Text.XML.C14N ()
import Text.XML.Cursor as Cur ()
import Text.XML.Stream.Parse as XSP (content, force, ignoreAttrs, parseLBS, parseText, requireAttr, tag', tagIgnoreAttrs)
import External.Helpers (sendSoapRequest', prepareSoapRequest, wrapToSoap)
import External.Certificate.CertificateService (toStrictUtf, parseTxt)
import qualified Text.XML as Text.XML.Stream.Parse
import Data.Request (DeliveryDataSendRequest(..), LanguageType (LanguageType_Fi))
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Text.XML.HaXml.Schema.Schema (schemaTypeToXML)
import Text.XML.HaXml
import External.Certificate.XmlUtils (addApitamoDeliveryDataSendRequestAttributes)


data DeliveryDataSendResponse = DeliveryDataSendResponse {filing :: Text, accepted :: Bool, timestamp :: Text, information :: Text, checksum :: Text, resultid :: Text} deriving (Show)

data DeliveryDataSendResponseFailureWithFiling = DeliveryDataSendResponseFailureWithFiling {filing :: Text, accepted :: Bool, timestamp :: Text, information :: Text}

data DeliveryDataSendResponseFault = DeliveryDataSendResponseFault {faultcode :: Text, faultstring :: Text} deriving (Show)

data TamoResult = TamoResult Text Text Text Text Text Text deriving (Show)

data VatDeclarationReceptionSummary = VatDeclarationReceptionSummary {tamoResult :: Maybe TamoResult} deriving (Show)

data DeliveryDataDeclarationReceiveVerificationSuccess = DeliveryDataDeclarationReceiveVerificationSuccess {filing :: Text, timestamp :: Text, status :: Text, information :: Text, resultid :: Text, accepted :: Bool, deliverydata :: Text} deriving (Show)

testSite = "https://apitesti.ilmoitin.fi/wsapp/apitamopki"

productionSite = "https://api.ilmoitin.fi/wsapp/apitamopki"

linuXBaseDirectory :: String
linuXBaseDirectory = "/home/azureuser/xop/"

data DeliveryDataRetrievalResponseType = WithTamoResult | WithoutTamoResult deriving (Show)

data DeliveryDataRetrievalResponse = DeliveryDataRetrievalResponse {xmlText :: Import.ByteString, responseType :: DeliveryDataRetrievalResponseType} deriving (Show)

xopMtomMessageRetrieveVatDeclarationProgressInfoOverride :: Monad m => Text -> Request -> m Request
xopMtomMessageRetrieveVatDeclarationProgressInfoOverride resultId request = do
  let requestXml =
        Data.List.concat
          [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>",
            "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">",
            "<soap:Body>",
            "<DeliveryDataRetrievalRequest xmlns=\"http://www.vero.fi/xmlschema/ApiTaMo\">",
            "<Language>fi</Language>",
            "<ResultId>" ++ unpack resultId ++ "</ResultId>",
            "</DeliveryDataRetrievalRequest>",
            "</soap:Body>",
            "</soap:Envelope>"
          ]

  return
    request
      { method = "POST",
        requestBody = RequestBodyLBS $ LBS.pack requestXml,
        requestHeaders =
          [ ("Content-Type", "text/xml; charset=utf-8"),
            ("SOAPAction", BS.pack "RetrievalAction")
          ]
      }

createVatDeclarationRequest ::  Text -> Bool -> Request
createVatDeclarationRequest vatDeclarationContent backgroundProcessing = do

  let request =  DeliveryDataSendRequest
        { deliveryDataSendRequest_language = LanguageType_Fi
          -- ^ Vastaussanomassa käytettävä kieli (fi=suomi, sv=ruotsi, 
          --   en=englanti).

        , deliveryDataSendRequest_backgroundProcessing = Just backgroundProcessing
          -- ^ Aineiston taustakäsittely (false=normaali käsittely, 
          --   true=taustakäsittely). Jos ei annettu suoritetaan normaali 
          --   käsittely.

        , deliveryDataSendRequest_emails = Nothing
        , deliveryDataSendRequest_reportingMaterial = Just $ Xs.XsdString "<xop:Include xmlns:xop=\"http://www.w3.org/2004/08/xop/include\" href=\"cid:123452407\"/>"
          -- ^ Elementti sisältää varsinainen ilmoituksen 
          --   MTOM/XOP-liitteenä. Alkuperäisen tiedoston nimen on oltava 
          --   mukana elementin sisällä.

        , deliveryDataSendRequest_attachments = Nothing
          -- ^ Elementti sisältää ilmoituksen tuloveroliitetiedostot.
        }

  let xmlRequest  =pack $  verbatim
        $ addApitamoDeliveryDataSendRequestAttributes
        $ schemaTypeToXML "DeliveryDataSendRequest" request

  let xopStartBoundary = "--adb92404-b8d9-471b-934a-fdc1ca4c8105\r\n"
  let xopHeader = "Content-Type: application/xop+xml; charset=UTF-8; type=\"text/xml\"\r\nContent-ID: <c8ccef39-fa08-4cec-8a1c-48a0ca707fb5>\r\n\r\n"
  let xmlBody =
          (wrapToSoap $ pack xmlRequest)
          <>"\r\n--adb92404-b8d9-471b-934a-fdc1ca4c8105\r\nContent-Type: text\r\nContent-Transfer-Encoding: binary\r\nContent-ID: <123452407>"

  let xopAttachment = ("\r\nContent-Disposition: attachment; filename=\"ALVILMOITUS.txt\"\r\n\r\n"::Text) <> vatDeclarationContent
  let xopEndBoundary = "\r\n--adb92404-b8d9-471b-934a-fdc1ca4c8105--"::Text

  let message = xopStartBoundary <> xopHeader <> xmlBody <> xopAttachment <> xopEndBoundary

  defaultRequest
      { method = "POST",
        requestBody = RequestBodyLBS $ LBS.pack (unpack message),
        requestHeaders =
          [ ("Content-Type", "Multipart/Related;boundary=adb92404-b8d9-471b-934a-fdc1ca4c8105;type=\"application/xop+xml\";start=\"<c8ccef39-fa08-4cec-8a1c-48a0ca707fb5>\";start-info=\"text/xml\""),
            ("SOAPAction", BS.pack "SendAction")
          ]
      }

findAndComposeXmlContent' :: LBS.ByteString -> IO LBS.ByteString
findAndComposeXmlContent' x = do
  putStrLn $ decodeLatin1 $ LBS.toStrict x

  let (basePart, tamoResultPart) = B.breakSubstring "<TamoResult xmlns=\"http://www.vero.fi/xmlschema/TaMoResult\">" (LBS.toStrict x)

  let tamoText' = decodeLatin1 {- LBS.toStrict -} tamoResultPart

  let startTamoString = "<TamoResult"
  let endTamoString = "</TamoResult>"

  let tamoText'' = findStringBetween startTamoString endTamoString tamoText'
  let tamoText = fromMaybe "" tamoText''

  let decoded = decodeUtf8 basePart

  let startSoapString = "<soap:Envelope"
  let endSoapString = "</soap:Envelope>"
  let summaryXml = findStringBetween startSoapString endSoapString decoded
  let baseXml = fromMaybe "" summaryXml

  final <-
    if tamoText /= ""
      then do
        let endResultString = "</Result>"
        let firstPartWithoutDeliveryData = fromMaybe "" $ findStringBetween startSoapString endResultString baseXml
        let endDeliveryDataRetrievalResponseString = "</DeliveryDataSendResponse>"
        let lastPartWithoutDeliveryData = fromMaybe "" $ findStringBetween endDeliveryDataRetrievalResponseString endSoapString baseXml
        let allTogetherNow = firstPartWithoutDeliveryData <> tamoText <> lastPartWithoutDeliveryData
        putStrLn "----with tamo"
        putStrLn allTogetherNow
        putStrLn "----with tamo"
        return allTogetherNow
      else do
        putStrLn "----No tamo result"
        putStrLn "XML content start ..."
        putStrLn (baseXml)
        putStrLn "XML content end ..."
        return baseXml
  return (U.fromString (unpack final))

sendVatDeclarationRequestImmediate'' :: Text -> CompanyId -> Handler (Either String String)
sendVatDeclarationRequestImmediate'' vatDeclaration companyId = do

  $(logInfo) "OK"
  putStrLn "Immediate request ------------- started -------------"

  mgr <- mkMngr' "" companyId

  (site, request) <- case fromSqlKey companyId of
    5 -> do
      let request = createVatDeclarationRequest vatDeclaration False
      return (testSite, request)
    2 -> sendResponseStatus status404 ("company with id 2 cannot make such a request" :: Text)
    _ -> do
      let request = createVatDeclarationRequest vatDeclaration False
      return (productionSite, request)

  result <- liftIO $ sendSoapRequest' site request mgr findAndComposeXmlContent'
  let resultVat = parseTxt parseSuccessfulVatDeclarationReceiveVerificationAfterImmediateProcessing $ toStrictUtf result

  case resultVat of
    Right x -> do
      putStrLn "Immediate request returned"
      putStrLn (pack $ show x)
      return $ Right (show x)
    Left exp -> do
      let message = "Vat declaration (immediate) was not accepted for processing"
      print exp
      putStrLn message
      return $ Left (show exp)

sendVatReportImmediate :: Text -> CompanyId -> Handler (Either String String)
sendVatReportImmediate vatDeclaration companyId' = do

  sendVatDeclarationRequestImmediate'' vatDeclaration companyId'

parseLbsText :: MonadThrow m => ConduitM Network.SOAP.Parsing.Stream.Event Void m r -> LBS.ByteString -> m r
parseLbsText p txtIn =
  runConduit $
    fuse (XSP.parseLBS def txtIn) p

parseSuccessfulVatDeclarationReceiveVerificationAfterImmediateProcessing :: MonadThrow m => ConduitM Network.SOAP.Parsing.Stream.Event o m (VatDeclarationReceptionSummary)
parseSuccessfulVatDeclarationReceiveVerificationAfterImmediateProcessing =
  XSP.force "Envelope tag is missing" $
    XSP.tagIgnoreAttrs "{http://schemas.xmlsoap.org/soap/envelope/}Envelope" $
      XSP.force "Body  tag is missing" $
        XSP.tagIgnoreAttrs "{http://schemas.xmlsoap.org/soap/envelope/}Body" $
          XSP.force "DeliveryDataSendResponse is missing" $
            XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}DeliveryDataSendResponse" $
              do
                XSP.force "Result tag is missing" $
                  XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}Result" $ do
                    filing <- XSP.force "filing tag missing" $ XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}Filing" XSP.content
                    accepted <- XSP.force "accepted tag missing" $ XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}Accepted" XSP.content
                    timestamp <- XSP.force "timestamp tag missing" $ XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}Timestamp" XSP.content
                    information <- XSP.force "information tag missing" $ XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}Information" XSP.content
                    XSP.force "checksum tag missing" $ XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}CheckSum" XSP.content
                tamoForm <- XSP.force "TamoResult is missing" $
                  XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/TaMoResult}TamoResult" $
                    do
                      checkupresult' <- XSP.force "accepted tag missing" $ XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/TaMoResult}CheckupResult" XSP.content

                      let parseTamoResult :: MonadThrow m => ConduitT Network.SOAP.Parsing.Stream.Event o m (Maybe TamoResult)
                          parseTamoResult = XSP.tag' "{http://www.vero.fi/xmlschema/TaMoResult}Form" parseAttributes $ \(customer, sequence, filingType, definition, status, year) ->
                            return $ TamoResult customer sequence filingType definition status year
                            where
                              parseAttributes =
                                (\a b c d e f -> (a, b, c, d, e, f)) <$> XSP.requireAttr "customer"
                                  <*> XSP.requireAttr "sequence"
                                  <*> XSP.requireAttr "filingType"
                                  <*> XSP.requireAttr "definition"
                                  <*> XSP.requireAttr "status"
                                  <*> XSP.requireAttr "year"
                                  <* XSP.ignoreAttrs

                      XSP.force "Forms tag is missing" $ XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/TaMoResult}Forms" $ XSP.force "form tag missing" parseTamoResult
                return $ VatDeclarationReceptionSummary (Just tamoForm)

parseAttributes = (\a b c d e f -> (a, b, c, d, e, f)) <$> XSP.requireAttr "customer" <*> XSP.requireAttr "sequence" <*> XSP.requireAttr "filingType" <*> XSP.requireAttr "definition" <*> XSP.requireAttr "status" <*> XSP.requireAttr "year" <* XSP.ignoreAttrs

parseSuccessfulVatDeclarationReceiveVerificationAfterBackgroundProcessing :: MonadThrow m => ConduitM Network.SOAP.Parsing.Stream.Event o m (VatDeclarationReceptionSummary)
parseSuccessfulVatDeclarationReceiveVerificationAfterBackgroundProcessing =
  XSP.force "Envelope tag is missing" $
    XSP.tagIgnoreAttrs "{http://schemas.xmlsoap.org/soap/envelope/}Envelope" $
      XSP.force "Body  tag is missing" $
        XSP.tagIgnoreAttrs "{http://schemas.xmlsoap.org/soap/envelope/}Body" $
          XSP.force "DeliveryDataRetrievalResponse is missing" $
            XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}DeliveryDataRetrievalResponse" $
              do
                filing <- XSP.force "filing tag missing" $ XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}Filing" XSP.content
                timestamp <- XSP.force "timestamp tag missing" $ XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}Timestamp" XSP.content
                status <- XSP.force "checksum tag missing" $ XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}Status" XSP.content
                information <- XSP.force "information tag missing" $ XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}Information" XSP.content
                resultid <- XSP.force "resultid tag missing" $ XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}ResultId" XSP.content
                accepted' <- XSP.force "Result is missing" $
                  XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}Result" $
                    do
                      accepted <- XSP.force "accepted tag missing" $ XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}Accepted" XSP.content
                      case accepted of
                        "true" -> return True
                        _ -> return False

                tamoForm <- XSP.force "TamoResult is missing" $
                  XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/TaMoResult}TamoResult" $
                    do
                      checkupresult' <- XSP.force "accepted tag missing" $ XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/TaMoResult}CheckupResult" XSP.content

                      let parseTamoResult :: MonadThrow m => ConduitT Network.SOAP.Parsing.Stream.Event o m (Maybe TamoResult)
                          parseTamoResult = XSP.tag' "{http://www.vero.fi/xmlschema/TaMoResult}Form" parseAttributes $ \(customer, sequence, filingType, definition, status, year) ->
                            return $ TamoResult customer sequence filingType definition status year
                            where
                              parseAttributes =
                                (\a b c d e f -> (a, b, c, d, e, f)) <$> XSP.requireAttr "customer"
                                  <*> XSP.requireAttr "sequence"
                                  <*> XSP.requireAttr "filingType"
                                  <*> XSP.requireAttr "definition"
                                  <*> XSP.requireAttr "status"
                                  <*> XSP.requireAttr "year"
                                  <* XSP.ignoreAttrs

                      XSP.force "Forms tag is missing" $ XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/TaMoResult}Forms" $ XSP.force "form tag missing" parseTamoResult
                return $ VatDeclarationReceptionSummary (Just tamoForm)

parseSuccessfulVatDeclarationForBackgroundProcessing :: MonadThrow m => ConduitM Network.SOAP.Parsing.Stream.Event o m (DeliveryDataSendResponse)
parseSuccessfulVatDeclarationForBackgroundProcessing =
  XSP.force "No data1" $
    XSP.tagIgnoreAttrs "{http://schemas.xmlsoap.org/soap/envelope/}Envelope" $
      XSP.force "No data2" $
        XSP.tagIgnoreAttrs "{http://schemas.xmlsoap.org/soap/envelope/}Body" $
          XSP.force "No data3" $
            XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}DeliveryDataSendResponse" $
              XSP.force "No data4" $
                XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}Result" $ do
                  filing <- XSP.force "filing tag missing" $ XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}Filing" XSP.content
                  accepted <- XSP.force "accepted tag missing" $ XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}Accepted" XSP.content
                  timestamp <- XSP.force "timestamp tag missing" $ XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}Timestamp" XSP.content
                  information <- XSP.force "information tag missing" $ XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}Information" XSP.content
                  checksum <- XSP.force "checksum tag missing" $ XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}CheckSum" XSP.content
                  resultid <- XSP.force "resultid tag missing" $ XSP.tagIgnoreAttrs "{http://www.vero.fi/xmlschema/ApiTaMo}ResultId" XSP.content

                  accepted' <- case accepted of
                    "true" -> return True
                    _ -> return False
                  return $ DeliveryDataSendResponse filing accepted' timestamp information checksum resultid

sendVatDeclarationForBackgroundProcessing :: Transport -> IO (Either Control.Exception.SomeException DeliveryDataSendResponse)
sendVatDeclarationForBackgroundProcessing t = invokeWS t "" () () parser
  where
    parser = RawParser $ \x -> do
      let r = decodeUtf8 $ LBS.toStrict (x)
      parseTxt parseSuccessfulVatDeclarationForBackgroundProcessing r

sendVatDeclarationForImmediateProcessing :: Transport -> IO (Either Control.Exception.SomeException VatDeclarationReceptionSummary)
sendVatDeclarationForImmediateProcessing t = invokeWS t "" () () parser
  where
    parser = RawParser $ \x -> do
      let r = decodeUtf8 $ LBS.toStrict (x)
      parseTxt parseSuccessfulVatDeclarationReceiveVerificationAfterImmediateProcessing r

sendVatDeclarationProgressInfoRequest :: Transport -> IO (Either Control.Exception.SomeException VatDeclarationReceptionSummary)
sendVatDeclarationProgressInfoRequest t = invokeWS t "" () () parser
  where
    parser = RawParser $ \x -> do
      parseTxt parseSuccessfulVatDeclarationReceiveVerificationAfterBackgroundProcessing (decodeUtf8 $ LBS.toStrict x)
