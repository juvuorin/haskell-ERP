{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts, DuplicateRecordFields, CPP #-}



{-# LANGUAGE AllowAmbiguousTypes #-}


module External.Helpers where
import Data.ByteString.Base64 as Base'
import Network.HTTP.Client
    ( RequestBody(RequestBodyLBS),
      Request,
      requestHeaders,
      requestBody,
      method,
      httpLbs,
      newManager )


import Control.Monad ( Monad(return) )
import Control.Monad.Except (liftIO, MonadIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.ByteString.Lazy.UTF8 as U ( fromString, toString )
import           Data.ByteString.UTF8 as UT ( fromString, toString )

import           Data.List.Split ()
import qualified Data.Text as T
import qualified Data.Text as Text
import           Data.Text.Encoding ( encodeUtf8 ) --as XSP
import           Text.XML.Cursor as Cur ()
import           External.Utils
    ( sign')
import Text.XML.HaXml.Verbatim (verbatim, Verbatim)
import Text.XML.HaXml.Combinators as C
import Text.XML.HaXml.Schema.PrimitiveTypes

import Crypto.Hash.SHA256 ( hash )
import qualified Data.ByteString.Lazy.UTF8 as Data.ByteString.UTF8

--import External.IncomeRegister.Types ()
import Text.XML.HaXml.Schema.Schema (SchemaType (schemaTypeToXML), Content)
import Data.Xmldsig (SignatureType)
import Data.Xmldsig
import Types hiding (unCertificate)
import Text.XML.HaXml.OneOfN
import Import (unpack, pack, CompanyId, Entity (Entity))
import qualified ClassyPrelude.Yesod as Text.XML.HaXml.Types
import Import hiding (unCertificate, Content, (++))
--import Import hiding (httpLbs, fromList,newManager)
import           External.Utils                  (mkMngr, mkMngrDefaultTls)
import qualified Data.ByteString.Lazy.UTF8 as LBS
import qualified Text.XML.HaXml
import Text.XML.HaXml.Posn (Posn)

pvtKeyFilename ="privatekey.pem"
csrFilename ="csr.csr"
certificateFilename ="Certificate.pem"

stripBeginAndEnd:: T.Text->T.Text
stripBeginAndEnd t =
    T.replace "-----END CERTIFICATE-----" "" $ T.replace "-----BEGIN CERTIFICATE-----" "" t

path =""

wrapToSoap toBeWrapped = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"<>
                            "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">"<>
                                "<soap:Body>"<>
                                    toBeWrapped<>
                                "</soap:Body>"<>
                            "</soap:Envelope>"


sendSoapRequest::MonadIO m => String -> Request -> ManagerSettings -> m LBS.ByteString
sendSoapRequest endpoint request managerSettings = do

  manager <- liftIO $ Network.HTTP.Client.newManager managerSettings
  let initReq = parseRequest_ endpoint
  let request' = initReq
                                { method          = "POST"
                                , requestHeaders  = requestHeaders request
                                , requestBody = requestBody request

                                }
  response <- liftIO $ Network.HTTP.Client.httpLbs request' manager

  return $ responseBody response

sendSoapRequest'::MonadIO m => String -> Request -> ManagerSettings-> (LBS.ByteString->m LBS.ByteString) -> m LBS.ByteString
sendSoapRequest' endpoint request managerSettings handler = do

  manager <- liftIO $ Network.HTTP.Client.newManager managerSettings
  let initReq = parseRequest_ endpoint
  let request' = initReq
                                { method          = "POST"
                                , requestHeaders  = requestHeaders request
                                , requestBody = requestBody request

                                }
  Import.print request'
  response <- liftIO $ Network.HTTP.Client.httpLbs request' manager
  handler (responseBody response)


sendSoapRequest''::String -> Request -> ManagerSettings-> (LBS.ByteString -> Handler (Either Text Text)) -> Handler (Either Text Text)
sendSoapRequest'' endpoint request managerSettings handler = do

  manager <- liftIO $ Network.HTTP.Client.newManager managerSettings
  let initReq = parseRequest_ endpoint
  let request' = initReq
                                { method          = "POST"
                                , requestHeaders  = requestHeaders request
                                , requestBody = requestBody request

                                }
  response <- liftIO $ Network.HTTP.Client.httpLbs request' manager
  handler (responseBody response)



sendSoapRequest'''::String -> Request -> ManagerSettings-> (LBS.ByteString ->IO (Either Text Text)) -> IO (Either Text Text)
sendSoapRequest''' endpoint request managerSettings handler = do

  manager <- liftIO $ Network.HTTP.Client.newManager managerSettings
  let initReq = parseRequest_ endpoint
  let request' = initReq
                                { method          = "POST"
                                , requestHeaders  = requestHeaders request
                                , requestBody = requestBody request

                                }
  response <- liftIO $ Network.HTTP.Client.httpLbs request' manager
  handler (responseBody response)


sendSoapRequestAndValidate::String -> Request -> ManagerSettings-> (LBS.ByteString ->IO (Either Text (Text.XML.HaXml.Content Posn))) -> IO (Either Text (Text.XML.HaXml.Content Posn))
sendSoapRequestAndValidate endpoint request managerSettings handler = do
--  Import.print endpoint
  manager <- liftIO $ Network.HTTP.Client.newManager managerSettings
  let initReq = parseRequest_ endpoint
  --Import.print initReq


  let request' = initReq
                                { method          = "POST"
                                , requestHeaders  = requestHeaders request
                                , requestBody = requestBody request

                                }

--  Import.print "tämä lähtee:"
--  Import.print request'

  response <- liftIO $ Network.HTTP.Client.httpLbs request' manager

  handler (responseBody response)




prepareSoapRequest::String -> String -> Request
prepareSoapRequest action body  = do
  let req = defaultRequest
  req  { method          = "POST"
      , requestBody     = RequestBodyLBS $  U.fromString (wrapToSoap body)  --LBS.pack (xmlBody)
      , requestHeaders  = [ ("Content-Type", "text/xml; charset=utf-8")
                      , ("SOAPAction", BS.pack action)
      ]}


soapRequest::MonadIO m => String -> String -> Request -> m Request
soapRequest action body request = return request
                                { method          = "POST"
                                , requestBody     = RequestBodyLBS $  U.fromString (wrapToSoap body)  --LBS.pack (xmlBody)
                                , requestHeaders  = [ ("Content-Type", "text/xml; charset=utf-8")
                                                , ("SOAPAction", BS.pack action)
                                ]}

signIt :: Verbatim a => Key Company -> a -> HandlerFor App [Content ()]
signIt companyId payload = do
        let digestValue = BS.unpack $ Base'.encode $ Crypto.Hash.SHA256.hash (Data.Text.Encoding.encodeUtf8 $ Text.pack $ verbatim payload)
        let signedInfo = mkSignedInfo (Digest' digestValue)
        let signedInfoXML' = schemaTypeToXML "SignedInfo" signedInfo

        -- This is important, canonicalization would do the same
        let signedInfoXML  = (tag "SignedInfo" `o` addAttribute "xmlns" "http://www.w3.org/2000/09/xmldsig#") (Prelude.head signedInfoXML')
    --    let signedInfoXML  = signedInfoXML'

        security <- runDB $ selectList [SecurityInfoCompanyId ==. companyId][] :: Handler [Entity SecurityInfo]
        securityEntity <- case security of
          (x:xs) -> return x
          _ -> sendResponseStatus status404 ("Security info was not found"::Text.Text)

        let pvtKey = securityInfoCurrentprivatekey (entityVal securityEntity)     -- Sign

        signatureValueBase64 <-case pvtKey of
          Just key -> do
            -- Sign signedInfoXML
            signatureValue' <- liftIO $ sign' key (Data.ByteString.UTF8.fromString $ verbatim signedInfoXML)
            -- create Base64 encoded signature value
            return $ BS.unpack $ Base'.encode (LBS.toStrict signatureValue')

          Nothing -> sendResponseStatus status404 ("Private key for the company could not be found!"::Text.Text)


        let certificate' = securityInfoCertificate (entityVal securityEntity)     -- Sign

        certificateValueWithoutHeaderAndFooter<-case certificate' of
          Just certificateValue ->
            return $ Prelude.concat $ Prelude.lines $ unpack ( stripBeginAndEnd (pack (UT.toString certificateValue)))

          Nothing -> sendResponseStatus status404 ("Certificate for the comapny can not be found!"::Text.Text)


        -- Read certificate
--        certificateValue' <- liftIO $ Prelude.readFile  certificateFilePath


        -- Create signature data
        let signed = mkSignature (Digest' digestValue) (Signature' signatureValueBase64) (Certificate'  certificateValueWithoutHeaderAndFooter)

        -- Get XML schema from data
        let signedXML = schemaTypeToXML "Signature" signed

        -- Add xmlsig attribute to signature
        let addAttributes  = tag "Signature" `o` addAttribute "xmlns" "http://www.w3.org/2000/09/xmldsig#"

        return $ addAttributes (Prelude.head signedXML)


signIt' :: Verbatim a => a -> ByteString->ByteString->IO [Content ()]
signIt' payload key cert= do
        let digestValue = BS.unpack $ Base'.encode $ Crypto.Hash.SHA256.hash (Data.Text.Encoding.encodeUtf8 $ Text.pack $ verbatim payload)
        let signedInfo = mkSignedInfo (Digest' digestValue)
        let signedInfoXML' = schemaTypeToXML "SignedInfo" signedInfo

        -- This is important, canonicalization would do the same
        let signedInfoXML  = (tag "SignedInfo" `o` addAttribute "xmlns" "http://www.w3.org/2000/09/xmldsig#") (Prelude.head signedInfoXML')
    --    let signedInfoXML  = signedInfoXML'
        signatureValue' <- sign' key (Data.ByteString.UTF8.fromString $ verbatim signedInfoXML)
        let signatureValueBase64 =  BS.unpack $ Base'.encode (LBS.toStrict signatureValue')

        let certificateValueWithoutHeaderAndFooter = Prelude.concat $ Prelude.lines $ unpack ( stripBeginAndEnd (pack (UT.toString cert)))

        -- Create signature data
        let signed = mkSignature (Digest' digestValue) (Signature' signatureValueBase64) (Certificate'  certificateValueWithoutHeaderAndFooter)

        -- Get XML schema from data
        let signedXML = schemaTypeToXML "Signature" signed

        -- Add xmlsig attribute to signature
        let addAttributes  = tag "Signature" `o` addAttribute "xmlns" "http://www.w3.org/2000/09/xmldsig#"

        return $ addAttributes (Prelude.head signedXML)



--signAndAddSignature :: Verbatim a => a -> ByteString->ByteString->IO [Content ()]
signAndAddSignature :: [Content()] -> ByteString->ByteString->IO [Content ()]

signAndAddSignature payload key cert= do

        let digestValue = BS.unpack $ Base'.encode $ Crypto.Hash.SHA256.hash (Data.Text.Encoding.encodeUtf8 $ Text.pack $ verbatim payload)
        let signedInfo = mkSignedInfo (Digest' digestValue)
        let signedInfoXML' = schemaTypeToXML "SignedInfo" signedInfo

        -- This is important, canonicalization would do the same
        let signedInfoXML  = (tag "SignedInfo" `o` addAttribute "xmlns" "http://www.w3.org/2000/09/xmldsig#") (Prelude.head signedInfoXML')
    --    let signedInfoXML  = signedInfoXML'
        signatureValue' <- sign' key (Data.ByteString.UTF8.fromString $ verbatim signedInfoXML)
        let signatureValueBase64 =  BS.unpack $ Base'.encode (LBS.toStrict signatureValue')

        let certificateValueWithoutHeaderAndFooter = Prelude.concat $ Prelude.lines $ unpack ( stripBeginAndEnd (pack (UT.toString cert)))

        -- Create signature data
        let signed = mkSignature (Digest' digestValue) (Signature' signatureValueBase64) (Certificate'  certificateValueWithoutHeaderAndFooter)

        -- Get XML schema from data
        let signedXML = schemaTypeToXML "Signature" signed

        -- Add xmlsig attribute to signature
        let addAttributes  = tag "Signature" `o` addAttribute "xmlns" "http://www.w3.org/2000/09/xmldsig#"

        let signature' = addAttributes (Prelude.head signedXML)

        return $ addSignature signature' payload



addSignature' signature x = do

    let keepChildrenAndAddNewElement e = children e ++ signature
    inplace keepChildrenAndAddNewElement $ Prelude.head x

addSignature'' signature x =
    let keepChildrenAndAddNewElement e = children e ++ signature
    in inplace keepChildrenAndAddNewElement $ Prelude.head x

addSignature signature x = inplace (\t->children t ++ signature) $ Prelude.head x


newtype Digest' = Digest'  {unDigest ::String}
newtype Signature' = Signature' {unSignature ::String}
newtype Certificate' = Certificate' {unCertificate::String}

mkSignature :: Digest' -> Signature' -> Certificate' -> SignatureType

mkSignature digestValue signatureValue certificateValue =
    let
        canonicalizationMethodType = CanonicalizationMethodType
         { canonMethodType_algorithm = AnyURI "http://www.w3.org/2001/10/xml-exc-c14n#" -- :: AnyURI
         , canonMethodType_text0  = "" -- :: String
         , canonMethodType_any1 = [] -- :: [AnyElement]
         , canonMethodType_text2 = "" -- :: String
         }


        signatureMethodType = SignatureMethodType
         { signatMethodType_algorithm = AnyURI "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256" -- :: AnyURI
         , signatMethodType_text0= ""   -- :: String
         , signatMethodType_hMACOutputLength = Nothing -- :: Maybe HMACOutputLengthType
         , signatMethodType_text2 = ""   --- :: String
         , signatMethodType_any3 = [] -- :: [AnyElement]
         , signatMethodType_text4 =""  -- :: String
         }

        referenceType = ReferenceType
         { refType_id = Nothing -- Maybe ID
         , refType_uRI = Just $ AnyURI ""  -- :: Maybe AnyURI
         , refType_type = Nothing -- :: Maybe AnyURI
         , refType_transforms = Just transformsType
         , refType_digestMethod = digestMethodType
         , refType_digestValue = digestValueType
         }
        digestValueType = DigestValueType $ Base64Binary $ unDigest digestValue

        transformType = TransformType
         { transfType_algorithm = AnyURI "http://www.w3.org/2000/09/xmldsig#enveloped-signature"  -- :: AnyURI
         , transfType_choice0 = [OneOf3 ""] -- :: [OneOf3 String Xsd.XsdString (AnyElement)]
          -- ^ Choice between:
          --   
          --   (1) mixed text
          --   
          --   (2) XPath
          --   
          --   (3) unknown
        }


        transformsType = TransformsType
         { transfType_transform = [transformType]
         }
        digestMethodType = DigestMethodType
         { digestMethodType_algorithm =  AnyURI "http://www.w3.org/2001/04/xmlenc#sha256"
         , digestMethodType_text0 = "" -- :: String
         , digestMethodType_any1 = [] -- :: [AnyElement]
         , digestMethodType_text2 =""  -- :: String
         }

        signedInfoType = SignedInfoType
         { signedInfoType_id = Nothing-- :: Maybe ID
         , signedInfoType_canonicalizationMethod = canonicalizationMethodType  -- :: CanonicalizationMethodType
         , signedInfoType_signatureMethod = signatureMethodType -- :: SignatureMethodType
         , signedInfoType_reference = [referenceType] -- :: [ReferenceType]
         }

        signatureValueType = SignatureValueType (Base64Binary $ unSignature signatureValue) signatureValueTypeAttributes
        signatureValueTypeAttributes = SignatureValueTypeAttributes
            { signatValueTypeAttrib_id = Nothing -- :: Maybe ID
            }



        signatureType = SignatureType
         { signatType_id = Nothing  -- :: Maybe ID
         , signatType_signedInfo = signedInfoType -- :: SignedInfoType
         , signatType_signatureValue = signatureValueType -- :: SignatureValueType
         , signatType_keyInfo = Just keyInfoType -- :: Maybe KeyInfoType
         , signatType_object = [] -- :: [ObjectType]
         }

        x509DataType = X509DataType
         { x509DataType_choice0 =   FourOf6 $ Base64Binary $ unCertificate certificateValue -- :: OneOf6 X509IssuerSerialType Base64Binary Xsd.XsdString Base64Binary Base64Binary (AnyElement)
          -- ^ Choice between:
          --   
          --   (1) X509IssuerSerial
          --   
          --   (2) X509SKI
          --   
          --   (3) X509SubjectName
          --   
          --   (4) X509Certificate
          --   
          --   (5) X509CRL
          --   
          --   (6) unknown
         }


        keyInfoType = KeyInfoType
         { keyInfoType_id = Nothing-- :: Maybe ID
         , keyInfoType_choice0 = [FiveOf9 x509DataType] -- :: [OneOf9 String Xsd.XsdString KeyValueType RetrievalMethodType X509DataType PGPDataType SPKIDataType Xsd.XsdString (AnyElement)]
          -- ^ Choice between:
          --   
          --   (1) mixed text
          --   
          --   (2) KeyName
          --   
          --   (3) KeyValue
          --   
          --   (4) RetrievalMethod
          --   
          --   (5) X509Data
          --   
          --   (6) PGPData
          --   
          --   (7) SPKIData
          --   
          --   (8) MgmtData
          --   
          --   (9) unknown
        }

        in signatureType

mkSignedInfo digestValue =
    let
        canonicalizationMethodType = CanonicalizationMethodType
         { canonMethodType_algorithm = AnyURI "http://www.w3.org/2001/10/xml-exc-c14n#" -- :: AnyURI
         , canonMethodType_text0  = "" -- :: String
         , canonMethodType_any1 = [] -- :: [AnyElement]
         , canonMethodType_text2 = "" -- :: String
         }


        signatureMethodType = SignatureMethodType
         { signatMethodType_algorithm = AnyURI "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256" -- :: AnyURI
         , signatMethodType_text0= ""   -- :: String
         , signatMethodType_hMACOutputLength = Nothing -- :: Maybe HMACOutputLengthType
         , signatMethodType_text2 = ""   --- :: String
         , signatMethodType_any3 = [] -- :: [AnyElement]
         , signatMethodType_text4 =""  -- :: String
         }

        referenceType = ReferenceType
         { refType_id = Nothing -- Maybe ID
         , refType_uRI = Just $ AnyURI ""  -- :: Maybe AnyURI
         , refType_type = Nothing -- :: Maybe AnyURI
         , refType_transforms = Just transformsType
         , refType_digestMethod = digestMethodType
         , refType_digestValue = digestValueType
         }

        digestValueType = DigestValueType $ Base64Binary $ unDigest digestValue

        transformType = TransformType
         { transfType_algorithm = AnyURI "http://www.w3.org/2000/09/xmldsig#enveloped-signature"  -- :: AnyURI
         , transfType_choice0 = [OneOf3 ""] -- :: [OneOf3 String Xsd.XsdString (AnyElement)]
          -- ^ Choice between:
          --   
          --   (1) mixed text
          --   
          --   (2) XPath
          --   
          --   (3) unknown
        }


        transformsType = TransformsType
         { transfType_transform = [transformType]
         }
        digestMethodType = DigestMethodType
         { digestMethodType_algorithm =  AnyURI "http://www.w3.org/2001/04/xmlenc#sha256"
         , digestMethodType_text0 = "" -- :: String
         , digestMethodType_any1 = [] -- :: [AnyElement]
         , digestMethodType_text2 =""  -- :: String
         }

        signedInfoType = SignedInfoType
         { signedInfoType_id = Nothing-- :: Maybe ID
         , signedInfoType_canonicalizationMethod = canonicalizationMethodType  -- :: CanonicalizationMethodType
         , signedInfoType_signatureMethod = signatureMethodType -- :: SignatureMethodType
         , signedInfoType_reference = [referenceType] -- :: [ReferenceType]
         }





        in signedInfoType