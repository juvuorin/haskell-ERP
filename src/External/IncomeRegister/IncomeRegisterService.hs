{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnicodeSyntax #-}


module External.IncomeRegister.IncomeRegisterService where

import Control.Monad.Except ()
import Crypto.Hash.SHA256
import Data.ByteString.Base64 as Base'
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List.Split ()
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.XML.Types (Event)
import Database.Persist.Postgresql (fromSqlKey)
import External.Helpers (addSignature, prepareSoapRequest, sendSoapRequestAndValidate, signIt)
import External.IncomeRegister.Types
import External.IncomeRegister.XmlUtils (addSignedInfoAttributeForSigning)
import External.Utils
  ( createPemWithBeginAndEnd,
    mkMngr',
  )
import Import hiding (ByteString, Event, force, (++))
import OpenSSL.EVP.Base64 (decodeBase64)
import OpenSSL.EVP.Digest (getDigestByName)
import OpenSSL.EVP.Verify (VerifyStatus (..), verify)
import OpenSSL.PEM (readX509)
import OpenSSL.X509 (getPublicKey)
import Text.XML.C14N (c14n, c14n_exclusive_1_0)
import Text.XML.Cursor as Cur ()
import Text.XML.HaXml
import Text.XML.HaXml.Parse (xmlParse')
import Text.XML.HaXml.Posn ( noPos, Posn )
import Text.XML.HaXml.Schema.PrimitiveTypes
import Text.XML.HaXml.Types (Document (Document))
import Text.XML.Prettify
import qualified Text.XML.Stream.Parse
import Prelude hiding (id, print, (.))

productionSite = "https://ws.tulorekisteri.fi/20170526"

testSite = "https://ws-testi-2.tulorekisteri.fi/20170526"

-- | Prepare connection manager (uses companyId to get the right private key and certificate)
getMgr' :: CompanyId -> Handler ManagerSettings
getMgr' companyId = mkMngr' "" companyId

-- | Send xml report to the authority |
sendReport :: Reportable a => CompanyId -> a -> Handler (Either Text Text)
sendReport companyId x = do
  -- Prepare connection manager with company specific security credentials
  mgr <- mkMngr' "" companyId

  -- Build a report (anything that has an instance of the Reportable class)
  let report = buildReport x

  -- Sign the document with the company specific key
  signature <- signIt companyId report

  -- Add signature to the xml document (report)
  let content = verbatim $ addSignature signature report

  Import.putStrLn $ prettyPrintXmlDefault (pack $ content)

  (site, request, requestMgr) <- case (fromSqlKey companyId) of
    -- Company with id 5 is a test company
    5 -> return (testSite ++ "/" ++ service x, prepareSoapRequest (action x) content, mgr)

    -- Company with id 2 is also a test company, but for testing certificate retrieval/management
    2 -> sendResponseStatus status404 ("company with id 2 cannot make such a request" :: Text)
    -- Any other company is considered "real"
    _ -> return (productionSite ++ "/" ++ service x, prepareSoapRequest (action x) content, mgr)


  Import.print (site, request)
  result <- liftIO $ sendSoapRequestAndValidate site request requestMgr validateDocument
  case result of
    Left x -> return $ Left x
    Right x -> do
      let status = parseStatusResponse x
      case status of
        Right x -> return $ Right x
        Left x -> return $ Left x

type XmlContent = (Text.XML.HaXml.Content Posn)

getContentHeadMaybe :: ByteString -> Maybe XmlContent
getContentHeadMaybe x = do
  let content = unpack $  Data.Text.Encoding.decodeUtf8 $ LBS.toStrict x
  let doc = xmlParse' "" content

  case doc of
    Right x -> do
      let (Text.XML.HaXml.Types.Document _ _ root _) = x

      -- Peel Envelope and Body and just leave the content
      let contentWithoutEnvelopeAndBody = (children `o` children) $ CElem root noPos
      case contentWithoutEnvelopeAndBody of
        [] -> Nothing
        (x:xs) -> Just x
    Left _ -> Nothing


getValueByTagName tagName x = do
  let u = deep (tag tagName) $ Prelude.head x
  case u of
    [] -> Nothing
    (x : []) -> do
      let CElem (Elem _ _ [CString _ value _]) _ = x
      Just value
    _ -> Nothing


canonicalizeSignedInfo signedInfo = do
  let signedInfo' = verbatim $ addSignedInfoAttributeForSigning signedInfo

  c14n [] c14n_exclusive_1_0 [] False Nothing (Data.Text.Encoding.encodeUtf8 (Text.pack $ signedInfo'))
      >>= (return . Import.decodeUtf8)

canonicalize x = 
    c14n [] c14n_exclusive_1_0 [] False Nothing (Data.Text.Encoding.encodeUtf8 (Text.pack x))
      >>= (return . Import.decodeUtf8)

verifySignature signedInfo x509Certificate signatureValue = do

  -- Signed info needs to be canonicalized and must be added a xml-sig attribute
  canonicalizedSignedInfo <- canonicalizeSignedInfo signedInfo

  -- Need to create a PEM style certificate for the API
  x509 <- liftIO $ readX509 $ unpack $ createPemWithBeginAndEnd x509Certificate

  publicKey <- getPublicKey x509

  -- Pick digest algorithm
  (Just digest) <- getDigestByName "RSA-SHA256"
  OpenSSL.EVP.Verify.verify digest (decodeBase64 $ unpack signatureValue) publicKey (unpack canonicalizedSignedInfo)

sha256DigestInBase64 x =
  Base'.encode $ Crypto.Hash.SHA256.hash (Data.Text.Encoding.encodeUtf8 $ pack $ verbatim x)

-- Everything but the <Signature> should be the content of which the hash value is calculated
getSignedContent = chip $ keep `without` tag "Signature"


-- When a document is received from the service we must verify the signature and that the content is 
-- untouched
validateDocument :: ByteString -> IO (Either Text XmlContent)
validateDocument x = do

  case getContentHeadMaybe x of
    Nothing -> do
      return $ Left "Cannot find content head from XML"
    Just x -> do

      Import.putStrLn $ prettyPrintXmlDefault (pack $ verbatim x)

      let result =(,,)<$>getValueByTagName "DigestValue" [x]<*>
                         getValueByTagName "SignatureValue" [x]<*>
                         getValueByTagName "X509Certificate" [x]
      case result of
        Just (digestValue, signatureValue, x509Certificate) -> do
          let signedContent = getSignedContent x
          let signedInfo = deep (tag "SignedInfo") x

          canonicalized <- canonicalize (verbatim signedContent)

          let computedDigestValueOfSignedContent =
                BS.unpack $
                  Base'.encode $
                    Crypto.Hash.SHA256.hash (Data.Text.Encoding.encodeUtf8 $ canonicalized)

          verified <- verifySignature signedInfo (pack x509Certificate) signatureValue

          case verified of
            VerifySuccess ->
              if computedDigestValueOfSignedContent == digestValue
                then return $ Right x
                else return $ Left "Document was not validated successfully, computed digest does not match with the original"
            VerifyFailure -> return $ Left "Document was not validated successfully, signature verification failed"
        Nothing-> return $ Left "Signature tag is missing contents"


parseStatusResponse :: XmlContent -> Either Text Text
parseStatusResponse x = do
  let result = deep (tag "StatusResponse") x
  case result of
    [] -> Left (pack "StatusResponse tag is not present")
    l@(_ : _) -> do
      let statusValue = getValueByTagName "DeliveryDataStatus" l
      case statusValue of
        Just "3" -> Right "Report has been successfully transferred to IR"
        _ -> Left "Report has been not been transferred to IR"
