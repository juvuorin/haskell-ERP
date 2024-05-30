{-# LANGUAGE CPP #-}
{-# TypeFamilies #-}


{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module External.Certificate.CertificateService
  ( runSignNewCertificate,
    runRenewCertificate,
    parseTxt, toStrictUtf,readPEM
  )
where

import Control.Monad.Except
import Crypto.Hash.Algorithms (SHA512 (SHA512))
import Crypto.PubKey.RSA
import Crypto.Store.PKCS8
import Data.ByteString.Base64.Lazy
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.ByteString.Lazy.UTF8 as U (fromString)
import Data.ByteString.UTF8 (toString)
import Data.CertificateServices
  ( CertificateRequestType (CertificateRequestType),
    EnvironmentTypes (EnvironmentTypes_PRODUCTION, EnvironmentTypes_TEST),
    GetCertificateRequest (..),
    RenewCertificateRequest (..),
    SignNewCertificateRequest (..),
    String100 (String100),
    String16 (String16),
    String30 (String30),
    String32 (String32),
  )
import Data.List (head)
import Data.List.Split ()
import Data.Maybe (fromJust)
import Data.PEM
import Data.String.Conversions (cs)
import qualified Data.Text
import qualified Data.Text as T
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (addUTCTime, nominalDay)
import Data.X509
import qualified Data.X509 as X509
import Data.X509.PKCS10
import Data.XML.Types (Event)
import Data.Xmldsig (SignatureType)
import Database.Persist.Postgresql (fromSqlKey)
import Database.Persist.Sql (toSqlKey)
import External.Certificate.XmlUtils
import External.Helpers (prepareSoapRequest, sendSoapRequest', signAndAddSignature)
import External.Utils as Utils
  ( createPemWithBeginAndEnd,
    findStringBetween,
    getKeyAndCert,
    mkMngrDefaultTls,
  )
import GHC.IO.Exception (IOException (..))
import Import hiding (Event, authorityCompanyId, authorityCustomerId, authorityCustomerName, authorityTransferId, authorityTransferPassword, force, path, putStrLn)
import OpenSSL.EVP.Base64 (decodeBase64)
import OpenSSL.EVP.Digest (getDigestByName)
import OpenSSL.EVP.Verify (verify)
import OpenSSL.PEM (readX509)
import OpenSSL.X509 (getNotAfter, getPublicKey)
import System.Directory
import Text.XML (nameLocalName)
import Text.XML.C14N
import Text.XML.Cursor as Cur ()
import Text.XML.HaXml (Verbatim (verbatim))
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Text.XML.HaXml.Schema.Schema
import Text.XML.Stream.Parse
import Prelude hiding (concat, id, length, lines, print, readFile, writeFile, (++), (.))

newtype RetrievalId = RetrievalId {unRetrievalId :: Text} deriving (Show)

data StatusMessageRetrievalId = Fail String | Ok RetrievalId

pvtKeyFilename = "privatekey.pem"

csrFilename = "csr.csr"

certificateFilename = "Certificate.pem"

productionSite = "https://pkiws.vero.fi/2017/10/CertificateServices"

testSite :: String
--testSite = "https://pkiws-testi.vero.fi/DEV/2017/10/CertificateServices"
testSite = "https://pkiws-testi.vero.fi/2017/10/CertificateServices"


keyTesting = False

path = ""

linuXBaseDirectory :: String
linuXBaseDirectory = "/home/azureuser/xop/"

c14n' x =
  c14n [] c14n_exclusive_1_0 [] False Nothing (Data.Text.Encoding.encodeUtf8 x)

mkSignNewCertificateRequest :: EnvironmentTypes -> Text -> Text -> Text -> Text -> String -> SignNewCertificateRequest
mkSignNewCertificateRequest environment customerId customerName transferId transferPassword certificateRequest =
  SignNewCertificateRequest
    { signNewCertificateRequest_environment = environment,
      signNewCertificateRequest_customerId = String30 $ Xs.Normalized (unpack customerId),
      signNewCertificateRequest_customerName = Just $ String100 $ Xs.Normalized (unpack customerName),
      signNewCertificateRequest_transferId = String32 $ Xs.Normalized (unpack transferId),
      signNewCertificateRequest_transferPassword = String16 $ Xs.Normalized (unpack transferPassword),
      signNewCertificateRequest_certificateRequest = CertificateRequestType $ Xs.Base64Binary certificateRequest
    }

mkRenewCertificateRequest :: EnvironmentTypes -> Text -> Text -> String -> Maybe SignatureType -> RenewCertificateRequest
mkRenewCertificateRequest environment customerId customerName certificateRequest signature =
  RenewCertificateRequest
    { renewCertificateRequest_environment = environment,
      renewCertificateRequest_customerId = String30 $ Xs.Normalized (unpack customerId),
      renewCertificateRequest_customerName = Just $ String100 $ Xs.Normalized (unpack customerName),
      renewCertificateRequest_certificateRequest = CertificateRequestType $ Xs.Base64Binary certificateRequest,
      renewCertificateRequest_signature = signature
    }

mkGetCertificateRequest :: EnvironmentTypes -> Text -> Text -> RetrievalId -> GetCertificateRequest
mkGetCertificateRequest environment customerId customerName retrievalId =
  GetCertificateRequest
    { getCertificateRequest_environment = environment,
      getCertificateRequest_customerId = String30 $ Xs.Normalized (unpack customerId),
      getCertificateRequest_customerName = Just $ String100 $ Xs.Normalized (unpack customerName),
      getCertificateRequest_retrievalId = String32 $ Xs.Normalized (unpack $ unRetrievalId retrievalId)
    }

prepareSendCertificateRequest :: Monad IO => CompanyId -> Text -> Text -> Text -> Text -> Text -> IO Request
prepareSendCertificateRequest companyId customerId customerName transferPassword transferId csr = do
  let requestType = if companyId == toSqlKey 2 || companyId == toSqlKey 5 then EnvironmentTypes_TEST else EnvironmentTypes_PRODUCTION
  let xmlString =
        verbatim $
          addSignNewCertificateRequestAttributes $
            schemaTypeToXML "SignNewCertificateRequest" $
              mkSignNewCertificateRequest requestType customerId customerName transferId transferPassword (unpack csr)

  putStrLn xmlString
  return $ prepareSoapRequest "signNewCertificate" xmlString

prepareRenewCertificateRequest :: ByteString -> ByteString -> CompanyId -> Text -> Text -> Text -> IO Request
prepareRenewCertificateRequest key cert companyId customerId customerName certificateRequest = do
  let requestType = if companyId == toSqlKey 2 || companyId == toSqlKey 5 then EnvironmentTypes_TEST else EnvironmentTypes_PRODUCTION
  let xmlWithProperAttributes =
        addRenewCertificateRequestAttributes $
          schemaTypeToXML "RenewCertificateRequest" $
            mkRenewCertificateRequest requestType customerId customerName (unpack certificateRequest) Nothing

  signedContent <- signAndAddSignature xmlWithProperAttributes key cert

  return $ prepareSoapRequest "renewCertificate" $ verbatim signedContent

prepareGetCertificateRequest :: CompanyId -> Text -> Text -> RetrievalId -> IO Request
prepareGetCertificateRequest companyId customerId customerName retrievalId = do
  let requestType = if companyId == toSqlKey 2 || companyId == toSqlKey 5 then EnvironmentTypes_TEST else EnvironmentTypes_PRODUCTION
  let content =
        verbatim $
          addGetCertificateRequestAttributes $
            schemaTypeToXML "GetCertificateRequest" $
              mkGetCertificateRequest requestType customerId customerName retrievalId

  return $ prepareSoapRequest "getCertificate" content

stripHeader :: Text -> Text
stripHeader = Text.replace "<SOAP-ENV:Header/>" ""

stripBeginAndEnd :: Text -> Text
stripBeginAndEnd t =
  Text.replace "-----END CERTIFICATE-----" "" $ Text.replace "-----BEGIN CERTIFICATE-----" "" t

stripBeginAndEnd' :: Text -> Text -> Text -> Text
stripBeginAndEnd' b e t =
  Text.replace e "" $ Text.replace b "" t

readPEM :: String -> [PEM]
readPEM content = either error id $ pemParseBS $ cs content

findAndComposeXmlContent :: LBS.ByteString -> IO LBS.ByteString
findAndComposeXmlContent x = do
  let content = stripHeader $ decodeUtf8 $ LBS.toStrict x
  liftIO $ putStrLn "........................................."

  liftIO $ putStrLn (Text.unpack content)
  liftIO $ putStrLn "........................................."

  let signatureValueBegin = "<ds:SignatureValue>"
  let signatureValueEnd = "</ds:SignatureValue>"

  let signedInfoElementBegin = "<ds:SignedInfo>"
  let signedInfoElementEnd = "</ds:SignedInfo>"

  let certificate509Begin = "<ds:X509Certificate>"
  let certificate509End = "</ds:X509Certificate>"

  let certificateValue' =
        unpack $
          Text.replace "&#13;" "" $
            stripBeginAndEnd' certificate509Begin certificate509End $
              fromMaybe "" $ findStringBetween certificate509Begin certificate509End content

  let certificateValue = "-----BEGIN CERTIFICATE-----" ++ certificateValue' ++ "-----END CERTIFICATE-----"

  let signatureValue = fromMaybe "" $ findStringBetween signatureValueBegin signatureValueEnd content
  let signedInfoElement =
        Text.replace "<ds:SignedInfo>" "<ds:SignedInfo xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\">" $
          fromMaybe "" $ findStringBetween signedInfoElementBegin signedInfoElementEnd content

  let signatureContent =
        Text.pack $
          mconcat $
            lines $
              unpack $
                Text.replace "&#13;" "" $
                  stripBeginAndEnd' signatureValueBegin signatureValueEnd signatureValue

  canonicalizedSignedInfo <-
    c14n [] c14n_exclusive_1_0 [] False Nothing (Data.Text.Encoding.encodeUtf8 signedInfoElement)
      >>= return . decodeUtf8

  cert <- readX509 certificateValue

  key <- getPublicKey cert

  digest <- getDigestByName "RSA-SHA256"

  let sContent = decodeBase64 (unpack signatureContent)
  case digest of
    Just digest' -> do
      status <- OpenSSL.EVP.Verify.verify digest' sContent key (unpack canonicalizedSignedInfo)
      liftIO $ print status
    Nothing -> liftIO $ putStrLn "Not working!"
  return (U.fromString $ unpack content)

storeFirst :: Handler ()
storeFirst = do

  -- 999 oikeat kirjautumistiedot Ideal Learning Oy:lle
  let file = "certificateStore/999/privatekey.pem"
  result <- liftIO (try (readKeyFile file) :: IO (Either IOError [OptProtected PrivKey]))

  let certFile = "certificateStore/999/Certificate.pem"
  cert <- liftIO (try (BS.readFile certFile) :: IO (Either IOError ByteString))

  let companyId = toSqlKey 1

  case cert of
    Right x -> runDB $ updateWhere [SecurityInfoCompanyId ==. companyId] [SecurityInfoCertificate =. Just x]
    Left _ -> sendResponseStatus status404 ("Cannot find certificate!" :: Text)

  case result of
    Right (privKey : _) -> do
      liftIO $ putStrLn ("Private key file path to create csr:" ++ file)
      let k = recover "" privKey
      privKey <- case k of
        Right privKey -> return $ writeKeyFileToMemory TraditionalFormat [privKey]
        Left err -> sendResponseStatus status404 ("Cannot create a test key from file!" :: Text)

      runDB $ updateWhere [SecurityInfoCompanyId ==. companyId] [SecurityInfoCurrentprivatekey =. Just privKey]
    Right _ -> sendResponseStatus status404 ("Cannot create a test key from file!" :: Text)
    Left _ -> sendResponseStatus status404 ("Cannot create a test key from file!" :: Text)

initSecurityRecord :: Key Company -> HandlerFor App (Either Text Text)
initSecurityRecord companyId' = do
  security <- runDB $ selectList [SecurityInfoCompanyId ==. companyId'] [] :: Handler [Entity SecurityInfo]
  _ <- case security of
    (x : xs) -> return $ Right "Security record already inserted"
    _ -> do
      result <-
        runDB $
          insert
            SecurityInfo
              { securityInfoCompanyId = companyId',
                securityInfoCurrentprivatekey = Nothing,
                securityInfoPotentialprivatekey = Nothing,
                securityInfoOldprivatekey = Nothing,
                securityInfoCertificate = Nothing,
                securityInfoCertificatevalid = Nothing
              }
      return $ Right ("Security record successfully inserted for company" ++ show (fromSqlKey companyId'))

  -- The following is for testing purposes only and applies to company with id 2
  let file = "certificateStoreTest/SignNewCertificate_Private__new.key"
  result <- liftIO (try (readKeyFile file) :: IO (Either IOError [OptProtected PrivKey]))

  case result of
    Right (privKey : _) -> do
      let k = recover "" privKey
      case k of
        Right privKey -> do
          let key = writeKeyFileToMemory TraditionalFormat [privKey]
          runDB $ updateWhere [SecurityInfoCompanyId ==. toSqlKey 2] [SecurityInfoPotentialprivatekey =. Just key]
          return $ Right ("Potential private key changed to company with id 2" :: Text)
        Left err -> sendResponseStatus status404 ("Cannot create a test key from file!" :: Text)
    Left _ -> sendResponseStatus status404 ("Cound not extract private key for test record for comapny 2" :: Text)


initSecurityRecord2 :: Key Company -> HandlerFor App (Either Text Text)
initSecurityRecord2 companyId' = do
  security <- runDB $ selectList [SecurityInfoCompanyId ==. companyId'] [] :: Handler [Entity SecurityInfo]
  case security of
    (x : xs) -> return $ Right "Security record already inserted"
    _ -> do
      result <-
        runDB $ insert
            SecurityInfo
              { securityInfoCompanyId = companyId',
                securityInfoCurrentprivatekey = Nothing,
                securityInfoPotentialprivatekey = Nothing,
                securityInfoOldprivatekey = Nothing,
                securityInfoCertificate = Nothing,
                securityInfoCertificatevalid = Nothing
              }

      return $ Right $ pack ("Security record successfully inserted for company" ++ (show (fromSqlKey companyId')))

sendRenewCertificateRequest' :: CompanyId -> Text -> Text -> Handler (Either Text.Text RetrievalId)
sendRenewCertificateRequest' companyId customerId customerName = do
  -- If companyId is 2, then use the test company and test mode
  let mode = if companyId == toSqlKey 2 then "test" else "production"

  -- Default tls setting with no certificate and private key, just default tls socket
  mgr <- liftIO mkMngrDefaultTls

  -- If we are executing in test mode, we get the test private key from a file
  pvtKey <-
    if mode == "test" && keyTesting
      then -- In test mode we get the private key from a file
        getNewPrivateKeyTest companyId $ Sign "certificateStoreTest/RenewCertificate_Private__new.key"
      else do
        -- In production mode we get the certificate and the key from the database
        keyAndCert <- getKeyAndCert companyId
        (key, cert) <- case keyAndCert of
          Just x -> return x
          Nothing -> sendResponseStatus status404 ("Certificate and private key not found! Certificate can only be renewed if old key and certificate exist" :: Text)

        x509 <- liftIO $ readX509 (toString cert)

        -- get information on the validity of the certificate
        validity <- liftIO $ getNotAfter x509
        liftIO $ putStrLn $ "Certificate is valid until: " ++ show validity

        -- get current time
        current <- liftIO getCurrentTime
        liftIO $ putStrLn $ "Current time is: " ++ show current

        -- the earliest time to renew the certificate is 60 days before the end of validity
        let earliestRenewalTime = addUTCTime (-60 * nominalDay) validity
        liftIO $ putStrLn $ "Earliest renewal time is: " ++ show earliestRenewalTime

        if current >= earliestRenewalTime
          then -- We only create a new private key if certificate is about to be expire within 60 days
            getNewPrivateKey companyId
          else sendResponseStatus status404 ("The earliest time to renew the certificate is 60 days before the expiration of the certificate" :: Text)

  -- Create a new certificate signing request
  csr <- liftIO $ createCsr' pvtKey

  case csr of
    -- If certificate signing request is successfully created
    Right csr -> do
      -- Get security record of the company
      security <- runDB $ selectList [SecurityInfoCompanyId ==. companyId] [] :: Handler [Entity SecurityInfo]
      securityEntity <- case security of
        -- Security record exists
        (x : xs) -> return x
        -- Security record is missing
        _ -> sendResponseStatus status404 ("Security info was not found" :: Text.Text)

      -- Get certificate
      let certificate' = securityInfoCertificate (entityVal securityEntity)

      -- Check if certificate exists
      certificate <- case certificate' of
        -- Yes
        Just certificate -> return certificate
        -- No
        Nothing -> sendResponseStatus status404 ("Certificate for the comapny can not be found!" :: Text.Text)

      -- If we run in test mode, we get the private key from a file
      oldPvtKey <-
        if mode == "test" && keyTesting
          then -- Old private key for the test mode is the one that was used when the first certificate was obtained
            getNewPrivateKeyTest companyId $ Renew "certificateStoreTest/SignNewCertificate_Private__new.key"
          else -- If we run in production mode, we get the private key from the database
          do
            let key' = securityInfoCurrentprivatekey (entityVal securityEntity)

            -- Check if we got the key or not
            case key' of
              -- Yes
              Just key -> return key
              -- No
              Nothing -> sendResponseStatus status404 ("Private key for the company could not be found!" :: Text.Text)

      -- Renew request is signed with the old private key that matches the current certificate
      -- and csr is signed with the new private key
      preparedRequest <- liftIO $ prepareRenewCertificateRequest oldPvtKey certificate companyId customerId customerName (toStrictUtf csr)

      -- Depending on the companyId we either send the request to the test site or production site
      let site = if companyId == toSqlKey 2 || companyId == toSqlKey 5  then testSite else productionSite

      checkedResponse <- liftIO $ sendSoapRequest' site preparedRequest mgr findAndComposeXmlContent

      -- Try to parse the response
      let resultRenewCertificateRequest = parseTxt parseRenewCertificateRequestResponse' (toStrictUtf checkedResponse)

      -- Is parsing successful
      case resultRenewCertificateRequest of
        -- Yes
        Right x -> do
          liftIO $ putStrLn "renewCertificateRequest returned successfully"
          liftIO $ putStrLn $ "RetrievalId is:" ++ show x
          return $ Right x
        -- No
        Left exp -> do
          let message = "renewCertificateRequest returned unsuccessfully with exception"
          liftIO $ print exp
          liftIO $ putStrLn message
          return $ Left (Text.pack (message ++ " " ++ show exp))
    Left _ -> return $ Left "could not create csr!"

toStrictUtf :: LBS.ByteString -> Text
toStrictUtf x = decodeUtf8 $ LBS.toStrict x

data SignOrRenew = Sign String | Renew String

data Mode = Test String | Production

sendSignNewCertificateRequest' ::
  CompanyId ->
  Text ->
  Text ->
  Text ->
  Text ->
  Handler (Either Text.Text RetrievalId)
sendSignNewCertificateRequest' companyId' customerId customerName transferId transferPassword = do
  -- Check if the company has the security record
  keyAndCert <- getKeyAndCert companyId'
  let keyAndCertAreAvailable = case keyAndCert of
        Just x -> True
        Nothing -> False

  -- Check if we run in test mode or production mode
  let mode =
        if companyId' == toSqlKey 2 || companyId' == toSqlKey 5
          then "test"
          else "production"

  if not keyAndCertAreAvailable || mode == "test"
    then do
      mgr <- liftIO mkMngrDefaultTls

      -- If we are in test mode, we use the predefined key
      pvtKey <-
        if mode == "test" && keyTesting
          then getNewPrivateKeyTest companyId' $ Sign "certificateStoreTest/SignNewCertificate_Private__new.key"
          else -- If in production, we create a new private key
            getNewPrivateKey companyId'

      -- Create the certificate signing request with the private key
      csr' <- liftIO $ createCsr' pvtKey

      -- Check if we get the csr created
      csr <- case csr' of
        -- Yes
        Right k -> return k
        -- No
        Left _ -> sendResponseStatus status404 ("Could not create the certificate signing request" :: Text)

      preparedRequest <-
        liftIO $
          prepareSendCertificateRequest companyId' customerId customerName transferPassword transferId $
            toStrictUtf csr

      -- Change the site according to the companyId, 2 is for testing
      let site = if companyId' == toSqlKey 2 || companyId' == toSqlKey 5 then testSite else productionSite
      print preparedRequest
      print site

      -- Once received from the server pass the data througg findAndComposeXmlContent function
      checkedResponse <- liftIO $ sendSoapRequest' site preparedRequest mgr findAndComposeXmlContent

      -- Try to parse the response
      let resultSignNewCertificateRequest =
            parseTxt parseSignNewCertificateRequestResponse' $
              toStrictUtf checkedResponse

      case resultSignNewCertificateRequest of
        Right x -> do
          liftIO $ putStrLn "signNewCertificateRequest OK"
          liftIO $ putStrLn $ "RetrievalId:" ++ show x
          liftIO $ putStrLn "...odotetaan, että homma valmistuu..."
          return $ Right x
        Left exp -> do
          let message = "signNewCertificateRequest FAILED"
          liftIO $ print exp
          liftIO $ putStrLn message
          return $ Left (Text.pack ("SignNewCertificateRequest failed" ++ show exp))
    else return $ Left ("Please remove old keys and certificates" :: Text.Text)

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

printException :: Either IOError a -> IO ()
printException (Right _) = liftIO $ putStrLn "No exception caught"
printException (Left e) =
  liftIO $
    putStrLn $
      concat
        [ "ioe_filename = ",
          show $ ioe_filename e,
          ", ioe_description = ",
          show $ ioe_description e,
          ", ioe_errno = ",
          show $ ioe_errno e
        ]

getNewPrivateKey :: CompanyId -> Handler ByteString
getNewPrivateKey companyId = do
  let rsaKeySize = 256
  let publicExponent = 3

  -- Create a new private key/public key pair
  (pubKey, privKey) <- liftIO $ generate rsaKeySize publicExponent

  let d = PrivKeyRSA privKey
  let p = PubKeyRSA pubKey
  let result = writeKeyFileToMemory TraditionalFormat [d]

  security <- runDB $ selectList [SecurityInfoCompanyId ==. companyId] [] :: Handler [Entity SecurityInfo]
  securityEntity <- case security of
    (x : xs) -> return x
    _ -> sendResponseStatus status404 ("Security info was not found" :: Text)

  runDB $ update (entityKey securityEntity) [SecurityInfoPotentialprivatekey =. Just result]
  return result

getNewPrivateKeyTest :: CompanyId -> SignOrRenew -> Handler ByteString
getNewPrivateKeyTest companyId mode =
  case mode of
    Sign x -> do
      -- Read test key from a file
      result <- liftIO (try (readKeyFile x) :: IO (Either IOError [OptProtected PrivKey]))

      case result of
        Right (privKey : _) -> do
          liftIO $ putStrLn ("Private key test file path to create csr:" ++ x)
          let k = recover "" privKey
          case k of
            Right privKey -> do
              let result = writeKeyFileToMemory TraditionalFormat [privKey]
              security <- runDB $ selectList [SecurityInfoCompanyId ==. companyId] [] :: Handler [Entity SecurityInfo]
              securityEntity <- case security of
                (x : xs) -> return x
                _ -> sendResponseStatus status404 ("Security info was not found" :: Text)

              runDB $ update (entityKey securityEntity) [SecurityInfoPotentialprivatekey =. Just result]
              return result
            Left err -> sendResponseStatus status404 ("Cannot create a test key from file!" :: Text)
        Right _ -> sendResponseStatus status404 ("Cannot create a test key from file!" :: Text)
        Left _ -> sendResponseStatus status404 ("Cannot create a test key from file!" :: Text)
    Renew x -> do
      -- Read test key from a file
      result <- liftIO (try (readKeyFile x) :: IO (Either IOError [OptProtected PrivKey]))

      case result of
        Right (privKey : _) -> do
          liftIO $ putStrLn ("Private key test file path to create csr:" ++ x)
          let k = recover "" privKey
          case k of
            Right privKey -> do
              let result = writeKeyFileToMemory TraditionalFormat [privKey]
              security <- runDB $ selectList [SecurityInfoCompanyId ==. companyId] [] :: Handler [Entity SecurityInfo]
              securityEntity <- case security of
                (x : xs) -> return x
                _ -> sendResponseStatus status404 ("Security info was not found" :: Text)

              runDB $ update (entityKey securityEntity) [SecurityInfoCurrentprivatekey =. Just result]
              return result
            Left err -> sendResponseStatus status404 ("Cannot create a test key from file!" :: Text)
        Right _ -> sendResponseStatus status404 ("Cannot create a test key from file!" :: Text)
        Left _ -> sendResponseStatus status404 ("Cannot create a test key from file!" :: Text)

createCsr' :: ByteString -> IO (Either Text LBS.ByteString)
createCsr' pvtKey = do
  let (privKey' : _) = readKeyFileFromMemory pvtKey
  liftIO $ putStrLn "Private key to create csr for:abcd..."
  let k = recover "" privKey'
  case k of
    Left err -> do
      liftIO $ putStrLn $ "Unable to recover key: " ++ show err
      return $ Left "FAIL"
    Right (PrivKeyRSA privKey) -> do
      let pubKey = private_pub privKey

      -- TODO This should be changed according to company information!
      let subjectAttrs = makeX520Attributes [(X520CommonName, ""), (X520OrganizationName, "")]

      let extAttrs = PKCS9Attributes [PKCS9Attribute $ ExtBasicConstraints False Nothing, PKCS9Attribute $ ExtKeyUsage [KeyUsage_digitalSignature, KeyUsage_nonRepudiation, KeyUsage_keyEncipherment]]
      Right req <- generateCSR subjectAttrs extAttrs (KeyPairRSA pubKey privKey) SHA512
      liftIO $ putStrLn "Certification request (csr) successfully created"
      print req

      -- Base64 encode, just the content without BEGIN and AND headers
      return $ Right $ Data.ByteString.Base64.Lazy.encode $ LBS.fromStrict $ pemContent $ toPEM req

    Right _ ->
      return $ Left "Not RSA Key!"

sendGetCertificateRequest' :: CompanyId -> Text -> Text -> RetrievalId -> Handler (Either Text Text)
sendGetCertificateRequest' companyId customerId customerName retrievalId = do
  mgr <- liftIO mkMngrDefaultTls
  preparedRequest <- liftIO $ prepareGetCertificateRequest companyId customerId customerName retrievalId

  -- Change the site according to the companyId, 2 is for testing
  let site = if companyId == toSqlKey 2 || companyId == toSqlKey 5 then testSite else productionSite

  checkedResponse <- liftIO $ sendSoapRequest' site preparedRequest mgr findAndComposeXmlContent

  let resultGetCertificateRequest = parseTxt parseGetCertificateRequestResponse' $ toStrictUtf checkedResponse

  case resultGetCertificateRequest of
    Right x -> do
      let newCertificate = T.unpack $ createPemWithBeginAndEnd (Data.Text.replace "\n" "" x)
      let pems = readPEM newCertificate
      let cert = Data.X509.decodeSignedObject $ pemContent $ Data.List.head pems

      x509 <- liftIO $ readX509 newCertificate

      validity <- liftIO $ getNotAfter x509
      liftIO $ putStrLn $ "Certificate is valid until: " ++ show validity
      current <- liftIO getCurrentTime
      liftIO $ putStrLn $ "Current time is: " ++ show current
      let earliestRenewalDay = addUTCTime (-60 * nominalDay) validity
      liftIO $ putStrLn $ "Earliest renewal day is: " ++ show earliestRenewalDay

      case cert of
        Right cert -> do
          let cert' = X509.getCertificate cert
          liftIO $ putStrLn "Certificate value extracted OK"
          let publicKey = certPubKey cert'
          case publicKey of
            PubKeyRSA pubKey' -> do
              liftIO $ putStrLn "RSA PublicKey found OK"

              -- Extract security record of a company
              security <- runDB $ selectList [SecurityInfoCompanyId ==. companyId] [] :: Handler [Entity SecurityInfo]
              securityEntity <- case security of
                (x : xs) -> return x
                _ -> sendResponseStatus status404 ("Security info was not found" :: Text)
              liftIO $ putStrLn "Security entity found OK"

              -- Get the potential private key from the db
              let pvtKeyPotential' = securityInfoPotentialprivatekey (entityVal securityEntity)
              case pvtKeyPotential' of
                Just x -> do
                  let (pvtKeyPotential : _) = readKeyFileFromMemory x
                  let k = recover "" pvtKeyPotential
                  liftIO $ putStrLn "Potential key recovered OK"

                  case k of
                    Left err -> do
                      liftIO $ putStrLn $ "Unable to recover key: " ++ show err
                      return $ Left (pack $ show err)
                    -- We found an RSA private key from the db
                    Right (PrivKeyRSA privKey) -> do
                      liftIO $ putStrLn "Processing private keys started"

                      let pubKey = private_pub privKey
                      -- This data refers to the public part of the private key
                      let public_size_pvt = public_size pubKey
                      let public_n_pvt = public_n pubKey
                      let public_e_pvt = public_e pubKey

                      -- This data refers to the public key found in the certificate
                      let public_size_pub = public_size pubKey'
                      let public_n_pub = public_n pubKey'
                      let public_e_pub = public_e pubKey'

                      -- If the public part of the private key match with the public key found in the certificate
                      -- we can update the database
                      if public_size_pvt == public_size_pub && public_n_pvt == public_n_pub && public_e_pvt == public_e_pub
                        then do
                          runDB $ do
                            update (entityKey securityEntity) [SecurityInfoCertificate =. Just (Import.fromString newCertificate)]
                            update (entityKey securityEntity) [SecurityInfoCertificatevalid =. Just validity]
                            update (entityKey securityEntity) [SecurityInfoCurrentprivatekey =. Just x]
                            update (entityKey securityEntity) [SecurityInfoPotentialprivatekey =. Nothing]
                          liftIO $ putStrLn "Processing private keys finished"

                          liftIO $ putStrLn "New certificate checked for integrity with the private key and stored to database"
                          return $ Right $ pack newCertificate
                        else do
                          liftIO $ putStrLn "Processing private keys finished with an error"
                          return $ Left ("Potential private key and certificate do not match" :: Text)
                    Right _ -> return $ Left ("Private key is not RSA private key" :: Text)
                Nothing -> do
                  liftIO $ putStrLn "No potential key found in the database"
                  return $ Left ("No potential key found in the database" :: Text)
            _ -> do
              liftIO $ putStrLn "No RSA public key found from the certificate"
              sendResponseStatus status404 ("No RSA public key found from the certificate" :: Text)
        Left info -> do
          liftIO $ putStrLn "Could not decode the certificate"
          sendResponseStatus status404 ("Could not decode the certificate" :: Text)
    Left exp -> do
      let message = "getCertificateRequest failed"
      liftIO $ print exp
      return $ Left $ pack (show exp)

runRenewCertificate :: CompanyId -> Handler (Either Text.Text Text.Text)
runRenewCertificate companyId =
  if fromSqlKey companyId /= 2 &&  fromSqlKey companyId /= 1
    then sendResponseStatus status404 ("Illegal operation" :: Text)
    else do
      company <- runDB $ get404 companyId
      let customerName = companyName company
      let customerId = companyCompanyid company

      retrievalId <- sendRenewCertificateRequest' companyId customerId customerName
      case retrievalId of
        Right retrievalId -> do
          liftIO $ putStrLn "Started to retrieve certificate (renewal)"
          liftIO $ putStrLn $ "RetrievalId is " ++ (show retrievalId)
          sendGetCertificateRequest' companyId customerId customerName retrievalId
          return $ Right ("Certificate successfully renewed" :: Text.Text)
        Left s -> return $ Left s

runSignNewCertificate ::
  CompanyId ->
  Handler (Either Text.Text Text.Text)
runSignNewCertificate companyId = do
  print "here we go!"
  if fromSqlKey companyId /= 2 && fromSqlKey companyId /= 5 && fromSqlKey companyId /= 1
    then sendResponseStatus status404 ("Illegal operation" :: Text)
    else do
      if companyId == toSqlKey 2 then
        initSecurityRecord companyId
      else initSecurityRecord2 companyId

      company <- runDB $ get404 companyId

      -- Get transferId and transferPassword
      let transferId = companyTransferid company
      let transferPassword = companyTransferpassword company
      print transferId
      print transferPassword

      if (transferId == Nothing) || (transferPassword == Nothing)
        then sendResponseStatus status404 ("Cannot find transferId and transferPassword" :: Text)
        else do
          let customerName = companyName company
          let customerId = companyCompanyid company
          print customerId
          retrievalId <- sendSignNewCertificateRequest' companyId customerId customerName (fromJust transferId) (fromJust transferPassword)

          -- If we get retrievalId we have a certificate ready to be picked up
          case retrievalId of
            Right retrievalId -> do
              liftIO $ putStrLn "Started to retrieve certificate"
              sendGetCertificateRequest' companyId customerId customerName retrievalId
              liftIO $ putStrLn "Certificate retrieval complete"
              return $ Right "A new certificate has been successfully fetched"
            Left s -> return $ Left s

sendReport :: Text -> IO ()
sendReport vatDeclaration =
  return ()

parseTxt :: MonadThrow m => Sink Event m b -> Text -> m b
parseTxt p txtIn =
    runConduit $ yield txtIn .| Text.XML.Stream.Parse.parseText def .| p

{-   yield txtIn
    $= Text.XML.Stream.Parse.parseText def $$ p
 -}
parseSignature :: MonadThrow m => ConduitM Event o m (Maybe Text)
parseSignature =
  do
    signatureContent <- force "Signature tag missing" $
      tagIgnoreAttrs "{http://www.w3.org/2000/09/xmldsig#}Signature" $
        do
          signedInfo <- force "SignedInfo tag missing" $
            tagIgnoreAttrs "{http://www.w3.org/2000/09/xmldsig#}SignedInfo" $
              do
                canonicalizationAlgorithm <- force "CanonicalizationMethod tag missing" $ tag' "{http://www.w3.org/2000/09/xmldsig#}CanonicalizationMethod" (requireAttr "Algorithm") $ \alg -> return alg
                signatureAlgorithm <- force "SignatureMethod tag missing" $ tag' "{http://www.w3.org/2000/09/xmldsig#}SignatureMethod" (requireAttr "Algorithm") $ \alg -> return alg
                force "Reference tag missing" $
                  tagIgnoreAttrs "{http://www.w3.org/2000/09/xmldsig#}Reference" $
                    do
                      transformaAlgorithms <- force "Transforms tag missing" $ tagIgnoreAttrs "{http://www.w3.org/2000/09/xmldsig#}Transforms" $ many' parseTransform
                      digestMethod <- force "DigestMethod tag missing" $ tag' "{http://www.w3.org/2000/09/xmldsig#}DigestMethod" (requireAttr "Algorithm") $ \alg -> return alg
                      force "DigestValue tag missing" $ tagIgnoreAttrs "{http://www.w3.org/2000/09/xmldsig#}DigestValue" content
          signatureValue <- force "SignatureValue tag missing" $ tagIgnoreAttrs "{http://www.w3.org/2000/09/xmldsig#}SignatureValue" content
          remoteCertificate <-
            force "KeyInfo tag missing" $
              tagIgnoreAttrs "{http://www.w3.org/2000/09/xmldsig#}KeyInfo" $
                force "X509Data tag missing" $
                  tagIgnoreAttrs "{http://www.w3.org/2000/09/xmldsig#}X509Data" $
                    force "X509Certificate tag missing" $ tagIgnoreAttrs "{http://www.w3.org/2000/09/xmldsig#}X509Certificate" content
          return signedInfo
    return $ Just signatureContent

parseGetCertificateRequestResponse' :: MonadThrow m => ConduitM Event o m Text
parseGetCertificateRequestResponse' =
  force "No Envelope" $
    tagIgnoreAttrs "{http://schemas.xmlsoap.org/soap/envelope/}Envelope" $
      force "No Body" $
        tagIgnoreAttrs "{http://schemas.xmlsoap.org/soap/envelope/}Body" $
          force "No CertificateResponse" $
            tagIgnoreAttrs "{http://certificates.vero.fi/2017/10/certificateservices}GetCertificateResponse" $ do
              certificate <- force "Certificate tag missing" $ tagIgnoreAttrs "Certificate" content
              status' <-
                force "Result tag missing" $
                  tagIgnoreAttrs "Result" $
                    force "Status tag missing" $ tagIgnoreAttrs "Status" content
              errors <- case status' of
                "FAIL" -> force "ErrorInfo tag missing" $ tagIgnoreAttrs "{http://www.w3.org/2000/09/xmldsig#}Transforms" $ many' parseErrorInfo
                _ -> return []
              signatureContent <- parseSignature
              return certificate

parseTransform :: MonadThrow m => ConduitT Event o m (Maybe Text)
parseTransform = tag' "Transform" (requireAttr "Algorithm") $ \alg ->
  return alg

parseErrorInfo :: MonadThrow m => ConduitT Event o m (Maybe Text)
parseErrorInfo = do
  code <- force "ErrorCode tag missing" $ tagIgnoreAttrs "ErrorCode" content
  message <- force "ErrorMessage tag missing" $ tagIgnoreAttrs "ErrorMessage" content
  return $ Just (code <> message)

-- Namespace- and attribute- ignorant tagNoAttr.
laxTag :: (MonadThrow m) => Text -> ConduitM Event Void m a -> ConduitM Event Void m (Maybe a)
laxTag ln = tag' (matching $ (== ln) . nameLocalName) ignoreAttrs . const

-- Non-maybe version of laxTag/tagNoAttr.
flaxTag :: (MonadThrow m) => Text -> ConduitM Event Void m a -> ConduitM Event Void m a
flaxTag ln s = force ("got no " ++ show ln) $ laxTag ln s

laxContent :: (MonadThrow m) => Text -> ConduitM Event Void m (Maybe Text)
laxContent ln = laxTag ln content

flaxContent :: (MonadThrow m) => Text -> ConduitM Event Void m Text
flaxContent ln = flaxTag ln content

-- Unpack and read a current tag content.
readContent :: (Read a, MonadThrow m) => ConduitM Event Void m a
readContent = fmap (read . unpack) content

-- Unpack and read tag content by local name.
readTag :: (Read a, MonadThrow m) => Text -> ConduitM Event Void m a
readTag n = flaxTag n readContent

parseRenewCertificateRequestResponse' :: MonadThrow m => ConduitM Event o m RetrievalId
parseRenewCertificateRequestResponse' =
  force "No Envelope" $
    tagIgnoreAttrs "{http://schemas.xmlsoap.org/soap/envelope/}Envelope" $
      force "No Body" $
        tagIgnoreAttrs "{http://schemas.xmlsoap.org/soap/envelope/}Body" $
          force "No RenewCertificateResponse" $
            tagIgnoreAttrs "{http://certificates.vero.fi/2017/10/certificateservices}RenewCertificateResponse" $ do
              retrievalId <- force "Retrieval tag missing" $ tagIgnoreAttrs "RetrievalId" content
              status' <- force "Result tag missing" $ tagIgnoreAttrs "Result" $ force "Status tag missing" $ tagIgnoreAttrs "Status" content
              errors <- case status' of
                "FAIL" -> force "ErrorInfo tag missing" $ tagIgnoreAttrs "{http://www.w3.org/2000/09/xmldsig#}Transforms" $ many' parseErrorInfo
                _ -> return [] --Assumin "OK"
              signatureContent <- parseSignature
              return $ RetrievalId retrievalId

parseSignNewCertificateRequestResponse' :: MonadThrow m => ConduitM Event o m RetrievalId
parseSignNewCertificateRequestResponse' =
  force "No Envelope" $
    tagIgnoreAttrs "{http://schemas.xmlsoap.org/soap/envelope/}Envelope" $
      force "No Body" $
        tagIgnoreAttrs "{http://schemas.xmlsoap.org/soap/envelope/}Body" $
          force "No SignNewCertificateResponse" $
            tagIgnoreAttrs "{http://certificates.vero.fi/2017/10/certificateservices}SignNewCertificateResponse" $ do
              retrievalId <- force "Retrieval tag missing" $ tagIgnoreAttrs "RetrievalId" content
              status' <-
                force "Result tag missing" $
                  tagIgnoreAttrs "Result" $
                    force "Status tag missing" $ tagIgnoreAttrs "Status" content
              errors <- case status' of
                "FAIL" -> force "ErrorInfo tag missing" $ tagIgnoreAttrs "{http://www.w3.org/2000/09/xmldsig#}Transforms" $ many' parseErrorInfo
                _ -> return []
              signatureContent <- parseSignature
              return $ RetrievalId retrievalId
