{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-#  FlexibleInstances #-}
module External.Finvoice.Maventa where
import Import hiding (httpLbs, map, fromList,newManager)
import           External.Utils                  (mkMngr, mkMngrDefaultTls)
import           Network.HTTP.Client             (httpLbs, newManager)
import qualified Data.ByteString.Lazy.UTF8 as LBS
import Data.Aeson
import Crypto.JWT hiding (header)
import Control.Monad.Except ( runExceptT )
import Data.Finvoice30
import External.Finvoice.Invoice
import Data.Aeson.Encode.Pretty
import Text.XML.HaXml
import Network.HTTP.Client.MultipartFormData (formDataBody, partFileRequestBody, partFileSource, partBS)
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Text.XML.HaXml.Schema.Schema as Schema hiding (element)
import qualified Data.List
import Text.PrettyPrint hiding (double, int)
import Text.XML.HaXml.Pretty (element, content)
import Text.XML.HaXml.Posn (noPos)
import Text.XML.HaXml.Parse (xmlParse')
import Text.XML.HaXml.Types (Document (Document))
import Data.Text.Encoding (decodeASCII)
import Data.XML.Types
import qualified Text.XML.HaXml.Pretty as P
import Text.PrettyPrint.HughesPJ hiding ( int, double )
import qualified Prelude
import Text.Pretty.Simple (pPrint)
import Handler.Banking.Utils

baseUrl = "https://ax-stage.maventa.com"
tokenEndpoint =baseUrl++"/oauth2/token"
sendEndpoint = baseUrl++"/v1/invoices" 
profileEndpoint = baseUrl++"/v1/company/profiles" 
invoicesEndpoint = baseUrl++"/v1/invoices" 

statusEndpoint =baseUrl++"/status/authenticated"
companyAuthorizationStatusEndpoint =baseUrl++"/v1/companies/5a301110-db65-48fe-8993-61db26939d74/status"

data MaventaEnvironment = MaventaEnvironment {id::Text, name::Text, values::[KeyValueEnabledTriplet]} 
  deriving (Generic, Show)
data KeyValueEnabledTriplet = KeyValueEnabledTriplet {key::Text, value::Text, enabled::Bool} 
  deriving (Generic, Show)

data AuthenticationRequestResponse =
  AuthenticationRequestResponse
    { access_token :: String
    , token_type   :: String
    , expires_in   :: Int
    }
  deriving (Show, Generic)

instance FromJSON External.Finvoice.Maventa.AuthenticationRequestResponse


instance FromJSON MaventaEnvironment
instance FromJSON KeyValueEnabledTriplet
json ="{"++
	"\"id\": \"8fcd1855-3d31-4c67-898e-545d527d2d53\","++
	"\"name\": \"EXAMPLE_ENVIRONMENT\","++
	"\"values\": ["++
	"{"++
	"\"key\": \"scope\","++
			"\"value\": \"scope1 scope2 scope3\","++
			"\"enabled\": true"++
		"},"++
		"{"++
			"\"key\": \"client_id\","++
			"\"value\": \"5a301110-db65-48fe-8993-61db26939d74\","++
			"\"enabled\": true"++
		"},"++
		"{"++
			"\"key\": \"client_secret\","++
			"\"value\": \"80254cea-a353-4e5c-846b-a08940375edb\","++
			"\"enabled\": true"++
		"},"++
		"{"++
			"\"key\": \"vendor_api_key\","++
			"\"value\": \"316d377c-db16-4bdd-bf1b-9fa4a0c9d8a2\","++
			"\"enabled\": true"++
		"},"++
		"{"++
			"\"key\": \"baseUrl\","++
			"\"value\": \"https://ax-stage.maventa.com\","++
			"\"enabled\": true"++
		"},"++
		"{"++
			"\"key\": \"access_token\","++
			"\"value\": \"\","++
			"\"enabled\": true"++
		"}"++
	"]"++
 "}"

maventa_scope ="eui"
grant_type= "client_credentials"

authenticationTokenRequest :: Handler (Maybe Token)
authenticationTokenRequest = do

  let initReq = parseRequest_ $ tokenEndpoint
  let request' =
        initReq
          { method = "POST"
          , requestHeaders =
              [("Content-Type", "application/x-www-form-urlencoded")]
          }

  maventa_client_id <- getSetting maventa_client_id
  maventa_client_secret <- getSetting maventa_client_secret
  maventa_vendor_api_key <- getSetting maventa_vendor_api_key

  let request = urlEncodedBody
          [("client_id", fromString $ maventa_client_id),
          ("client_secret",fromString $ maventa_client_secret), 
          ("vendor_api_key",fromString $ maventa_vendor_api_key),
          ("scope" ,maventa_scope),
          ("grant_type" ,grant_type)] $ request'
  
  response <- liftIO $ httpLbsTls request
  let authenticationResponse = (decode $ responseBody response)  
  case authenticationResponse of
    Just r  -> do
      return $ Just $ Token $ access_token (r::AuthenticationRequestResponse)
    Nothing -> sendResponseStatus status404 ("Could not parse authentication token"::Text)

{- authenticationTokenRequest' :: IO (Maybe Token)
authenticationTokenRequest' = do
  print tokenEndpoint
  let initReq = parseRequest_ $ tokenEndpoint
  let request' =
        initReq
          { method = "POST"
          , requestHeaders =
              [("Content-Type", "application/x-www-form-urlencoded")]
          }


  let request = urlEncodedBody
          [("client_id", maventa_client_id),
          ("client_secret",maventa_client_secret), 
          ("vendor_api_key",maventa_vendor_api_key),
          ("scope" ,maventa_scope),
          ("grant_type" ,grant_type)] $ request'
  response <- httpLbsTls request
  print response
  let authenticationResponse = (decode $ responseBody response)  
  case authenticationResponse of
    Just r  -> do
      return $ Just $ Token $ access_token (r::AuthenticationRequestResponse)
    Nothing -> return Nothing 

 -}
sendProfileRequest' :: Token->IO ()
sendProfileRequest' token = do
  print profileEndpoint
  print $ fromString $ unToken token
  let initReq = parseRequest_ $ profileEndpoint
  let request' =
        initReq
          { method = "GET"
          , requestHeaders =
              [("Authorization", "Bearer "++(fromString $ unToken token))]
          }
  print request'
  response <- httpLbsTls request'
  let responseJson = (decode $ responseBody response)  
  case responseJson of
      Just (Array x) -> putStrLn (decodeUtf8 (toStrict (encodePretty x)))
      x -> print x

sendAuthStatusRequest' :: Token->IO ()
sendAuthStatusRequest' token = do
  print $ fromString $ unToken token
  let initReq = parseRequest_ $ companyAuthorizationStatusEndpoint
  let request' =
        initReq
          { method = "GET"
          , requestHeaders =
              [("Authorization", "Bearer "++(fromString $ unToken token))]
          }
  
  print request'
  response <- httpLbsTls request'
  print response
  let responseJson = (decode $ responseBody response)  
  case responseJson of
      Just (Object x) -> print x
      x -> print x

sendCompanyStatusRequest' :: Token->IO ()
sendCompanyStatusRequest' token = do
  print $ fromString $ unToken token
  let initReq = parseRequest_ $ companyAuthorizationStatusEndpoint

  let request' =
        initReq
          { method = "GET"
          , requestHeaders =
              [("Authorization", "Bearer "++(fromString $ unToken token))]
          }
  
  print request'
  response <- httpLbsTls request'
  print response
  let responseJson = (decode $ responseBody response)  
  case responseJson of
      Just (Object x) -> print x
      x -> print x

sendStatusRequest' :: Token->IO ()
sendStatusRequest' token = do
  print $ fromString $ unToken token
  let initReq = parseRequest_ $ statusEndpoint
  let request' =
        initReq
          { method = "GET"
          , requestHeaders =
              [("Authorization", "Bearer "++(fromString $ unToken token))]
          }
  
  print request'
  response <- httpLbsTls request'
  print response
  let responseJson = (decode $ responseBody response)  
  case responseJson of
      Just (Object x) -> print x
      x -> print x


sendInvoicesRequest' :: Token->IO ()
sendInvoicesRequest' token = do
  print $ fromString $ unToken token
  let initReq = parseRequest_ $ invoicesEndpoint
  let request' =
        initReq
          { method = "GET"
          , requestHeaders =
              [("Authorization", "Bearer "++(fromString $ unToken token))]
          }
  
  print request'
  response <- httpLbsTls request'
  print response
  let responseJson = (decode $ responseBody response)  
  case responseJson of
      Just (Object x) -> print x
      x -> print x

addFinvoiceAttributes x = (replaceTag "Finvoice" `o`
                addAttribute "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance" `o`
                addAttribute "xsi:noNamespaceSchemaLocation" "Finvoice3.0.xsd" `o`
                tag "Finvoice"
                ) (Data.List.head x)

header l =
    let xd = XMLDecl "1.0" 
             ( Just $ EncodingDecl "UTF-8" ) Nothing 
        pro = Prolog ( Just xd ) [] Nothing []
        [ CElem e _ ] =  l
    in  Text.XML.HaXml.Types.Document pro emptyST e []

sendInvoiceRequest' :: SalesInvoiceId->Handler ()
sendInvoiceRequest' salesInvoiceId = do
  !invoice' <- invoice salesInvoiceId
  let !contentRoot = elementToXMLFinvoice invoice'

  let xmlVersionAndEncoding ="<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  
  let xmlString = xmlVersionAndEncoding ++ (verbatim $ addFinvoiceAttributes contentRoot)
  let initReq = parseRequest_ $ sendEndpoint

  request'' <- formDataBody [partFileRequestBody "file" "finvoice_example.xml" $ RequestBodyLBS $ fromString  xmlString] initReq

  result <- authenticationTokenRequest
  token <- case result of 
    Just x -> return x
    Nothing -> sendResponseStatus status404 ("Cannot get token!"::Text)

  let request' = request'' {requestHeaders = (requestHeaders request'') ++                      
              [("Authorization", "Bearer "++(fromString $ unToken token))]}  

  response <- httpLbsTls request'
  let responseJson = (decode $ responseBody response)  
  case responseJson of
      Just (Object x) -> print x
      x -> print x
  
{- testAuth :: IO () 
testAuth = do
  result <- authenticationTokenRequest
  case result of 
    Just x -> print "token ok"
    Nothing -> print "no token"
 -}
testProfile = do
  result <- authenticationTokenRequest
  case result of 
    Just x -> liftIO $ sendProfileRequest' x
    Nothing -> print "not ok"

testStatus = do
  result <- authenticationTokenRequest
  case result of 
    Just x -> liftIO $ sendStatusRequest' x
    Nothing -> print "not ok"

testCompanyStatus = do
  result <- authenticationTokenRequest
  case result of 
    Just x -> liftIO $ sendCompanyStatusRequest' x
    Nothing -> print "not ok"

testSendAuthStatus = do
  result <- authenticationTokenRequest
  case result of 
    Just x -> liftIO $ sendAuthStatusRequest' x
    Nothing -> print "not ok"

testInvoices = do
  result <- authenticationTokenRequest
  case result of 
    Just x -> liftIO $ sendInvoicesRequest' x
    Nothing -> print "not ok"

tlsManager = do
  managerSettings <- mkMngrDefaultTls
  newManager managerSettings

httpLbsTls request = do
  manager <- liftIO $ tlsManager
  liftIO $ httpLbs request manager

