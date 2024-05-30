{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Handler.Banking.Authenticate where
import Import hiding (httpLbs, map, fromList)
import External.Banking.Auth (accountAccessConsentsRequest, tlsManager, httpLbsTls, authenticationTokenRequest)
import External.Banking.Types
import External.Utils
import qualified Data.ByteString.Lazy.UTF8 as LBS
import Data.Aeson
--import Web.JWT
import Crypto.JWT
import Control.Monad.Except
import      Network.HTTP.Client             (httpLbs)

import Data.Aeson.Encode.Pretty
import Text.XML.HaXml (x)
import Handler.Banking.Utils
certFilePath = "certificates/Banking/API/Certificate.pem"
keyFilePath = "certificates/Banking/API/privatekey.pem"
nonce' = "23389ac0-8f46-4a8a-a022-4d1f5eecef63"
state' = "ipLYgl3BqlG3F0-FAqFNEytP-ksBv1shpbfEFlU8LVE"

doJwtSign :: JWK -> ClaimsSet -> IO (Either JWTError SignedJWT)
doJwtSign jwk claims = runExceptT $ do
  let alg = PS256
  signClaims jwk (newJWSHeader ((), alg)) claims

prepareAndSignTokenRequestData :: ConsentId->Scope->Handler (Maybe String)
prepareAndSignTokenRequestData consentId scope = do

  clientId <- getSetting appClientId
  redirectUriAuth <- getSetting appRedirectUriAuth 
 
  let tenant =  "alandsbanken-fi"
  let consent_type = "account"
  let message
        ="{\"scope\":\""++unScope scope++"\","++
          "\"claims\":{\"id_token\":"++
              "{\"openbanking_intent_id\":"++
                "{\"value\":\"urn:"++tenant++":"++consent_type++":"++unConsentId consentId++"\","++
                "\"essential\":true"++
                "}"++
              "}"++
            "},"++
          "\"iss\":\""++unClientId clientId++"\","++
          "\"redirect_uri\":\""++unRedirectUriAuth redirectUriAuth++"\","++
          "\"state\":\""++state'++"\","++
          "\"nonce\":\""++nonce'++"\","++
          "\"client_id\":\""++unClientId clientId++"\"}"
  putStrLn (pack message)

  let claimsJson = (Data.Aeson.decode (fromString message)) :: Maybe ClaimsSet

  case claimsJson of
    Just claimSet -> do
      key <-  liftIO $ privateKeyFromFile keyFilePath
      case key of
        Right x -> do
          signed <- liftIO $ doJwtSign (fromRSA x) claimSet
          case signed of
            Right (x) -> do

              return (Just (LBS.toString (encodeCompact  x)))
            Left s -> sendResponseStatus status404 (show s)
        Left s-> sendResponseStatus status404 (show s)
    Nothing -> sendResponseStatus status404 ("Could not create claims for signing object"::Text)

getAuthenticateR ::  Handler ()
getAuthenticateR = do
  let scope = Scope "openid accounts"
  token' <- authenticationTokenRequest scope
  
  token <- case token' of
    Nothing -> sendResponseStatus status404 ("Could not retrieve authentication token"::Text)
    Just token -> return token
  
  consentRequestResponse <- accountAccessConsentsRequest token
  consentId <- case consentRequestResponse of
        Just (AccountAccessConsentsRequestResponse x) -> do
            return $ dataConsentId x
        Nothing -> sendResponseStatus status404 ("Could not retrieve consent id"::Text)

  clientId <-  getSetting appClientId 
  redirectUriAuth <- getSetting appRedirectUriAuth 
  authEndpoint <-  getSetting appAuthEndpoint 

  signed <- prepareAndSignTokenRequestData (ConsentId consentId) scope

  case signed of
    Just signedContent -> do
      putStrLn $ "signature:" ++ (pack signedContent)
      let content = (pack (unAuthEndpoint authEndpoint)++"?"++
                "request="++pack signedContent++
                "&response_type=code+id_token"++   
                "&scope="++(pack $ unScope scope)++  
                "&redirect_uri="++pack (unRedirectUriAuth redirectUriAuth)++
                "&state="++pack state'++
                "&client_id="++pack ( unClientId clientId)++
                "&nonce="++pack nonce'++
                "&response_mode=query"++
                "&ui_locales=sv-FI+fi") :: Text
      redirectWith status302 (content)
    Nothing -> sendResponseStatus status404  ("Could not sign content"::Text)

getAuthCallbackR ::  Handler ()
getAuthCallbackR = do

  code <- lookupGetParam "code"
  token <- case code of
    Nothing -> sendResponseStatus status404 ("Could not get access code"::Text)
    Just accessCode -> requestToken (AccessCode $ unpack accessCode)

  getAccounts token
  return ()


getAccounts :: String -> Handler ()
getAccounts token = do

  apiEndpoint <-  getSetting appApiEndpoint 
  xApiKey <-  getSetting appXApiKey 

  manager <- liftIO $ tlsManager
  initReq <- liftIO $ parseRequest ((unApiEndpoint apiEndpoint)++"/accounts")

  let request' =
          initReq
            { method = "GET"
            , Import.requestHeaders =
                [("Content-Type", "application/x-www-form-urlencoded")
                ,("Authorization", "Bearer "++fromString token)
                ,("X-API-Key", fromString (unXApiKey xApiKey))]            
            }

  response <- liftIO $ httpLbs request' manager
  let body = responseBody response
  let maybeJson = (decode body) :: Maybe Object
  case maybeJson of
    Just x -> putStrLn $ pack $ LBS.toString (encodePretty x) 
    Nothing -> sendResponseStatus status404 ("Could not get Account object response in json"::Text)

  
requestToken::AccessCode->Handler String
requestToken code = do

  tokenEndpoint <- getSetting appTokenEndpoint
  redirectUriAuth <- getSetting appRedirectUriAuth 
 
  initReq <- parseRequest (unTokenEndpoint tokenEndpoint)

  let request' =
          initReq
            { method = "POST"
            , Import.requestHeaders =
                                    [("Content-Type", "application/x-www-form-urlencoded")]

            , requestBody =
                  RequestBodyLBS $  "grant_type=authorization_code"++
                                    "&redirect_uri="++fromString (unRedirectUriAuth redirectUriAuth) ++
                                    "&code="++  fromString (unAccessCode code)++
                                    "&client_id=qrOlqUPm6IOVjQ-zdg5LdTAGhxCjRI7RkBG8eOOCx8s"

            }

  response <- liftIO $ httpLbsTls request' 
  
  let authenticationResponse = (decode $ responseBody response)  

  case authenticationResponse of
    Just  x  -> return $ access_token (x::AccessTokenRequestResponse)
    Nothing -> sendResponseStatus status404 ("Cannot get access token from the service provider!"::Text)

  
