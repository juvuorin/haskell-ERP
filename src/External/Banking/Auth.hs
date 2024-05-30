{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module External.Banking.Auth where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import External.Banking.Types
import External.Utils (mkMngr)
import Handler.Banking.Utils
import Import hiding (httpLbs, newManager)
import Network.HTTP.Client (httpLbs, newManager)

mkAccountAccessConsentsRequest =
  AccountAccessConsentsRequest
    { dataData = AccountAccessConsentsRequestData {dataPermissions = ["ReadAccountsBasic"]}
    }

accountAccessConsentsRequest :: Token -> Handler (Maybe AccountAccessConsentsRequestResponse)
accountAccessConsentsRequest token = do
  accountAccessConsentsEndpoint <- getSetting appAccountAccessConsentsEndpoint

  initRequest <- parseRequest (unAccountAccessConsentsEndpoint accountAccessConsentsEndpoint)
  let request =
        initRequest
          { method = "POST",
            requestHeaders =
              [ ("Authorization", "Bearer " ++ BS.pack (unToken token)),
                ("Content-Type", "application/json"),
                ("X-API-Key", "98ebc8a1-1b61-4856-bbc3-f65a0bee9b8a")
              ],
            requestBody =
              RequestBodyLBS $ encode (toJSON mkAccountAccessConsentsRequest)
          }

  manager <- liftIO $ tlsManager
  r <- liftIO $ httpLbs request manager
  let response = (decode $ responseBody r)
  case response of
    Just x -> return x
    Nothing -> sendResponseStatus status404 ("Could not parse AccountAccessConsentsRequestResponse" :: Text)

tlsManager = do
  managerSettings <- mkMngr "" "certificates/Banking/TLS/Certificate.pem" "certificates/Banking/TLS/privatekey.pem"
  newManager managerSettings

authenticationTokenRequest :: Scope -> Handler (Maybe Token)
authenticationTokenRequest scope = do
  clientId <- getSetting appClientId
  tokenEndpoint <- getSetting appTokenEndpoint
  let initReq = parseRequest_ $ unTokenEndpoint tokenEndpoint
  let request' =
        initReq
          { method = "POST",
            requestHeaders =
              [("Content-Type", "application/x-www-form-urlencoded")]
          }
  let request =
        urlEncodedBody
          [ ("grant_type", "client_credentials"),
            ("scope", fromString $ unScope scope),
            ("client_id", fromString $ unClientId clientId)
          ]
          $ request'
  response <- liftIO $ httpLbsTls request
  let authenticationResponse = (decode $ responseBody response)
  case authenticationResponse of
    Just r -> return $ Just $ Token $ access_token (r :: AuthenticationRequestResponse)
    Nothing -> sendResponseStatus status404 ("Could not parse authentication token" :: Text)

httpLbsTls request = do
  manager <- liftIO $ tlsManager
  liftIO $ httpLbs request manager
