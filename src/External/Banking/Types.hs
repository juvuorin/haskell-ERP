{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module External.Banking.Types where

import Data.Aeson
import Data.Aeson.Casing
import Import

type Permissions = [String]

data AccountAccessConsentsRequestData = AccountAccessConsentsRequestData
  { dataPermissions :: Permissions
  }
  deriving (Show, Generic)

instance ToJSON AccountAccessConsentsRequestData where
  toJSON = genericToJSON $ aesonPrefix id

instance FromJSON AccountAccessConsentsRequestData where
  parseJSON = genericParseJSON $ aesonPrefix id

data AccountAccessConsentsRequest = AccountAccessConsentsRequest
  { dataData :: AccountAccessConsentsRequestData
  }
  deriving (Show, Generic)

instance ToJSON AccountAccessConsentsRequest where
  toJSON = genericToJSON $ aesonPrefix id

instance FromJSON AccountAccessConsentsRequest where
  parseJSON = genericParseJSON $ aesonPrefix id

data AccountAccessConsentsRequestResponseData = AccountAccessConsentsRequestResponseData
  { dataConsentId :: String, --"string"
    dataCreationDateTime :: String, --"2019-08-24T14:15:22Z",
    dataStatus :: String, --"Authorised",
    dataStatusUpdateDateTime :: String, --"2019-08-24T14:15:22Z"
    dataPermissions :: Permissions, -- ["ReadAccountsBasic"]
    dataExpirationDateTime :: String -- "2019-08-24T14:15:22Z",
  }
  deriving (Show, Generic)

instance ToJSON AccountAccessConsentsRequestResponseData where
  toJSON = genericToJSON $ aesonPrefix id

instance FromJSON AccountAccessConsentsRequestResponseData where
  parseJSON = genericParseJSON $ aesonPrefix id

data AccountAccessConsentsRequestResponse = AccountAccessConsentsRequestResponse
  { dataData :: AccountAccessConsentsRequestResponseData
  }
  deriving (Show, Generic)

instance ToJSON AccountAccessConsentsRequestResponse where
 toJSON = genericToJSON $ aesonPrefix id

instance FromJSON AccountAccessConsentsRequestResponse where
 parseJSON = genericParseJSON $ aesonPrefix id

data AuthenticationRequestResponse = AuthenticationRequestResponse
  { access_token :: String,
    scope :: String,
    token_type :: String,
    expires_in :: Int
  }
  deriving (Show, Generic)

instance FromJSON AuthenticationRequestResponse

data AuthenticationRequest = AuthenticationRequest
  { request :: String, --xxx
    response_type :: String, --code+id_token
    scope :: String, --openid+payments
    redirect_uri :: String, -- https%3A%2F%2Fwww.your.server.com%2Fcallback
    state :: String, -- 4UKv2qPRu-z95dO9IA1nDgn_903P5vQ2bF7pXciH82Q
    client_id :: String, --0cd0beff-xxxx-4b2a-xxxx-a440f17xxxc294
    ui_locales :: String --sv-FI+fi
  }
  deriving (Show, Generic)

instance ToJSON AuthenticationRequest where
  toJSON = genericToJSON $ aesonPrefix id

instance FromJSON AuthenticationRequest where
  parseJSON = genericParseJSON $ aesonPrefix id

data AccessTokenRequestResponse = AccessTokenRequestResponse
  { access_token :: String,
    scope :: String,
    token_type :: String,
    expires_in :: Int,
    refresh_token :: String,
    id_token :: String
  }
  deriving (Show, Generic)

instance ToJSON AccessTokenRequestResponse

instance FromJSON AccessTokenRequestResponse

