{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnicodeSyntax #-}

module External.Certificate.Types
  ( module External.Certificate.Types,
  )
where

import qualified Data.Text as T

newtype CompanyId = CompanyId {unCompanyId :: Int} deriving (Show)

newtype CustomerId = CustomerId {unCustomerId :: T.Text} deriving (Show)

newtype CustomerName = CustomerName {unCustomerName :: T.Text} deriving (Show)

newtype TransferId = TransferId {unTransferId :: T.Text} deriving (Show)

newtype TransferPassword = TransferPassword {unTransferPassword :: T.Text} deriving (Show)

newtype RetrievalId = RetrievalId {unRetrievalId :: T.Text} deriving (Show)

newtype XmlToBeSigned = XmlToBeSigned {unXmlToBeSigned :: T.Text} deriving (Show)

newtype PrivateKey = PrivateKey {unPrivateKey :: T.Text} deriving (Show)

newtype Certificate = Certificate {unCertificate :: T.Text} deriving (Show)

data StatusMessageRetrievalId = Fail String | Ok RetrievalId
