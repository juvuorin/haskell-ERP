{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Common handler functions.
module Handler.Bookkeeping.InternalReporting.Utils where

import Import
    ( fromIntegral,
      Show,
      Generic,
      Bool(True),
      Double,
      Int,
      Maybe,
      String,
      Text,
      defaultTimeLocale,
      parseTimeM,
      FromJSON,
      ToJSON,
      PersistValue(PersistDouble, PersistInt64, PersistText),
      Day,
      CompanyId )

data AccountBalanceTriplet = AccountBalanceTriplet
  { accountId :: Int,
    accountName :: Text,
    accountBalance :: Double
  }
  deriving (Generic, Show)

instance ToJSON AccountBalanceTriplet

data ReportInfoDay = ReportInfoDay {firstDay :: Day, lastDay :: Day, companyId' :: CompanyId} deriving (Show, Generic)

data ReportInfo = ReportInfo {firstDate :: String, lastDate :: String, companyId :: CompanyId} deriving (Show, Generic)

instance FromJSON ReportInfo

instance FromJSON ReportInfoDay

--convertFromPersistent [] = []
convertFromPersistent [PersistInt64 a, PersistText b, PersistDouble c] =
   [(AccountBalanceTriplet (fromIntegral a) b c)]
convertFromPersistent _ = []

data TemplateRowType = P Int [(Int,Int)] | S Int String [(Int,Int)] | H Int String | R Int (Int, Int) {- | S Int (Int, Int) -} | A Int Int | L Int [Int] deriving (Show, Generic)

data RowType = HeaderWithAmount {tabIndex :: Int, header :: String, amount::Double} | Header {tabIndex :: Int, header :: String} | AccountInfo {tabIndex :: Int, accountInfo :: AccountBalanceTriplet} deriving (Generic, Show)

instance ToJSON RowType

parseDay' :: String -> Maybe Day
parseDay' = parseTimeM True defaultTimeLocale "%Y-%-m-%-d"
