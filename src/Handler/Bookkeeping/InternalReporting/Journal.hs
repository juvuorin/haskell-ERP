{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Bookkeeping.InternalReporting.Journal where

import Data.Conduit (($$))
import Data.Conduit.List as CL
import Database.Persist.Sql (rawQuery)
import Handler.Bookkeeping.InternalReporting.VatReport (ReportInfo, companyId, firstDate, lastDate, parseDay')
import Import

data ReportInfoDay = ReportInfoDay {firstDay :: Day, lastDay :: Day, companyId' :: CompanyId} deriving (Show, Generic)

instance FromJSON ReportInfoDay

data JournalEntry = JournalEntry {date :: Day, docId :: Int, account :: Int, accountName :: Text, memo :: Text, amount :: Double, entryType :: JournalEntryType} deriving (Generic, Show)

data JournalEntryType = D | K deriving (Generic, Show)
instance ToJSON JournalEntry

instance ToJSON JournalEntryType
--instance FromJSON JournalEntryType
--instance Show JournalEntryType

--ok
convertFromPersistent [] = []
-- "SELECT  transaction.date,transaction.id,account.code,account.name, transaction.memo,  entry.amount 

convertFromPersistent [PersistDay a, PersistInt64 b, PersistInt64 c, PersistText d, PersistText e, PersistDouble f] =
  [(JournalEntry a (fromIntegral b) (fromIntegral c) d e f (dk f))]
convertFromPersistent _ = []

dk x = if (x > 0) then D else K

postJournalR :: CompanyId -> Handler Value
postJournalR id = do
  info <- requireJsonBody :: Handler ReportInfo
  let daysOk = (\first last -> (first, last)) <$> (parseDay' (firstDate info)) <*> (parseDay' (lastDate info))
  case daysOk of
    Just (first, last) -> do
      let info' = ReportInfoDay first last (companyId info)
      let sql =
            "SELECT  transaction.date,transaction.id,account.code,account.name, transaction.memo,  entry.amount "
              ++ "FROM entry JOIN transaction ON entry.transaction_id = transaction.id "
              ++ "JOIN account ON entry.account_id = account.id "
              ++ "WHERE "
              ++ "(entry.transaction_id IN (select id from transaction where date BETWEEN ? AND ? AND company_id=?)) AND"
              ++ "(account.code>999 AND account.code<10000) group by account.code, transaction.date, entry.amount, transaction.memo,account.name,transaction.id order by transaction.date, account.code"
      let parameters =
            [ toPersistValue (firstDay info'),
              toPersistValue (lastDay info'),
              toPersistValue (companyId' info')
            ]
      entries <- runDB $ rawQuery sql parameters $$ CL.concatMap (convertFromPersistent) =$ CL.consume
      return (toJSON $ entries)
    Nothing -> return $ (object ["error" .= ("Could not create journal" :: Text)])
