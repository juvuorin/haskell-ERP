{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude   ***REMOVED***-}
{-# LANGUAGE OverloadedStrings   ***REMOVED***-}
{-# LANGUAGE TemplateHaskell     ***REMOVED***-}
{-# LANGUAGE TypeFamilies        ***REMOVED***-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- | Common handler functions.
module Handler.Bookkeeping.InternalReporting.GeneralLedger where

import Import hiding (account)

import Database.Persist.Postgresql ()
import Database.Persist.Sql (rawQuery)
import Data.Conduit (($$))
import Data.Conduit.List as CL
import Handler.Bookkeeping.InternalReporting.VatReport (ReportInfo, parseDay', firstDate, lastDate, companyId)
import Data.List (nub,(!!))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as TL


import Data.ByteString            as B
import Data.ByteString.Lazy       as BL
--import Data.Text                  as T
import Data.Text.Encoding         as T
--import Data.Text.IO               as T
import Data.Text.Lazy             as TL
import Data.Text.Lazy.Encoding    as TL
import Data.Text.Lazy.IO          as TL


--import Data.Text.Lazy             as TL
--import Data.Text.Lazy.Encoding    as TL
--import qualified Data.Conduit.Combinators as TL

data ReportInfoDay = ReportInfoDay {firstDay::Day, lastDay::Day, companyId'::CompanyId}     deriving (Show, Generic)
instance FromJSON ReportInfoDay

data GeneralLedgerEntry = GeneralLedgerEntry {account::Int,date::Day, memo::Import.Text, accountName::Import.Text, amount::Double,docId::Int} deriving (Generic, Show)
data GeneralLedgerBrick = GeneralLedgerBrick {entries::[GeneralLedgerBrickEntry],account::Int, accountName::Import.Text } deriving (Generic, Show)
data GeneralLedgerEntryType = D | K deriving (Generic, Show)
data GeneralLedgerBrickEntry = GeneralLedgerBrickEntry {account::Int, date::Day, memo::Import.Text, amount::Double, entryType::GeneralLedgerEntryType, balance::Double,docId::Int } deriving (Generic, Show)

instance ToJSON GeneralLedgerEntry
instance ToJSON GeneralLedgerBrick 
instance ToJSON GeneralLedgerEntryType 
instance ToJSON GeneralLedgerBrickEntry 

convertFromPersistent [] = []
convertFromPersistent [PersistInt64 a,PersistDay b, PersistText c,PersistText d,PersistDouble e, PersistInt64 f] = 
                                [(GeneralLedgerEntry (fromIntegral a) b c d e (fromIntegral f))]
convertFromPersistent _ = []



--data TemplateRowType = H Int String| R Int (Int,Int) | A Int Int | L Int [Int] deriving (Show, Generic)

round'' :: Double -> Double
round'' x = fromIntegral (round $ x * 1e2) / 1e2    
----
postGeneralLedgerR :: CompanyId -> Handler Value
postGeneralLedgerR id = do

    info <- requireJsonBody :: Handler ReportInfo
    let daysOk =(\first last ->(first,last)) <$>(parseDay' (firstDate info))<*>(parseDay' (lastDate info))
    case daysOk of
        Just (f,l)-> do


           {-  let sqlOld = "SELECT entry.account,  transaction.date, transaction.memo, account.name,entry.amount,transaction.id "++
                    "FROM entry JOIN transaction ON entry.transaction_id = transaction.id "++ 
                    "JOIN account ON entry.account = account.id "++
                    "WHERE "++
                    "(entry.transaction_id IN (select id from transaction where date BETWEEN ? AND ? AND company_id=?)) AND"++ 
                    "(entry.account>999 AND entry.account<10000) group by entry.account, transaction.date, entry.amount, transaction.memo,account.name,transaction.id order by entry.account, transaction.date DESC"
 -}
          
            let info' = ReportInfoDay f l (companyId info)
            let sql = "SELECT account.code,  transaction.date, transaction.memo, account.name,entry.amount,transaction.id "++
                    "FROM entry JOIN transaction ON entry.transaction_id = transaction.id "++ 
                    "JOIN account ON entry.account_id = account.id "++
                    "WHERE "++
                    "(entry.transaction_id IN (select id from transaction where date BETWEEN ? AND ? AND company_id=?)) AND"++ 
                    "(account.code>999 AND account.code<10000) order by account.code, transaction.date DESC"
-- Nasty bug here! fixed 23.11.2023   "(account.code>999 AND account.code<10000) group by account.code, transaction.date, entry.amount, transaction.memo,account.name,transaction.id order by account.code, transaction.date DESC"
-- result was grouped by amount and if there was 2 or more same amounts only one was included, as expected!
            let  parameters =
                   [  toPersistValue  (firstDay info')
                    , toPersistValue  (lastDay info')
                    , toPersistValue  (companyId' info')]

--                                    entry.account,  transaction.date, transaction.memo,account.name, entry.amount
                                    
            entries <- runDB $ rawQuery sql parameters $$ CL.concatMap (convertFromPersistent)  =$ CL.consume
            Import.putStrLn "OOOOOKOO"
            let entries'  = Import.map (\item->account (item::GeneralLedgerEntry)) (entries)
            let uniqueAccounts = nub entries' 
            Import.mapM_ (Import.putStrLn . Import.pack . show) uniqueAccounts

            let almostReady = Import.map (\accountId-> do
                 let filtered = Import.filter (\item->account (item::GeneralLedgerEntry)==accountId) entries
                 let accountName' = if (Import.length filtered > 0) then accountName ((filtered!!0)::GeneralLedgerEntry) else "cannot find account!"
                 let t = Import.foldr (\item acc->   do 
                        let bal =round''   (amount (item::GeneralLedgerEntry)+snd acc)  
--                                                        let bal = round' $ (round' $ amount (item::GeneralLedgerEntry))+(round' $ snd acc)
                        (fst acc++[GeneralLedgerBrickEntry
                           {  date=date (item::GeneralLedgerEntry)
                            , memo=memo (item::GeneralLedgerEntry)
                            , account = account (item::GeneralLedgerEntry)
                            , amount= amount (item::GeneralLedgerEntry)
                            , docId = docId (item::GeneralLedgerEntry)
                            , entryType=if (amount (item::GeneralLedgerEntry) > 0) then D else K
                            , balance=bal}],bal)
                        ) ([],0) filtered
                      
                 GeneralLedgerBrick {account=accountId, accountName=accountName', entries=(fst t)}
                                                                                                                               
                 ) uniqueAccounts
            
            Import.putStr $ T.decodeUtf8 . B.concat . BL.toChunks $ encodePretty (almostReady)
            return (toJSON $ almostReady)


        Nothing-> return $ (object ["error" .= ("Could not create balance sheet"::Import.Text)])

