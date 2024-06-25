{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- | Common handler functions.
module Handler.Bookkeeping.InternalReporting.BalanceRecalc where
import Import
import Database.Persist.Sql (rawQuery,toSqlKey, fromSqlKey)
import Data.Foldable
import Data.Conduit (($$))
import Data.Conduit.List as CL

import Handler.Bookkeeping.InternalReporting.Utils
import Data.List (nub)

postRecalcBalancesR' :: CompanyId -> Handler Value
postRecalcBalancesR' companyId = do
 
    let companyId = toSqlKey (1)
--    info <- requireCheckJsonBody :: Handler ReportInfo
    let s = "2022-01-01"
    let e = "2022-12-31"
    putStrLn "Sterted ok"
    let daysOk =((,)) <$>parseDay' (s)<*>parseDay' (e)
    case daysOk of
        Just (f,l)-> runDB $ do

            let info' = ReportInfoDay f l companyId
            {- let sql = "SELECT account.id as account_id,account.name, SUM(entry.amount) " ++
                        "FROM account INNER JOIN entry ON account.id=entry.account WHERE " ++
                        " (entry.transaction_id IN" ++
                        " (select id from transaction where date BETWEEN ? AND ? AND company_id=?)) group by account_id"

            let  parameters =  [ toPersistValue  (firstDay info')
                               , toPersistValue  (lastDay info')
                               , toPersistValue  (companyId' info')]
             -}



            keys <- selectKeys [TransactionDate>=. (f),
                                            TransactionDate<=. (l),
                                            TransactionCompanyId==. (companyId)] [] $$ CL.consume
            entries <- selectList [EntryTransactionId <-. keys] []
            let uniqueAccounts = nub $ Import.map (\(Entity _ item)-> entryAccountId item) entries
            let entries' = Import.map (\(Entity _ item)-> item) entries


            let accountBalances = Import.map (\accountId-> do
                                            let sameAccountEntries = Import.filter (\entry->entryAccountId entry ==accountId) entries' 
                                            let balance = foldl (\acc item ->(round'(acc+entryAmount item))) 0 sameAccountEntries

                                            (
                                              accountId,
                                              balance) 

                                            ) (uniqueAccounts)

            result <- Import.mapM (\account-> updateWhere [AccountId==.fst account][AccountBalance =. (snd account)] ) accountBalances               
              
            return ()         
           
--            putStrLn (pack (show entries'))
        Nothing -> return ()     
    sendResponseStatus status200 ("RECALC DONE" :: Text)


            


            

       --     entries <- runDB $ rawQuery sql parameters $$ CL.map convertFromPersistent  =$ CL.consume
{-             runDB $ do
              entries <- rawQuery sql parameters $$ CL.map convertFromPersistent  =$ CL.consume
              putStrLn (pack (show entries)) 
              let entries' =  Import.foldr (\entry acc->
                      case entry of
                          (x:xs) -> acc++[x {accountBalance=accountBalance x}]
                          [] -> acc
                      ) [] entries
  
              let accountBalances = Import.map (\item-> 
                                    AccountBalance {
                                      accountBalanceAccountId=toSqlKey (fromIntegral(accountId item)),
                                      accountBalanceCompanyId=companyId,
                                      accountBalanceBalance = accountBalance item}

                                    ) entries'
  
 -}{-             setState :: (MonadIO m) => Text -> Text -> ByteString -> SqlPersistT m ()
          setState m k v = either
              (\_  -> return ())
              (\k' -> repsert (k' :: Key SimpleState) (SimpleState m k v))
              (keyFromValues [PersistText m, PersistText k])
 -}

     
    --toJSON <$> (fillBalanceSheet reportTemplate info)
