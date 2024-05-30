{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude   ***REMOVED***-}
{-# LANGUAGE OverloadedStrings   ***REMOVED***-}
{-# LANGUAGE TemplateHaskell     ***REMOVED***-}
{-# LANGUAGE TypeFamilies        ***REMOVED***-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- | Common handler functions.
module Handler.Bookkeeping.InternalReporting.BalanceSheet where
import Import
    ( ($),
      Eq((==)),
      Monad(return),
      Num(abs),
      Show(show),
      Applicative((<*>)),
      Maybe(Nothing, Just),
      pack,
      putStrLn,
      requireCheckJsonBody,
      (++),
      mapM_,
      elem,
      PersistField(toPersistValue),
      YesodPersist(runDB),
      CompanyId,
      ToJSON(toJSON),
      Handler,
      Value,
      (<$>),
      ($$),
      (=$),
      concatMap,
      foldr,
      (.),
      round' )
import Database.Persist.Sql (rawQuery)
import Data.Foldable
import Data.Conduit (($$))
import Data.Conduit.List as CL

import Handler.Bookkeeping.InternalReporting.Utils as U

{- Vastaavaa
  Pysyvät vastaavat
    Aineettomat hyödykkeet
    Aineelliset hyödykkeet
    Sijoitukset
  Vaihtuvat vastaavat
    Vaihto-omaisuus
    Saamiset; lyhyt- ja pitkäaikaiset erikseen
    Rahoitusarvopaperit
    Rahat ja pankkisaamiset

Vastattavaa
  Oma pääoma
    Osake-, osuus- tai muu vastaava pääoma
    Ylikurssirahasto
    Arvonkorotusrahasto
    Käyvän arvon rahasto
    Muut rahastot
    Edellisten tilikausien voitto (tappio)
    Tilikauden voitto (tappio)
    Tilinpäätössiirtojen kertymä
  
    Pakolliset varaukset

  Vieras pääoma
    lyhyt- ja pitkäaikainen erikseen

 -}
postBalanceSheetR :: CompanyId -> Handler Value
postBalanceSheetR id = do
    --sendResponseStatus status200 ("NOT IMPLEMENTED" :: Text)

    info <- requireCheckJsonBody :: Handler ReportInfo
    let reportTemplate = [  H 0 "Vastaavaa",
                            H 2   "Aineettomat hyödykkeet",
                            R 4     (1000,1045),
                            H 2   "Aineelliset hyödykkeet",
                            R 4     (1130, 1136),
                            H 2   "Sijoitukset",
                            R 4     (1200, 1260),
                            H 2   "Vaihtuvat vastaavat",
                            H 4     "Vaihto-omaisuus",
                            R 6       (1400, 1440),
                            H 2   "Saamiset",
                            H 4     "Lyhytaikaiset",
                            R 6       (1500, 1540),
                            H 4     "Pitkäaikaiset",
                            R 6       (1550, 1580),
                            H 2   "Rahoitusarvopaperit",
                            R 4     (1600, 1610),
                            H 2   "Rahat ja pankkisaamiset",
                            R 4     (1700, 1710),
                            H 2   "Muu rahoitusomaisuus",
                            A 4     1546,

                            H 0 "Vastattavaa",
                            H 2 "Oma pääoma",
                            L 4 [2000,2002,2004,2006,2008,2010,2012],
                            A 4 2018,
                            H 4 "Tilikauden voitto (tappio)",
                            A 4 2110,

                            {-                                     /* Vieras pääoma; lyhyt- ja 2400-2490
                            pitkäaikainen erikseen 2300-2390
                            Tilinpäätössiirtojen kertymä
                            Pakolliset varaukset 2130-2150
                            */
                            -}
                            H 2 "Vieras pääoma",
                            H 4 "Lyhytaikainen",
                            R 6 (2300, 2382),
                            H 4 "Pitkäaikainen",
                            R 6 (2200, 2290),
                            H 4 "Tilinpäätössiirtojen kertymä",
                            H 4 "Pakolliset varaukset",
                            R 6 (2140, 2160)]
    toJSON <$> (fillBalanceSheet reportTemplate info)

fillBalanceSheet:: [TemplateRowType]->ReportInfo->Handler [RowType]
fillBalanceSheet reportTemplate info  = do

    let daysOk =((,)) <$>parseDay' (firstDate info)<*>parseDay' (lastDate info)
    case daysOk of
        Just (f,l)-> do
            let info' = ReportInfoDay f l (companyId info)

 {-        let sql = "SELECT account.id as account_id,account.name, SUM(entry.amount) " ++
                        "FROM account INNER JOIN entry ON account.id=entry.account WHERE " ++
                        " (entry.transaction_id IN" ++
                        " (select id from transaction where date BETWEEN ? AND ? AND company_id=?)) AND " ++
                        " (entry.account>999 AND entry.account<3000) group by account_id" -}
            let sql = "SELECT account.code, account.name, SUM(entry.amount) " ++
                          "FROM account INNER JOIN entry ON account.id=entry.account_id WHERE " ++
                          " (entry.transaction_id IN" ++
                          " (select id from transaction where date BETWEEN ? AND ? AND company_id=?)) AND " ++
                          " (entry.account_id IN (select id from account where code>999 AND code<3000))group by account.code,account.name"
  
            let  parameters =  [ toPersistValue  (firstDay info')
                                , toPersistValue  (lastDay info')
                                , toPersistValue  (companyId' info')]

            entries <- runDB $ rawQuery sql parameters $$ CL.map convertFromPersistent  =$ CL.consume
        
--            entries' <- runDB $ rawQuery sql parameters $$ CL.map convertFromPersistent  =$ CL.consume
            let entries' =  Import.foldr (\entry acc->
                      case entry of
                          (x:xs) -> acc++[x {U.accountBalance= round' $ U.accountBalance x}]
--                          (x:xs) -> acc++[x {U.accountBalance=abs $ round' $ U.accountBalance x}]

                          [] -> acc
                      ) [] entries

         
{-             accounts<-runDB $ selectList ([AccountCompanyId==.companyId info
                                         , AccountCode>.999
                                         , AccountCode<.3000]++([AccountBalance<=.(-0.01)] ||. [AccountBalance>=.(0.01)])) []
            
            
            putStrLn(pack (show accounts))

            let entries' = Import.map (\(Entity _ account)-> AccountBalanceTriplet {accountName = Import.accountName account,accountId=accountCode account, accountBalance=Import.accountBalance account}) accounts
 -}
            let buildItems tabIndex (firstAccount, lastAccount) = filtered
                    where filtered = Import.foldr(\item acc->
--                            if accountId item>=firstAccount && accountId item<=lastAccount
                            if (U.accountId item) `Import.elem` [firstAccount..lastAccount]
                            then acc++[AccountInfo tabIndex item]
                            else acc)
                            [] entries'

            let buildSingleItem tabIndex account = filtered
                    where filtered = Import.foldr(\item acc->
                            if account == U.accountId item
                            then acc++[AccountInfo tabIndex item]
                            else acc)
                            [] entries'

            let buildItemsFromList tabIndex list = filtered
                    where filtered = Import.concatMap(buildSingleItem tabIndex) list

                       --Import.map (\item->AccountInfo tabIndex item ) filtered


            let report = foldl (\acc item->
                    case item of
                        H tabIndex header   -> acc++[Header tabIndex  header]
                        A tabIndex account  -> acc++buildSingleItem tabIndex account
                        R tabIndex range    -> acc++buildItems tabIndex range
                        L tabIndex list     -> acc++buildItemsFromList tabIndex list
                        _ -> acc
                    ) [] reportTemplate

            Import.mapM_ (putStrLn . pack . show ) report
            putStrLn(pack $ show $ toJSON report)
--            return (toJSON entries')
            return report
            --return (toJSON report)

        Nothing-> return []



 