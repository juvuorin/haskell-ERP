{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Bookkeeping.InternalReporting.VatReport where

import Control.Monad.ST (ST)
import Data.Conduit.List as CL (consume, head)
import qualified Data.List
import Data.Text (pack, replace, unpack)
import Data.Time
import Database.Persist.Sql
  ( fromSqlKey,
    rawQuery,
    toSqlKey,
  )
import External.ApitamoPKI.Apitamo (sendVatReportImmediate)
import Import hiding
  ( account,
    accountBalance,
    pack,
  )
import Text.Printf
import Control.Monad.Logger

data VatReportEventType = SendVatReport | SendAndReplacePreviousVatReport deriving (Show,Eq)


data ReportInfo = ReportInfo
  { firstDate :: String,
    lastDate :: String,
    companyId :: CompanyId
  }
  deriving (Show, Generic)

instance FromJSON ReportInfo
{- 
data ReportInfoDay = ReportInfoDay
  { reportInfoDayFirstDay :: Day,
    reportInfoDayLastDay :: Day,
    reportInfoDayCompanyId :: CompanyId
  }
  deriving (Show, Generic)
 -}
--instance FromJSON ReportInfoDay

data ReportInfoDay = ReportInfoDay
  { firstdate :: Day,
    lastdate ::Day,
    companyid :: CompanyId
  }
  deriving (Show, Generic)

instance FromJSON ReportInfoDay


data TaxResult = TaxResult
  { vat10 :: Double,
    vat14 :: Double,
    vat24 :: Double
  }
  deriving (Show, Generic)

data AccountBalanceTriplet = AccountBalanceTriplet
  { account :: AccountId,
    accountName :: Text,
    accountBalance :: Double
  }
  deriving (Generic, Show)

instance ToJSON AccountBalanceTriplet

getAccountSumWithoutDate ::
  AccountId ->
  CompanyId ->
  ReaderT SqlBackend (HandlerFor App) (Either Text AccountBalanceTriplet)
getAccountSumWithoutDate account companyId {- runDB $ -} =
  do
    let query =
          ( "SELECT account.code, account.name, SUM(entry.amount) "
              ++ "FROM account INNER JOIN entry ON account.id=entry.account_id WHERE "
              ++ "account_id=? AND entry.transaction_id IN "
              ++ "(select id from transaction where company_id=?) group by account_id"
          ) ::
            Text
    let parameters = [toPersistValue account, toPersistValue companyId]
    accountEntry <- get account
    case accountEntry of
      Just x -> do
        entries <- rawQuery query parameters $$ CL.head
        case entries of
          Just [account, name, sum] -> do
            let values =
                  ( \account' name' sum' ->
                      (account' :: AccountId, name' :: Text, sum' :: Double)
                  )
                    <$> fromPersistValue account
                    <*> fromPersistValue name
                    <*> fromPersistValue sum
            case values of
              Right (account_, name_, sum_) ->
                return $ Right (AccountBalanceTriplet account_ name_ sum_)
              Left x -> return $ Left (x :: Text)
          _ ->
            return $
              Right (AccountBalanceTriplet account (Import.accountName x) 0)
      Nothing -> return $ Left ("Query completed with no data" :: Text)

getAccountSum ::
  AccountId ->
  ReportInfoDay ->
  ReaderT SqlBackend (HandlerFor App) (Either Text AccountBalanceTriplet)
getAccountSum account ReportInfoDay {..} = do
  let query =
        ( "SELECT account.code, account.name, SUM(entry.amount) "
            ++ "FROM account INNER JOIN entry ON account.id=entry.account_id WHERE "
            ++ "entry.account_id=? AND entry.transaction_id IN "
            ++ "(select id from transaction where date BETWEEN ? AND ? AND company_id=?) group by account.code,account.name"
        ) ::
          Text
  let parameters =
        [ toPersistValue account,
          toPersistValue (firstdate),
          toPersistValue (lastdate),
          toPersistValue (companyid)
        ]
  $(logInfo) "OOOKOO Phase 1"
  accountEntry <- get account
  $(logInfo) "OOOKOO Phase 2"
  case accountEntry of
    Just x -> do
      entries <- rawQuery query parameters $$ CL.head
      case entries of
        Just [PersistInt64 account_, PersistText name_, PersistDouble sum_] ->
          return $ Right (AccountBalanceTriplet (toSqlKey account_) name_ sum_)
        _ ->
          return $
            Right (AccountBalanceTriplet account (Import.accountName x) 0)
    Nothing -> return $ Left ("Query completed with no data" :: Text)

getAccountSumAbs' ::
  AccountId ->
  ReportInfoDay ->
  DB (Either Text Double)
getAccountSumAbs' account ReportInfoDay{..} {- runDB $ -} =
  do
    $(logInfo) $ "Account:" ++ (pack $ show account)
    let query =
          ( "SELECT SUM(entry.amount) "
              ++ "FROM account INNER JOIN entry ON account.id=entry.account_id WHERE "
              ++ "entry.account_id=? AND entry.transaction_id IN "
              ++ "(select id from transaction where date BETWEEN ? AND ? AND company_id=?) group by account.code,account.name"
          ) ::
            Text
    let parameters =
          [ toPersistValue account,
            toPersistValue (firstdate),
            toPersistValue (lastdate),
            toPersistValue (companyid)
          ]
    accountEntry <- get account
    case accountEntry of
      Just x -> do
        -- Run SQL query
        entries <- rawQuery query parameters $$ CL.head
        case entries of
          -- Should just return one value in an array (one row with one field)
          Just [PersistDouble sum_] -> return $ Right (abs sum_)
          _ -> return $ Right 0
      Nothing ->
        sendResponseStatus
          status500
          ("Account does not exist while running VAT report!2" :: Text) --return $ Left ("Account does not exist"::Text)



parseDay' :: String -> Maybe Day
parseDay' = parseTimeM True defaultTimeLocale "%Y-%-m-%-d"

-- %-d-%-m-%Y
date :: IO (Integer, Int, Int) -- :: (year, month, day)
date = getCurrentTime >>= return . toGregorian . utctDay

date' :: IO String -- "%d%m%Y%H%M%S"
date' =
  getZonedTime
    >>= (return . formatTime defaultTimeLocale "%d%m%Y%H%M%S" . zonedTimeToLocalTime)

{-
Tunnus  P/V T   L/T Kuvaus                                                  Muoto               Sallitut  arvot
000     P   T       Tietuetunnus                                            AN8                 VSRALVKV
198     P       *   Ohjelmiston tuottama aikaleima                          PPKKVVVVHHMMSS
014     P           llmoituksen tuottaneen ohjelmiston yksilöivä tieto      Y-TUNNUS_AN2
048     P           llmoituksen tuottanut ohjelmisto                        AN35
049     P           Vastaanottavan palvelun muodostama sanomatunniste,
                    joka yksilöi sanoman, välityspalvelu täyttää            AN9
010     P   T       Asiakkaan y-tunnus tai henkilötunnus                    YTUNNUS|| HETU
050     P       *   Verokauden pituus
                    K = kuukausi
                    Q = Vuosineljännes
                    V = kalenterivuosi tai poronhoitovuosi                  AN1                 K,Q,V
052     V   T   *   Verokausi Pakollinen tieto kuukausi- ja vuosineljännes
                    -ilmoituksilla.
                    - Verokaudella K kuukausi 1 - 12
                    - Verokaudella Q vuosineljännes 1 – 4
                    - Verokaudella V jätetään ilmoittamatta                 N2
053     P   T   *   Verokauden vuosi                                        VVVV
056     V       *   Ei toimintaa                                            N1                  1

301     V       *   Suoritettava 24%:n vero kotimaan myynnistä              G13,2
302     V       *   Suoritettava 14%:n vero kotimaan myynnistä              G13,2
303     V       *   Suoritettava 10%:n vero kotimaan myynnistä              G13,2
304     V       *   Vero tavaroiden maahantuonnista EU:n ulkopuolelta       G13,2
305     V       *   Vero tavaraostoista muista EU-maista                    G13,2
306     V       *   Vero palveluostoista muista EU-maista                   G13,2
307     V       *   Verokauden vähennettävä vero                            G13,2
308     V       *   Maksettava vero / Palautukseen oikeuttava vero (-)      G13,2
309     V       *   0-verokannan alainen liikevaihto                        G13,2
310     V       *   Tavaroiden maahantuonnit EU:n ulkopuolelta              G13,2
311     V       *   Tavaroiden myynnit muihin EU-maihin                     G13,2
312     V       *   Palveluiden myynnit muihin EU-maihin                    G13,2
313     V       *   Tavaraostot muista EU-maista                            G13,2
314     V       *   Palveluostot muista EU-maista                           G13,2
315     V       *   Alarajahuojennukseen oikeuttava liikevaihto             R13,2
316     V       *   Alarajahuojennukseen oikeuttava vero                    R13,2
317     V       *   Alarajahuojennuksen määrä                               R13,2
336     V           Alarajahuojennusta voi hakea vain tilikauden viimei-
                    senä verokautena tai kalenterivuoden viimeisenä
                    neljänneksenä tai arvonlisäverovelvollisuuden päät-
                                tymiskaudella
                    1=haen, koska kyseessä on tilikauden viimeinen ve-
                    rokausi
                    2=haen, koska kyseessä on kalenterivuoden viimei-
                    nen neljännes
                    3=haen, koska alv-velvollisuuteni on päättynyt tällä
                    verokaudella
                                                                            N1                  1,2,3
337     V           Maksuperusteinen arvonlisävero                          N1                  1
318     V       *   Vero rakentamispalvelun ja metalliromun ostoista        G13,2
319     V       *   Rakentamispalvelun ja metalliromun myynnit              G13,2
320     V       *   Rakentamispalvelun ja metalliromun ostot                G13,2
332     V           Laskuvirhe / Ilmoituksen täyttövirhe                    N1                  1
333     V           Verotarkastuksessa saatu ohjaus                         N1                  1
334     V           Oikeuskäytännön muutos                                  N1                  1
335     V           Laintulkintavirhe                                       N1                  1
042     V           Yhteyshenkilön puhelinnumero                            AN35
999     P           Lopputunnus                                             N8
 -}
{-
let report = [
        { 301: 'Suoritettava 24%:n vero kotimaan myynnistä' },
        { 302: 'Suoritettava 14%:n vero kotimaan myynnistä' },
        { 303: 'Suoritettava 10%:n vero kotimaan myynnistä' },
        { 304: 'Vero tavaroiden maahantuonnista EU:n ulkopuolelta' },
        { 305: 'Vero tavaraostoista muista EU-maista' },
        { 306: 'Vero palveluostoista muista EU-maista' },
        //        307: 'Verokauden vähennettävä vero',
        //        308: 'Maksettava vero / Palautukseen oikeuttava vero (-)',
        { 309: '0-verokannan alainen liikevaihto' },
        { 310: 'Tavaroiden maahantuonnit EU:n ulkopuolelta' },
        { 311: 'Tavaroiden myynnit muihin EU-maihin' },
        { 312: 'Palveluiden myynnit muihin EU-maihin' },
        { 313: 'Tavaraostot muista EU-maista' },
        { 314: 'Palveluostot muista EU-maista' },
        { 318: 'Vero rakentamispalvelun ja metalliromun ostoista' },
        { 319: 'Rakentamispalvelun ja metalliromun myynnit' },
        { 320: 'Rakentamispalvelun ja metalliromun ostot' }
    ]

 -}
{- data Id = Id String

t000  = Id "000"
t198 = Id "198"
t014 = Id "014"
t048 = Id "048"
t010 = Id "010"
t050 = Id "050"
t052 = Id "052"
t053 = Id "052"
t056 = Id "056"
t032 = Id "032"
t301 = Id "301" --ALV 24 suoritettava
t302 = Id "302" --ALV 14 suoritettava
t303 = Id "303" --ALV 10 suoritettava
t305 = Id "305" --Vero tavaraostoista muista EU-maista
t306 = Id "306" --Vero palveluostoista muista EU-maista
            --       ( 312, 3024 ), --Palveluiden myynnit muihin EU-maihin G13,2
t313 = Id "313" --Tavaraostot muista EU-maista G13,2
t314 = Id "314" --Palveluostot muista EU-maista G13,2
t309 = Id "309"  --0-verokannan alainen myynti

 -}

taxAccountFilingMapping :: [(Int, [Int64])]
taxAccountFilingMapping =
  [ (301, [2366]), --ALV 24 suoritettava
    (302, [2367]), --ALV 14 suoritettava
    (303, [2368]), --ALV 10 suoritettava
    (305, [2365]), --Vero tavaraostoista muista EU-maista
    (306, [2363]), --Vero palveluostoista muista EU-maista
    --       ( 312, 3024 ), --Palveluiden myynnit muihin EU-maihin G13,2
    (313, [4040]), --Tavaraostot muista EU-maista G13,2
    (314, [4201]), --Palveluostot muista EU-maista G13,2
    (309, [3021, 3022]) --0-verokannan alainen myynti
  
  ]

--      ( 309, 3022 )] --0-verokannan alainen myynti ulkomaille
--       ( 318, 2378 ),	--	V			*		Vero rakentamispalvelun ja metalliromun ostoista	G13,2
--       ( 319, 3025 ),	--	V			*		Rakentamispalvelun ja metalliromun myynnit	G13,2
--       ( 320, 4017 )]	--	V			*		Rakentamispalvelun ja metalliromun ostot	G13,2

--        ( 304, 2371 ), --Vero tavaroiden maahantuonnista EU:n ulkopuolelt
--        ( 307, 0) -- Verokauden vähennettävä vero                   //    307:Math.abs(totalReceivable),
--        ( 308,0)  --Maksettava vero / Palautukseen oikeuttava vero (-)     Math.abs(totalTax)-Math.abs(totalReceivable),
--        ( 309, 3021 ),--0-verokannan alainen liikevaihto
--        ( 310, 4016 ),--Tavaroiden maahantuonnit EU:n ulkopuolelta
--        ( 311, 3023 ),--Tavaroiden myynnit muihin EU-maihin
--315 V  * Alarajahuojennukseen oikeuttava liikevaihto R13,2
--316 V  * Alarajahuojennukseen oikeuttava vero R13,2
--317 V  * Alarajahuojennuksen määrä R13,2
--336 V   Alarajahuojennusta voi hakea vain tilikauden viimei-
--337 V   Maksuperusteinen arvonlisävero N1 1
data Row
  = Row String Value

(-->) :: (Monoid m, IsString m) => m -> m -> m
(-->) id value = id ++ ":" ++ value ++ "\r\n"

postVatReportR :: CompanyId -> Handler Value
postVatReportR companyId' = do
  runDB $ do
    logEvent
      "Started preparing VAT report for sending to Tax authority"
      (Just companyId')
    $(logInfo) "Start preparing VAT report for sending to Tax authority"    
    info <- liftHandler $ requireCheckJsonBody :: DB ReportInfoDay
  --  let last = lastdate info
    --let (year, month, day) = toGregorian last
--    let endDate = getEndDate last 
    let diff = diffDays (lastdate info) (firstdate info)
    if diff > 31 then sendResponseStatus status404 ("Vat period length must be 31 days of less"::Text)
      else if diff==0 then sendResponseStatus status404 ("Vat period length must be greater than 0 days"::Text)
      else if diff < 0 then sendResponseStatus status404 ("Vat period lenght cannot be negative"::Text) 
      else return ()

    vatTransactions <- selectList [TransactionDate >=. firstdate info,
          TransactionDate <=. lastdate info,
          TransactionCompanyId ==. companyid info,
          TransactionType ==. TypeVatStatement][]
    mapM_ (\transaction-> deleteTransaction $ entityKey transaction) vatTransactions

    processedReport <- selectList [ProcessedVatReportPeriodstart ==. (firstdate info),
                                      ProcessedVatReportPeriodstart ==. (lastdate info),
                                      ProcessedVatReportCompanyId ==. companyId'][]        
  
    vatReportEventType <- case processedReport of
        []-> return SendVatReport 
        _ -> do
          logEvent ("VAT report will be resend to the authority and sales/purchases adjusted for VAT") (Just companyId')
          return SendAndReplacePreviousVatReport
     
    print ("Vat report event type is"++(show vatReportEventType))

    company <- get companyId'
    vatId <-
      case company of
        Just x -> return $ companyCompanyid x
        Nothing ->
          sendResponseStatus
            status404
            ( Data.Text.pack $
                "Company with id " ++ show companyId' ++ " not found"
            )
    keys <-
      selectKeys
        [ TransactionDate >=. firstdate info,
          TransactionDate <=. lastdate info,
          TransactionCompanyId ==. companyid info
        ]
        []
        $$ CL.consume
    entries' <- selectList [EntryTransactionId <-. keys] []
    let entries = Import.map entityVal entries'
    entriesWithAccounts <-
      mapM
        ( \entry -> do
            account <- get404 (entryAccountId entry)
            return (entry, account)
        )
        entries
    allAccounts' <- liftHandler $ getAccountEntities companyId'
    let taxResultDeduction =
          Import.foldr
            ( \entry acc ->
                case accountCode $ snd entry of
                  4000 ->
                    acc
                      { vat24 =
                          vat24 acc
                            + round' (entryAmount (fst entry) * (1 - 1 / 1.24))
                      }
                  4001 ->
                    acc
                      { vat14 =
                          vat14 acc
                            + round' (entryAmount (fst entry) * (1 - 1 / 1.14))
                      }
                  4002 ->
                    acc
                      { vat10 =
                          vat10 acc
                            + round' (entryAmount (fst entry) * (1 - 1 / 1.10))
                      }
                  _ -> acc
            )
            (TaxResult 0 0 0)
            entriesWithAccounts
    let taxResultDebt =
          Import.foldr
            ( \entry acc ->
                case accountCode $ snd entry of
                  3000 ->
                    acc
                      { vat24 =
                          vat24 acc
                            + round' (entryAmount (fst entry) * (1 - 1 / 1.24))
                      }
                  3001 ->
                    acc
                      { vat14 =
                          vat14 acc
                            + round' (entryAmount (fst entry) * (1 - 1 / 1.14))
                      }
                  3002 ->
                    acc
                      { vat10 =
                          vat10 acc
                            + round' (entryAmount (fst entry) * (1 - 1 / 1.10))
                      }
                  _ -> acc
            )
            (TaxResult 0 0 0)
            entriesWithAccounts
    $(logInfo) $ "taxResultDeduction:" ++  (pack $ show taxResultDeduction)
    $(logInfo) $ "taxResultDebt:" ++  (pack $ show taxResultDebt)

    let toId accountCode' = do
          let result =
                Import.filter
                  (\(Entity key value) -> accountCode value == accountCode')
                  allAccounts'
          case result of
            ((Entity key value) : xs) -> key

    let preparedEntry = entryWithDocId (toSqlKey 0)
    let entries'' =
          [ preparedEntry
              { entryAccountId = toId 1538,
                entryAmount = vat24 taxResultDeduction
              },
            preparedEntry
              { entryAccountId = toId 4000,
                entryAmount = - (vat24 taxResultDeduction)
              },
            preparedEntry
              { entryAccountId = toId 1539,
                entryAmount = vat14 taxResultDeduction
              },
            preparedEntry
              { entryAccountId = toId 4001,
                entryAmount = - (vat14 taxResultDeduction)
              },
            preparedEntry
              { entryAccountId = toId 1540,
                entryAmount = vat10 taxResultDeduction
              },
            preparedEntry
              { entryAccountId = toId 4002,
                entryAmount = - (vat10 taxResultDeduction)
              },
            -- Vat debt
            preparedEntry
              { entryAccountId = toId 2366,
                entryAmount = vat24 taxResultDebt
              },
            preparedEntry
              { entryAccountId = toId 3000,
                entryAmount = - (vat24 taxResultDebt)
              },
            preparedEntry
              { entryAccountId = toId 2367,
                entryAmount = vat14 taxResultDebt
              },
            preparedEntry
              { entryAccountId = toId 3001,
                entryAmount = - (vat14 taxResultDebt)
              },
            preparedEntry
              { entryAccountId = toId 2368,
                entryAmount = vat10 taxResultDebt
              },
            preparedEntry
              { entryAccountId = toId 3002,
                entryAmount = - (vat10 taxResultDebt)
              }
          ]
    let vatEntries =
          Import.filter
            (\item -> entryAmount item >= 0.01 || entryAmount item <= -0.01)
            entries''
    docId <-
      insert
        ( defTransaction
            { transactionDate = lastdate info,
              transactionValid = Just True,
              transactionMemo = Just "ALV-kirjaus",
              transactionCompanyId = companyId',
              transactionType = TypeVatStatement
            }
        )

    let vatAdjustmentEntriesWithProperTransactionId =
          Import.map (\entry -> entry {entryTransactionId = docId}) vatEntries
    $(logInfo) $ pack $ show vatAdjustmentEntriesWithProperTransactionId

    ids' <- insertEntries companyId' vatAdjustmentEntriesWithProperTransactionId $ lastdate info

    debts <-
      Import.mapM
        ( \accountCode -> do
            getAccountSum (toId accountCode) info
        )
        [2366, 2367, 2368, 2371, 2365, 2378, 2363]
    $(logInfo) $ pack $ show debts
    let validDebts =
          Import.filter
            ( \item ->
                accountBalance item >= 0.01 || accountBalance item <= (-0.01)
            )
            (rights debts)
    let sumDebts =
          Import.foldr
            (\item acc -> acc + (accountBalance item))
            0
            validDebts
    let debtsEntries =
          Import.map
            ( \item ->
                preparedEntry
                  { entryAmount = round' (- (accountBalance item)),
                    entryAccountId =
                      toId $
                        fromIntegral $ fromSqlKey $ account item
                  }
            )
            validDebts
    receivables <-
      Import.mapM
        ( \accountCode -> do
            getAccountSum (toId accountCode) info
            -- ALV saamistilit
        )
        [1530, 1532, 1534, 1536, 1538, 1539, 1540]
    let validReceivables =
          Import.filter
            (\item -> accountBalance item >= 0.01 || accountBalance item <= -0.01)
            (rights receivables)
    let sumReceivables =
          Import.foldr
            (\item acc -> acc + accountBalance item)
            0
            validReceivables
    let receivablesEntries =
          Import.map
            ( \item ->
                preparedEntry
                  { entryAmount = round' (- (accountBalance item)),
                    entryAccountId =
                      toId $ fromIntegral $ fromSqlKey $ account item
                  }
            )
            validReceivables
    let totalDebt = sumReceivables + sumDebts
    let vatDebt =
          preparedEntry
            { entryAccountId = toId 2364,
              entryAmount = totalDebt
            }

    print "***********************************************"        
    $(logInfo) $ "totalDebt:" ++ (pack $ show totalDebt)
    $(logInfo) $ "sumReceivables:" ++ (pack $ show sumReceivables)
    $(logInfo) $ "sumDebts:" ++ (pack $ show sumDebts)
    $(logInfo) $ "taxResultDeduction:" ++ (pack $ show taxResultDeduction)
    $(logInfo) $ "taxResultDebt:" ++ (pack $ show taxResultDebt)
    print "***********************************************"        

    let vatReportEntries = debtsEntries ++ receivablesEntries
    ----------------------------------------------------------------------
    timestamp <- liftIO date'
    let zeroFiling = (sumReceivables == 0) && (sumDebts == 0)
    let (year, month, _) = toGregorian $ firstdate info
    let vatDeclarationBasicInfo =
          [ "000" --> "VSRALVKV",
            "198" --> timestamp,
            "014" --> "2438590-2_EB",
            "048" --> "EasyBooks",
            "010" --> Data.Text.unpack vatId,
            "050" --> "K",
            "052" --> show month, --ilmoituskausi
            "053" --> show year, --verovuosi
            if zeroFiling
              then "056" --> "1" -- ei toimintaa 1=Kyllä
              else "",
            "042" --> "0504828940", --yhteyshenkilön puh no,
            if vatReportEventType==SendVatReport
              then ""
              else "332" --> "1"
          ]
    vatReportForFiling <-
      Import.foldM
        ( \acc mapping -> do
            result <-
              foldM
                ( \acc account -> do
                    result <-
                      getAccountSumAbs'
                        ((toId $ fromIntegral $ (account)))
                        info
                    case result of
                      Right x -> return $ acc + x
                      Left _ -> return $ acc
                )
                0
                (snd mapping)
            return $
              acc
                ++ [ show (fst mapping)
                       --> Data.Text.unpack
                         ( Data.Text.replace "." "," $
                             Data.Text.pack $ printf "%.2f" result {- ++ "\r\n" -}
                         )
                   ]
        )
        [[]]
        taxAccountFilingMapping
    let readyReport =
          concat $
            vatDeclarationBasicInfo
              ++ vatReportForFiling
              ++ [ "307"
                     --> Data.Text.unpack
                       ( Data.Text.replace "." "," $
                           Data.Text.pack $ printf "%.2f" sumReceivables
                       ),
                   "308"
                     --> Data.Text.unpack
                       ( Data.Text.replace "." "," $
                           Data.Text.pack $ printf "%.2f" (- totalDebt)
                       ),
                   "999:1"
                 ]
    logWarnN $  pack $ ("-------------------------RAPORTTI ------------------------------- \n\r")
    $(logInfo) (Data.Text.pack readyReport)
    $(logInfo) "-------------------------RAPORTTI -------------------------------"

    let newDoc =
          defTransaction
            { transactionDate = (lastdate info),
              transactionValid = Just True,
              transactionMemo = (Just "ALV-laskelma"),
              transactionCompanyId = companyId',
              transactionType = TypeVatStatement
            }
    docId <- insert (newDoc {transactionMemo = (Just "ALV-laskelma")})
    -- Create total vat debt amount entry
    let vatEntriesWithProperTransactionId =
          Import.map (\entry -> entry {entryTransactionId = docId}) [vatDebt]
    ids <- insertEntries companyId' vatEntriesWithProperTransactionId $ lastdate info
    $(logInfo) $ pack $ show vatEntriesWithProperTransactionId

    vatReport <-
      Import.mapM
        (\account -> getAccountSum (toId account) info)
        $
        --ALV myynnit 24%, 14%, 10%
        [2366, 2367, 2368]
          ++
          --RS Rak.palveluosto ALV-velka
          [2378]
          ++
          --Tuonti EU:n ulkop. ALV-velka
          [2371]
          ++
          --EU-tavaraostot yhteensä
          [4040]
          ++
          --YS EU-tavaraoston ALV-velka
          [2365]
          ++
          --EU-palveluostot yhteensä
          [4201]
          ++
          --EU palveluostojen ALV-velat
          [2363]
          ++
          -- ALV-saamiset yhteensä arvonlisäverokannoittain'
          [1530, 1532, 1534, 1536, 1538, 1539, 1540]
    -- Set transaction id for all entries

    let vatReportEntriesWithProperTransactionId =
          Import.map
            (\entry -> entry {entryTransactionId = docId})
            vatReportEntries

    ids' <- insertEntries companyId' vatReportEntriesWithProperTransactionId $ lastdate info

    result <-
      liftHandler $
        --return $ Right "just kidding"
        sendVatReportImmediate (Data.Text.pack readyReport) companyId'
    case result of
      Right _ -> do
        logEvent ("VAT report send to Tax authority and sales/purchases adjusted for VAT") (Just companyId')
        time <- liftIO $ currentTime       
        insert $ ProcessedVatReport (firstdate info) (lastdate info) time companyId'  
        return (toJSON (rights vatReport))
      Left _ ->
        sendResponseStatus
          status500
          ("Could not send VAT report to IR" :: Text)
