{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Bookkeeping.InternalReporting.IncomeStatementMonthly where

import Data.Conduit (($$))
import Data.Conduit.List as CL
import qualified Data.Fold as F
import Data.Foldable
import qualified Data.List
import qualified Data.List as List
import Data.Profunctor
import Database.Persist.Sql (rawQuery, toSqlKey)
import Handler.Bookkeeping.InternalReporting.Utils
--import Handler.Bookkeeping.InternalReporting.Utils (RowType(..))
import Handler.Bookkeeping.InternalReporting.Utils (RowType (HeaderWithAmount))
import Handler.Bookkeeping.InternalReporting.Utils as U
  ( AccountBalanceTriplet
      ( AccountBalanceTriplet,
        accountBalance,
        accountId,
        accountName
      ),
    ReportInfo (companyId, firstDate, lastDate),
    ReportInfoDay (ReportInfoDay, companyId', firstDay, lastDay),
    RowType (AccountInfo, Header, HeaderWithAmount, accountInfo, tabIndex),
    TemplateRowType (..),
    convertFromPersistent,
    parseDay',
  )
import Import

--import Handler.Bookkeeping.InternalReporting.Utils (HeaderWithAmount)

-- import Control.Foldl
--b = AccountHierarchy {description = "1. LIIKEVAIHTO", parentId = toSqlKey 1, amount=Nothing}

--a = AccountHierarchy {accountHierarchyDescription = Just "1. LIIKEVAIHTO", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing}
{-
2. Valmiiden ja keskeneräisten tuotteiden varastojen muutos
3. Valmistus omaan käyttöön
4. Liiketoiminnan muut tuotot
5. Materiaalit ja palvelut
a) Aineet, tarvikkeet ja tavarat
aa) Ostot tilikauden aikana
ab) Varastojen muutos
b) Ulkopuoliset palvelut
6. Henkilöstökulut
a) Palkat ja palkkiot
b) Henkilösivukulut
ba) Eläkekulut
bb) Muut henkilösivukulut
7. Poistot ja arvonalentumiset
a) Suunnitelman mukaiset poistot
b) Arvonalentumiset pysyvien vastaavien hyödykkeistä
c) Vaihtuvien vastaavien poikkeukselliset arvonalentumiset
8. Liiketoiminnan muut kulut
9. LIIKEVOITTO (-TAPPIO)
10. Rahoitustuotot ja -kulut
a) Tuotot osuuksista saman konsernin yrityksissä
b) Tuotot osuuksista omistusyhteysyrityksissä
c) Tuotot muista pysyvien vastaavien sijoituksista
d) Muut korko- ja rahoitustuotot
e) Arvonalentumiset pysyvien vastaavien sijoituksista
f) Arvonalentumiset vaihtuvien vastaavien rahoitusarvopapereista
g) Korkokulut ja muut rahoituskulut
11. VOITTO (TAPPIO) ENNEN TILINPÄÄTÖSSIIRTOJA JA VEROJA
12. Tilinpäätössiirrot
a) Poistoeron muutos
b) Vapaaehtoisten varausten muutos
c) Konserniavustus
13. Tuloverot
14. Muut välittömät verot
15. TILIKAUDEN VOITTO (TAPPIO)
 -}

{- incomeStatementHierarchy =
  [AccountHierarchy {accountHierarchyId = toSqlKey 4, accountHierarchyDescription = Just "Tuloslaskelma", accountHierarchyParentId =  Nothing, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "Tase", accountHierarchyParentId =  Nothing, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "1. LIIKEVAIHTO", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "2. Valmiiden ja keskeneräisten tuotteiden varastojen muutos", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "3. Valmistus omaan käyttöön", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "4. Liiketoiminnan muut tuotot", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "5. Materiaalit ja palvelut", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "a) Aineet, tarvikkeet ja tavarat", accountHierarchyParentId = Just $ toSqlKey 7, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "aa) Ostot tilikauden aikana", accountHierarchyParentId = Just $ toSqlKey 8, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "ab) Varastojen muutos", accountHierarchyParentId = Just $ toSqlKey 8, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "b) Ulkopuoliset palvelut", accountHierarchyParentId = Just $ toSqlKey 7, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "6. Henkilöstökulut", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "a) Palkat ja palkkiot", accountHierarchyParentId = Just $ toSqlKey 12, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "b) Henkilösivukulut", accountHierarchyParentId = Just $ toSqlKey 12, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "ba) Eläkekulut", accountHierarchyParentId = Just $ toSqlKey 14, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "bb) Muut henkilösivukulut", accountHierarchyParentId = Just $ toSqlKey 14, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "7. Poistot ja arvonalentumiset", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "a) Suunnitelman mukaiset poistot", accountHierarchyParentId = Just $ toSqlKey 17, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "b) Arvonalentumiset pysyvien vastaavien hyödykkeistä", accountHierarchyParentId = Just $ toSqlKey 17, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "c) Vaihtuvien vastaavien poikkeukselliset arvonalentumiset", accountHierarchyParentId = Just $ toSqlKey 17, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "8. Liiketoiminnan muut kulut", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "9. LIIKEVOITTO (-TAPPIO)", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "10. Rahoitustuotot ja -kulut", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "a) Tuotot osuuksista saman konsernin yrityksissä", accountHierarchyParentId = Just $ toSqlKey 23, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "b) Tuotot osuuksista omistusyhteysyrityksissä", accountHierarchyParentId = Just $ toSqlKey 23, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "c) Tuotot muista pysyvien vastaavien sijoituksista", accountHierarchyParentId = Just $ toSqlKey 23, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "d) Muut korko- ja rahoitustuotot", accountHierarchyParentId = Just $ toSqlKey 23, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "e) Arvonalentumiset pysyvien vastaavien sijoituksista", accountHierarchyParentId = Just $ toSqlKey 23, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "f) Arvonalentumiset vaihtuvien vastaavien rahoitusarvopapereista", accountHierarchyParentId = Just $ toSqlKey 23, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "g) Korkokulut ja muut rahoituskulut", accountHierarchyParentId = Just $ toSqlKey 23, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "11. VOITTO (TAPPIO) ENNEN TILINPÄÄTÖSSIIRTOJA JA VEROJA", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "12. Tilinpäätössiirrot", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "a) Poistoeron muutos", accountHierarchyParentId = Just $ toSqlKey 32, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "b) Vapaaehtoisten varausten muutos", accountHierarchyParentId = Just $ toSqlKey 32, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "c) Konserniavustus", accountHierarchyParentId = Just $ toSqlKey 32, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "13. Tuloverot", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "14. Muut välittömät verot", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
   AccountHierarchy {accountHierarchyDescription = Just "15. TILIKAUDEN VOITTO (TAPPIO)", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing}]

 -}

postIncomeStatementR :: CompanyId -> Handler Value
postIncomeStatementR companyId = do
  --sendResponseStatus status200 ("NOT IMPLEMENTED" :: Text)
  --accountingYears <- runDB $ selectList[AccountingYearCompanyId ==. companyId][]

  --    return []

  info <- requireCheckJsonBody :: Handler ReportInfo
  $(logInfo) (pack $ show info)
  let reportTemplate =
        [ H 0 "LIIKEVAIHTO",
          R 2 (3000, 3090),
          H 0 "Valmiiden ja keskeneräisten tuotteiden varastojen muutos",
          A 2 3100,
          H 0 "Valmistus omaan käyttöön",
          L 2 [3200, 3205],
          H 0 "Liiketoiminnan muut tuotot",
          L 2 [3300, 3301],
          {-             /*
                  5. Materiaalit ja palvelut
                  a) Aineet, tarvikkeet ja tavarat
                  aa) Ostot tilikauden aikana 4000-4030
                  ab) Varastojen muutos  4100
                  b) Ulkopuoliset palvelut 4200,4201, 4205
                  */
          -}
          H 0 "Materiaalit ja palvelut",
          H 2 "Aineet, tarvikkeet ja tavarat",
          H 2 "Ostot tilikauden aikana",
          R 4 (4000, 4018),
          H 2 "Varastojen muutos",
          A 4 4100,
          H 2 "Ulkopuoliset palvelut",
          L 4 [4200, 4201, 4202],
          --            //       H "Materiaalit ja palvelut yhteensä", 13400),

          {-             /*  6. Henkilöstökulut
                  a) Palkat ja palkkiot  6000
                  b) Henkilösivukulut 6030-6041
                  ba) Eläkekulut 6020
                  bb) Muut henkilösivukulut 6095
                          */
          -}
          H 0 "Henkilöstökulut",
          H 2 "Palkat ja palkkiot",
          A 4 6000,
          H 2 "Henkilösivukulut",
          H 4 "Eläkekulut",
          L 6 [6001, 6006, 6007, 6008],
          H 4 "Muut henkilösivukulut",
          L 6 [6003, 6004, 6010],
          {-             /*
                  7. Poistot ja arvonalentumiset
                  a) Suunnitelman mukaiset poistot  7000
                  b) Arvonalentumiset pysyvien vastaavien hyödykkeistä  7050
                  c) Vaihtuvien vastaavien poikkeukselliset arvonalentumiset 7060
                  8. Liiketoiminnan muut kulut 6100-6999
                  9. LIIKEVOITTO (-TAPPIO)
                  10. Rahoitustuotot ja -kulut
                  */
          -}
          H 0 "Poistot ja arvonalentumiset",
          H 2 "Suunnitelman mukaiset poistot",
          A 4 7000,
          H 2 "Arvonalentumiset pysyvien vastaavien hyödykkeistä",
          A 4 7050,
          H 2 "Vaihtuvien vastaavien poikkeukselliset arvonalentumiset",
          A 4 7060,
          L
            2
            [ 6464,
              6504, --Vakuutukset
              6988, --Muut liikekulut
              --                 7301, --Korkokulut ei vähennyskelpoiset
              6101, --Vuokrat
              --               7270, --Korkotuotot ei veronalaiset
              --   7000, --Suunnitelman mukaiset poistot
              6940 --Pankkikulut
            ],
          --  R 2 (6100, 9999),
          S 0 "LIIKEVOITTO (-TAPPIO)" [(3000, 7060)],
          H 0 "Rahoitustuotot ja -kulut",
          R 2 (7061, 7450),
          S 0 "VOITTO (TAPPIO) ENNEN TILINPÄÄTÖSSIIRTOJA JA VEROJA" [(3000, 7060), (7061, 7450)],
          H 0 "Tilinpäätössiirrot",
          --a) Poistoeron muutos
          --b) Vapaaehtoisten varausten muutos
          --c) Konserniavustus
          H 0 "Tuloverot",
          H 0 "Muut välittömät verot"
        ]

  toJSON <$> (fillIncomeStatement reportTemplate info)

adder :: Num a => F.L a a
adder = F.L id (+) 0


runFold :: F.L a b -> [a] -> b
runFold (F.L result iterate initial) = result . foldl iterate initial


getBalanceBetweenAccounts :: Int -> Int -> AccountBalanceTriplet -> Double
getBalanceBetweenAccounts firstAccount lastAccount item= do 
         if (accountId item) >= firstAccount && (accountId item) <= lastAccount
                        then round' (U.accountBalance item)
                        else 0

extractor :: [(Int, Int)] -> Int -> String -> F.L AccountBalanceTriplet RowType
extractor pairList tabIndex header = dimap (\triplet -> 
  List.sum $ List.map (\(f, l) -> getBalanceBetweenAccounts f l triplet) pairList) 
  (\sum -> HeaderWithAmount tabIndex header sum) adder

runExtractor :: [(Int, Int)] -> Int -> String -> [AccountBalanceTriplet] -> RowType
runExtractor pairList tabIndex header entries = runFold (extractor pairList tabIndex header) entries



fillIncomeStatement :: [TemplateRowType] -> ReportInfo -> Handler [RowType]
fillIncomeStatement reportTemplate info = do
  let daysOk = ((,)) <$> parseDay' (firstDate info) <*> parseDay' (lastDate info)
  $(logInfo) (pack $ show daysOk)
  case daysOk of
    Just (f, l) -> do
{-       let sql =
            "SELECT account.code,account.name, SUM(entry.amount) "
              ++ "FROM account INNER JOIN entry ON account.id=entry.account_id WHERE "
              ++ " (entry.transaction_id IN"
              ++ " (select id from transaction where date BETWEEN ? AND ? AND company_id=?)) AND "
              ++ " (entry.account_id IN (select id from account where code>2999 AND code<10000)) group by account.code,account.name"
 -}
      let sql =    "SELECT account.code,account.name, SUM(monthly_balance.balance)" 
              ++    "FROM account INNER JOIN monthly_balance ON account.id=monthly_balance.account_id WHERE" 
              ++    "((monthly_balance.date BETWEEN ? AND ?)"
              ++    "AND company_id=?) AND" 
			        ++    "(monthly_balance.account_id IN (select id from account where code>2999 AND code<10000 AND company_id=1)) group by account.code,account.name"


{-             "SELECT account.code,account.name, SUM(monthly_balance.balance) "
              ++ "FROM account INNER JOIN monthly_balance ON account.id=monthly_balance.account_id WHERE "
              ++ " (monthly_balanceid IN"
              ++ " (select id from transaction where date BETWEEN ? AND ? AND company_id=?)) AND "
              ++ " (monthly_balance.account_id IN (select id from account where code>2999 AND code<10000)) group by account.code,account.name"
 -}

{- MonthlyBalance json
    balance             Double
    accountId           AccountId
    date                Day
    Primary             accountId date
    AccoundBalanceIdentity      accountId date
    deriving Show Eq

 -}




      let info' = ReportInfoDay f l (companyId info)

      let parameters =
            [ toPersistValue (firstDay info'),
              toPersistValue (lastDay info'),
              toPersistValue (companyId' info')
            ]

      entries <- runDB $ rawQuery sql parameters $$ CL.map convertFromPersistent =$ CL.consume
      let concatenated = List.concat entries 
      let entries' =
            Import.foldr
              ( \x acc -> acc ++ [x {U.accountBalance = round' $ U.accountBalance x}])
              []
              concatenated

      let buildRows tabIndex (firstAccount, lastAccount) = accountInfoRows
            where
              accountInfoRows =
                Import.foldr
                  ( \item acc ->
                      if (accountId item) >= firstAccount && (accountId item) <= lastAccount
                        then acc ++ [AccountInfo tabIndex item]
                        else acc
                  )
                  []
                  entries'

      let buildHeaderRowWithAmountFromListOfAccountPairs tabIndex header pairList = do
            runExtractor pairList tabIndex header entries'

      let buildRowsFromListOfAccountPairs tabIndex list =
            List.concatMap (buildRows tabIndex) list 

      let buildRow tabIndex account = filtered
            where
              filtered =
                Import.foldr
                  ( \item acc ->
                      if account == accountId item
                        then acc ++ [AccountInfo tabIndex item]
                        else acc
                  )
                  []
                  entries'

      let buildRowsFromList tabIndex list = rowItems
            where
              rowItems = Import.concatMap (buildRow tabIndex) list

      let report =
            foldl
              ( \acc item ->
                  case item of
                    H tabIndex' header' -> acc ++ [Header tabIndex' header']
                    A tabIndex' account -> acc ++ buildRow tabIndex' account
                    R tabIndex' range -> acc ++ buildRows tabIndex' range
                    L tabIndex' list -> acc ++ buildRowsFromList tabIndex' list
                    P tabIndex' list -> acc ++ buildRowsFromListOfAccountPairs tabIndex' list
                    S tabIndex' header' list -> acc ++ [buildHeaderRowWithAmountFromListOfAccountPairs tabIndex' header' list]
              )
              []
              reportTemplate

      putStrLn (pack $ show entries')

      return report
    Nothing -> return []
