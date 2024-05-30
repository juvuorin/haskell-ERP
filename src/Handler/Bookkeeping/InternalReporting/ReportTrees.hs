{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude   ***REMOVED***-}
{-# LANGUAGE OverloadedStrings   ***REMOVED***-}
{-# LANGUAGE TemplateHaskell     ***REMOVED***-}
{-# LANGUAGE TypeFamilies        ***REMOVED***-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Bookkeeping.InternalReporting.ReportTrees where
import Database.Persist.Sql (rawQuery, toSqlKey)
import Data.Foldable
import Data.Conduit (($$))
import Data.Conduit.List as CL hiding (map, filter)
import Import
import Handler.Bookkeeping.InternalReporting.Utils as U
    ( convertFromPersistent,
      parseDay',
      RowType(Header, AccountInfo, accountInfo, tabIndex),
      ReportInfo(companyId, lastDate, firstDate),
      TemplateRowType(..),
      AccountBalanceTriplet(AccountBalanceTriplet, accountName,
                            accountId, accountBalance),
      ReportInfoDay(ReportInfoDay, companyId', lastDay, firstDay) )


hierarchy = 
    -- | Income statement hierarchy

    [Entity (toSqlKey 1)  AccountHierarchy {accountHierarchyDescription = Just "Tuloslaskelma", accountHierarchyParentId =  Nothing, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 2)  AccountHierarchy {accountHierarchyDescription = Just "1. LIIKEVAIHTO", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 3)  AccountHierarchy {accountHierarchyDescription = Just "2. Valmiiden ja keskeneräisten tuotteiden varastojen muutos", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 4)  AccountHierarchy {accountHierarchyDescription = Just "3. Valmistus omaan käyttöön", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 5)  AccountHierarchy {accountHierarchyDescription = Just "4. Liiketoiminnan muut tuotot", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 6)  AccountHierarchy {accountHierarchyDescription = Just "5. Materiaalit ja palvelut", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 7)  AccountHierarchy {accountHierarchyDescription = Just "a) Aineet, tarvikkeet ja tavarat", accountHierarchyParentId = Just $ toSqlKey 6, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 8)  AccountHierarchy {accountHierarchyDescription = Just "aa) Ostot tilikauden aikana", accountHierarchyParentId = Just $ toSqlKey 7, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 9)  AccountHierarchy {accountHierarchyDescription = Just "ab) Varastojen muutos", accountHierarchyParentId = Just $ toSqlKey 7, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 10)  AccountHierarchy {accountHierarchyDescription = Just "b) Ulkopuoliset palvelut", accountHierarchyParentId = Just $ toSqlKey 6, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 11)  AccountHierarchy {accountHierarchyDescription = Just "6. Henkilöstökulut", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 12)  AccountHierarchy {accountHierarchyDescription = Just "a) Palkat ja palkkiot", accountHierarchyParentId = Just $ toSqlKey 11, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 13)  AccountHierarchy {accountHierarchyDescription = Just "b) Henkilösivukulut", accountHierarchyParentId = Just $ toSqlKey 11, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 14)  AccountHierarchy {accountHierarchyDescription = Just "ba) Eläkekulut", accountHierarchyParentId = Just $ toSqlKey 13, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 15)  AccountHierarchy {accountHierarchyDescription = Just "bb) Muut henkilösivukulut", accountHierarchyParentId = Just $ toSqlKey 13, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 16)  AccountHierarchy {accountHierarchyDescription = Just "7. Poistot ja arvonalentumiset", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 17)  AccountHierarchy {accountHierarchyDescription = Just "a) Suunnitelman mukaiset poistot", accountHierarchyParentId = Just $ toSqlKey 16, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 18)  AccountHierarchy {accountHierarchyDescription = Just "b) Arvonalentumiset pysyvien vastaavien hyödykkeistä", accountHierarchyParentId = Just $ toSqlKey 16, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 19)  AccountHierarchy {accountHierarchyDescription = Just "c) Vaihtuvien vastaavien poikkeukselliset arvonalentumiset", accountHierarchyParentId = Just $ toSqlKey 16, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 20)  AccountHierarchy {accountHierarchyDescription = Just "8. Liiketoiminnan muut kulut", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 21)  AccountHierarchy {accountHierarchyDescription = Just "9. LIIKEVOITTO (-TAPPIO)", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 22)  AccountHierarchy {accountHierarchyDescription = Just "10. Rahoitustuotot ja -kulut", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 23)  AccountHierarchy {accountHierarchyDescription = Just "a) Tuotot osuuksista saman konsernin yrityksissä", accountHierarchyParentId = Just $ toSqlKey 22, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 24)  AccountHierarchy {accountHierarchyDescription = Just "b) Tuotot osuuksista omistusyhteysyrityksissä", accountHierarchyParentId = Just $ toSqlKey 22, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 25)  AccountHierarchy {accountHierarchyDescription = Just "c) Tuotot muista pysyvien vastaavien sijoituksista", accountHierarchyParentId = Just $ toSqlKey 22, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 26)  AccountHierarchy {accountHierarchyDescription = Just "d) Muut korko- ja rahoitustuotot", accountHierarchyParentId = Just $ toSqlKey 22, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 27)  AccountHierarchy {accountHierarchyDescription = Just "e) Arvonalentumiset pysyvien vastaavien sijoituksista", accountHierarchyParentId = Just $ toSqlKey 22, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 28)  AccountHierarchy {accountHierarchyDescription = Just "f) Arvonalentumiset vaihtuvien vastaavien rahoitusarvopapereista", accountHierarchyParentId = Just $ toSqlKey 22, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 29)  AccountHierarchy {accountHierarchyDescription = Just "g) Korkokulut ja muut rahoituskulut", accountHierarchyParentId = Just $ toSqlKey 22, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 30)  AccountHierarchy {accountHierarchyDescription = Just "11. VOITTO (TAPPIO) ENNEN TILINPÄÄTÖSSIIRTOJA JA VEROJA", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 31)  AccountHierarchy {accountHierarchyDescription = Just "12. Tilinpäätössiirrot", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 32)  AccountHierarchy {accountHierarchyDescription = Just "a) Poistoeron muutos", accountHierarchyParentId = Just $ toSqlKey 31, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 33)  AccountHierarchy {accountHierarchyDescription = Just "b) Vapaaehtoisten varausten muutos", accountHierarchyParentId = Just $ toSqlKey 31, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 34)  AccountHierarchy {accountHierarchyDescription = Just "c) Konserniavustus", accountHierarchyParentId = Just $ toSqlKey 31, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 35)  AccountHierarchy {accountHierarchyDescription = Just "13. Tuloverot", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 36)  AccountHierarchy {accountHierarchyDescription = Just "14. Muut välittömät verot", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 37)  AccountHierarchy {accountHierarchyDescription = Just "15. TILIKAUDEN VOITTO (TAPPIO)", accountHierarchyParentId = Just $ toSqlKey 1, accountHierarchyAmount=Nothing},
    

    -- | Balance sheet hierarchy
    Entity (toSqlKey 38)  AccountHierarchy {accountHierarchyDescription = Just "Tase", accountHierarchyParentId =  Nothing, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 39)  AccountHierarchy {accountHierarchyDescription = Just "  Vastaavaa", accountHierarchyParentId = Just $ toSqlKey 38, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 40)  AccountHierarchy {accountHierarchyDescription = Just "  Vastattavaa", accountHierarchyParentId = Just $ toSqlKey 38, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 41)  AccountHierarchy {accountHierarchyDescription = Just "    Pysyvät vastaavat", accountHierarchyParentId = Just $ toSqlKey 39, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 42)  AccountHierarchy {accountHierarchyDescription = Just "      Aineettomat hyödykkeet", accountHierarchyParentId = Just $ toSqlKey 41, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 43)  AccountHierarchy {accountHierarchyDescription = Just "      Aineelliset hyödykkeet", accountHierarchyParentId = Just $ toSqlKey 41, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 44)  AccountHierarchy {accountHierarchyDescription = Just "      Sijoitukset", accountHierarchyParentId = Just $ toSqlKey 41, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 45)  AccountHierarchy {accountHierarchyDescription = Just "    Vaihtuvat vastaavat", accountHierarchyParentId = Just $ toSqlKey 39, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 46)  AccountHierarchy {accountHierarchyDescription = Just "      Vaihto-omaisuus", accountHierarchyParentId = Just $ toSqlKey 45, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 47)  AccountHierarchy {accountHierarchyDescription = Just "      Saamiset; lyhyt- ja pitkäaikaiset erikseen", accountHierarchyParentId = Just $ toSqlKey 45, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 48)  AccountHierarchy {accountHierarchyDescription = Just "      Lyhytaikaiset saamiset", accountHierarchyParentId = Just $ toSqlKey 47, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 49)  AccountHierarchy {accountHierarchyDescription = Just "      Pitkäaikaiset saamiset", accountHierarchyParentId = Just $ toSqlKey 47, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 50)  AccountHierarchy {accountHierarchyDescription = Just "      Rahoitusarvopaperit", accountHierarchyParentId = Just $ toSqlKey 45, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 51)  AccountHierarchy {accountHierarchyDescription = Just "      Rahat ja pankkisaamiset", accountHierarchyParentId = Just $ toSqlKey 45, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 52)  AccountHierarchy {accountHierarchyDescription = Just "  Vastattavaa", accountHierarchyParentId = Just $ toSqlKey 38, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 53)  AccountHierarchy {accountHierarchyDescription = Just "    Oma pääoma", accountHierarchyParentId = Just $ toSqlKey 52, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 54)  AccountHierarchy {accountHierarchyDescription = Just "      Osake-, osuus- tai muu vastaava pääoma", accountHierarchyParentId = Just $ toSqlKey 53, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 55)  AccountHierarchy {accountHierarchyDescription = Just "      Ylikurssirahasto", accountHierarchyParentId = Just $ toSqlKey 53, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 56)  AccountHierarchy {accountHierarchyDescription = Just "      Arvonkorotusrahasto", accountHierarchyParentId = Just $ toSqlKey 53, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 57)  AccountHierarchy {accountHierarchyDescription = Just "      Käyvän arvon rahasto", accountHierarchyParentId = Just $ toSqlKey 53, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 58)  AccountHierarchy {accountHierarchyDescription = Just "      Muut rahastot", accountHierarchyParentId = Just $ toSqlKey 53, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 59)  AccountHierarchy {accountHierarchyDescription = Just "      Edellisten tilikausien voitto (tappio)", accountHierarchyParentId = Just $ toSqlKey 53, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 60)  AccountHierarchy {accountHierarchyDescription = Just "      Tilikauden voitto (tappio)", accountHierarchyParentId = Just $ toSqlKey 53, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 61)  AccountHierarchy {accountHierarchyDescription = Just "      Tilinpäätössiirtojen kertymä", accountHierarchyParentId = Just $ toSqlKey 53, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 62)  AccountHierarchy {accountHierarchyDescription = Just "      Pakolliset varaukset", accountHierarchyParentId = Just $ toSqlKey 40, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 63)  AccountHierarchy {accountHierarchyDescription = Just "    Vieras pääoma", accountHierarchyParentId = Just $ toSqlKey 40, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 64)  AccountHierarchy {accountHierarchyDescription = Just "      Lytyaikainen vieras pääoma", accountHierarchyParentId = Just $ toSqlKey 63, accountHierarchyAmount=Nothing},
    Entity (toSqlKey 65)  AccountHierarchy {accountHierarchyDescription = Just "      Pitkäaikainen vieras pääoma", accountHierarchyParentId = Just $ toSqlKey 63, accountHierarchyAmount=Nothing}]




{- data Asset = NonCurrentAssets | CurrentAssets
data NonCurrentAssets = IntangibleAssets | TangibleAssets
data IntagibleAssets =  EstablishmentExpenditure 
                      | DevelopmentExpenditure 
                      | ResearchExxpenditure 
                      | IntangibleRights 
                      | Goodwill 
                      | OtherCapitaliseLongtermExpenditure 
                      | AdvancePaymentsIntangibleAssets
data TangibleAssets = LandAnWaters | Buildings | MachineryAndEquipment | OtherTangibleAssets | AdvancePaymentsAndConstructionInProgress
data Investments = HoldingsInGroupUndertakingsOfInvestments 
            | AmountsOwedByGroupUndertakings 
            | ParticipatingInterests 
            | AmountsOwedByParticipatingInterestUndertakings
            | OtherSharesAndSimilaRightsOfOwnnership 
            | OtherDebtors
            | LongTermInvestmentProperties | LongTermInvestmentShares

data CurrentAssets = Stocks | Debtors 
data Stocks = RawMaterialsAndConsumables | SemifinishedProducts | FinishedProductsOrGoods | OtherStocks | AdvancePaymentsStocks
data Debtors=   TradeDebtors 
              | AmountsOwedByGroupMemberCompanies 
              | AmountsOwedByParticipatingInterest 
              | LoansReceivable 
              | OtherDebtorsDebtors
              | SubscribedCapitalUnpaid
              | PrepaymentsAndAccruedIncome

data InvestmentsOfCurrentAssets =  HoldingsInGroupUndertakingsOfCurrentAssets | OtherSharesAndSimilarRightsOfOwnership | OtherInvestments

data CapitalReservesAndLiabilities = CapitalAndReserves | Appproriations |Provisions |Creditors 
data CapitalAndReserves = SubscribeCapital | SharePremiumAccount | RevaluationReserve | OtherReserves | RetainedEarningsOrLoss | ProfitOrLossForTheFinancialYear 
data OtherReserves = ReserveForInvestedUnrestrictedEquity | LegalReserve | ReservesProvidedForByTheArticlesOfAssociationOrComparableRules 
    | FairValueReserve | OtherReservesOfOtherReserves

data Appropriations = CumulativeAcceleratedDepreciation | TaxationBasedReserves 
data Provisions = ProvisionsForPensions | ProvisionsForTaxation  | OtherProvisions 

data Creditors =  Bonds | ConvertibleBonds | LiabilitiesToCreditInstitutions | LoansFromPensionInstitutions
  |AdvancesReceived 
  |TradeCreditors 
  |BillsOfExchangePayable 
  |AmountsOwedToGroupUndertakings 
  |AmountsOwedToParticipatingInterestUndertakings 
  |OtherCreditors 
  |AccrualsAndDeferredIncome

 -}
{- 
Asset
A PYSYVÄT VASTAAVAT A NonCurrentAssets
I Aineettomat hyödykkeet I IntangibleAssets
1. Kehittämismenot 1. DevelopmentExpenditure
2. Aineettomat oikeudet 2. IntangiblRights
3. Liikearvo 3. Goodwill
4. Muut pitkävaikutteiset menot 4. Other capitalised longterm expenditure
5. Ennakkomaksut 5. Advance payments
II Aineelliset hyödykkeet II Tangible assets
1. Maa- ja vesialueet 1. Land and waters
2. Rakennukset ja rakennelmat 2. Buildings
3. Koneet ja kalusto 3. Machinery and equipment
4. Muut aineelliset hyödykkeet 4. Other tangible assets
5. Ennakkomaksut ja keskeneräiset hankinnat 5. Advance payments and construction in progress
III Sijoitukset III Investments
1. Osuudet saman konsernin yrityksissä 1. Holdings in group member companies
2. Saamiset saman konsernin yrityksiltä 2. Receivables from group member companies
3. Osuudet omistusyhteysyrityksissä 3. Participating interests
4. Saamiset omistusyhteysyrityksiltä 4. Receivables from participating interest
companies
5. Muut osakkeet ja osuudet 5. Other shares amd similar rights of ownership
6. Muut saamiset 6. Other receivables
B VAIHTUVAT VASTAAVAT B CURRENT ASSETS
I Vaihto-omaisuus I Stocks
1. Aineet ja tarvikkeet 1. Raw materials and consumables
2. Keskeneräiset tuotteet 2. Semifinished products
3. Valmiit tuotteet/tavarat 3. Finished products/goods
4. Muu vaihto-omaisuus 4. Other stocks
5. Ennakkomaksut 5. Advance payments
II Saamiset II Debtors
1. Myyntisaamiset 1. Trade debtors
TÄMÄ ON VANHA VERSIO / THIS IS THE OLD VERSION.
Katso uusi: / See the new version here: https://relipe.fi/wp-content/uploads/tase-fi-sv-en.pdf
Tase suomeksi ja englanniksi 2 (3)
Balance sheet in Finnish and English
2. Saamiset saman konsernin yrityksiltä 2. Amounts owed by group member companies
3. Saamiset omistusyhteysyrityksiltä 3. Amounts owed by participating interest
companies
4. Lainasaamiset 4. Loans receivable
5. Muut saamiset 5. Other debtors
6. Maksamattomat osakkeet/osuudet 6. Subscribed capital unpaid
7. Siirtosaamiset 7. Prepayments and accrued income
III Rahoitusarvopaperit III Investments
1. Osuudet saman konsernin yrityksissä 1. Holdings in group member companies
2. Muut osakkeet ja osuudet 2. Other shares and similar rights of ownership
3. Muut arvopaperit 3. Other investments
IV Rahat ja pankkisaamiset IV Cash in hand and at banks
V a s t a t t a v a a E q u i t y a n d l i a b i l i t i e s
A OMA PÄÄOMA A CAPITAL AND RESERVES
I Osake-, osuus- tai muu vastaava pääoma I Subscribed capital
II Ylikurssirahasto II Share premium account
III Arvonkorotusrahasto III Revaluation reserve
IV Käyvän arvon rahasto IV Fair value reserve
V Muut rahastot V Other reserves
1. Vararahasto 1. Legal reserve
2. Yhtiöjärjestyksen tai sääntöjen mukaiset
rahastot
2. Reserves provided for by the articles of
association or comparable rules
3. Muut rahastot 3. Other reserves
VI Edellisten tilikausien voitto (tappio) VI Retained earnings (loss)
VII Tilikauden voitto (tappio) VII Profit (loss) of the financial year
B TILINPÄÄTÖSSIIRTOJEN KERTYMÄ B APPROPRIATIONS
1. Poistoero 1. Cumulative accelerated depreciation
2. Vapaaehtoiset varaukset 2. Untaxed reserves
C PAKOLLISET VARAUKSET C PROVISIONS
1. Eläkevaraukset 1. Provisions for pensions
2. Verovaraukset 2. Provisions for taxation
3. Muut pakolliset varaukset 3. Other provisions
D VIERAS PÄÄOMA D CREDITORS
1. Joukkovelkakirjalainat 1. Bonds
2. Vaihtovelkakirjalainat 2. Convertible bonds
3. Lainat rahoituslaitoksilta 3. Loans from credit institutions
4. Eläkelainat 4. Pension loans
TÄMÄ ON VANHA VERSIO / THIS IS THE OLD VERSION.
Katso uusi: / See the new version here: https://relipe.fi/wp-content/uploads/tase-fi-sv-en.pdf
Tase suomeksi ja englanniksi 3 (3)
Balance sheet in Finnish and English
5. Saadut ennakot 5. Advances received
6. Ostovelat 6. Trade creditors
7. Rahoitusvekselit 7. Bills of exchange payable
8. Velat saman konsernin yrityksille 8. Amounts owed to group member companies
9. Velat omistusyhteysyrityksille 9. Amounts owed to participating interest
companies
10. Muut velat 10. Other creditors
11. Siirtovelat 11. Accruals and deferred income
 -}