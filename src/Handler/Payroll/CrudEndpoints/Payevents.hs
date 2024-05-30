{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Handler.Payroll.CrudEndpoints.Payevents where
import Import hiding (id)
import Database.Persist.Sql (ConnectionPool, runSqlPool, fromSqlKey, toSqlKey)
import Data.Fixed
import Data.Time
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable hiding (length)
import qualified Data.HashMap.Strict as HM
import GHC.Exts
import qualified Data.Vector as V
import Handler.Banking.Utils (getSetting)
import qualified Data.Map

getPayeventsR :: CompanyId -> EmployeeId -> Handler Value
getPayeventsR companyId employeeId = do
    payevents<-runDB $ selectList [ PayeventEmployeeId==.employeeId] [] 
    returnJson payevents


data NewEntrySet = NewEntrySet {ctxDate::Day
                              , ctxDocId::TransactionId
                              , ctxEntries::[Entry]
                              , ctxAccounts::[Entity Account]
                              }

processNewEntriesAndRunSideEffects::CompanyId -> NewEntrySet->DB [Key Entry] 
processNewEntriesAndRunSideEffects companyId NewEntrySet {..} = do
  let process = (transformEntryAccountCodeToAccountId (ctxAccounts)) . (addDocIds (ctxDocId)) 
  let processedEntries = process ctxEntries
  insertEntries companyId processedEntries ctxDate
  
instance Postable Payevent where 
  post payevent = do
    let Payevent {..} = payevent
    let ctxDate = payeventDate
    
    ctxAccounts <- getAccountEntities payeventCompanyId

    p <- getSetting appPredefinedAccounts
    let AccountConstant {..} = p

    id<-runDB $ do

        id <- insert payevent
        employee <- getJust payeventEmployeeId
        employeeType'<-case (employeeType employee) of
          Just x -> return x
          Nothing -> sendResponseStatus status404("Employee type is not set!"::Text)

        -- Spread employee record fields all over
        let Employee {..} = employee

        let employeeTax = round' $ employeeTaxpercentage*payeventGross/100
        let employeeTyel = round' $ employeeTyelpercentage*payeventGross/100
        let employeeTvm = round' $ employeeTvmpercentage*payeventGross/100

        let adjustments = [Payadjustment { payadjustmentPayadjustmenttypeId=toSqlKey 3, payadjustmentAmount=Just employeeTax, payadjustmentPayeventId=id, payadjustmentType=Tax},
                           Payadjustment { payadjustmentPayadjustmenttypeId=toSqlKey 2,payadjustmentAmount=Just employeeTyel, payadjustmentPayeventId=id, payadjustmentType=EmployeePensionInsurance},
                           Payadjustment { payadjustmentPayadjustmenttypeId=toSqlKey 1,payadjustmentAmount=Just employeeTvm, payadjustmentPayeventId=id, payadjustmentType=EmployeeUnemploymentInsurance}]

        let dailyAllowancePayment = round'(employeeSavadailyallowancepercentage*payeventGross/100)

        let info = PayeventNote { payeventNoteDescription = pack("Sairausvakuutuksen päivärahamaksu: "++ show dailyAllowancePayment)
                                , payeventNotePayeventId = id }

        infoKey<-insert info

        idsAdjustments <-  insertMany adjustments
        let rawIds = show $ map (fromSqlKey) idsAdjustments
        company <- getJust payeventCompanyId

        let Company {..} = company
        let Payevent {..} = payevent

        let employerSava = round' $ payeventGross*companySavapercentage/100
        let employerTvm = round' $ payeventGross*companyTvmpercentage/100
        let employerTyel = round' $ payeventGross*companyTyelpercentage/100 - employeeTyel
        let toBePaid = round' (payeventGross - employeeTvm - employeeTyel - employeeTax)
        let toBePaidYel = round' (payeventGross - employeeTax)

        let transaction = defTransaction {transactionCompanyId=payeventCompanyId, transactionValid = Just True,transactionMemo=Just "Palkka",transactionDate=payeventDate}
        
        ctxDocId <- insert transaction

        let ctxEntries = case employeeType' of
              Tyel ->
                [ defEntry  { entryAccountId = grossPay,                    entryAmount = payeventGross,entryMemo=Just("Palkka"::Text) }
                , defEntry  { entryAccountId = taxPrepaymentDebt,           entryAmount = -employeeTax, entryMemo=Just("Ennakonpidätys"::Text) }
                , defEntry  { entryAccountId = pensionInsuranceExpense,     entryAmount = employerTyel, entryMemo=Just("Työnantajan osuus TyEL-maksu"::Text) }
                , defEntry  { entryAccountId = accruedExpenses,             entryAmount = -employerTyel,entryMemo=Just("Työnantajan osuus TyEL-maksu"::Text) }
                , defEntry  { entryAccountId = healthInsuranceExpense,      entryAmount = employerSava, entryMemo=Just("Työnantajan Sava-maksu"::Text) }
                , defEntry  { entryAccountId = healthInsuranceDebt,         entryAmount = -employerSava,entryMemo=Just("Työnantajan Sava-maksu"::Text) }
                , defEntry  { entryAccountId = accruedExpenses,             entryAmount = -employeeTyel,entryMemo=Just("Työntekijän TyEL-maksu"::Text) }
                , defEntry  { entryAccountId = employerUnemploymentExpense, entryAmount = employerTvm,  entryMemo=Just("Työnantajan Tvm-maksu"::Text) }
                , defEntry  { entryAccountId = accruedExpenses,             entryAmount = -employerTvm, entryMemo=Just("Työnantajan Tvm-maksu"::Text) }
                , defEntry  { entryAccountId = accruedExpenses,             entryAmount = -employeeTvm, entryMemo=Just("Työntekijän Tvm-maksu"::Text) }
                , defEntry  { entryAccountId = accruedExpenses,             entryAmount= - toBePaid,    entryMemo=Just("Maksetaan tilille"::Text) }]

              Yel ->
                [ defEntry  { entryAccountId = grossPay,                    entryAmount = payeventGross,entryMemo=Just ("Palkka"::Text) }
                , defEntry  { entryAccountId = taxPrepaymentDebt,           entryAmount = -employeeTax, entryMemo=Just ("Ennakonpidätys"::Text) }
                , defEntry  { entryAccountId = healthInsuranceExpense,      entryAmount = employerSava, entryMemo=Just("Työnantajan Sava-maksu"::Text) }
                , defEntry  { entryAccountId = healthInsuranceDebt,         entryAmount = -employerSava,entryMemo=Just("Työnantajan Sava-maksu"::Text) }
                , defEntry  { entryAccountId = accruedExpenses,             entryAmount= - toBePaidYel, entryMemo=Just("Maksetaan tilille"::Text) }]

        keys <- processNewEntriesAndRunSideEffects payeventCompanyId NewEntrySet{..}
        return id
    $(logInfo) "ALL OK"
    sendResponseStatus status201 (object ["id" .=( fromSqlKey id)])



postPayeventsR :: CompanyId->EmployeeId->Handler ()
postPayeventsR companyId employeeId = do
    payevent <- requireCheckJsonBody :: Handler Payevent
    post payevent

data PayeventList = PayeventList [Payevent]  deriving (Generic,Show)
instance FromJSON PayeventList
instance ToJSON PayeventList

postPayeventEntriesR :: PayeventId -> Handler ()
postPayeventEntriesR id = do
      entries <- requireCheckJsonBody::Handler [Payevent]
      ids <-  runDB $ insertMany (entries)
      let rawIds = show $ map (fromSqlKey) ids
      sendResponseStatus status201 (rawIds)
