{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Payroll.ExternalReporting.ReportToIR where
import Import hiding (unpack)
import Data.Time
import Data.UUID.V4
import External.IncomeRegister.Types
import Database.Persist.Sql (fromSqlKey)
import External.IncomeRegister.IncomeRegisterService
import External.IncomeRegister.Types

import qualified External.IncomeRegister.Types as BasicData
import qualified External.IncomeRegister.Types as PayerSummary

postPayerSummaryReportToIR :: CompanyId -> Handler ()
postPayerSummaryReportToIR companyId = do
    runDB $ logEvent ("Sending payer summary report to IR for company with id "++ (show $ fromSqlKey companyId)) (Just companyId)

    toBeReported <- getReportData companyId
   
    result <- sendReport companyId (toBeReported :: PayerSummaryData)
    case result of
        Right s->runDB $ logEvent ("Sending payer symmary report to IR for company with id  "++show (fromSqlKey companyId)++" completed successfully") (Just companyId)
        Left s-> runDB $ logEvent ("Sending payer summary report to IR for payevent "++ show (fromSqlKey companyId)++" failed") (Just companyId)

--    returnJson result    
    return () 

postReportToIR:: CompanyId->PayeventId -> Handler Value
postReportToIR companyId payeventId = do

  operationId <- liftIO $ nextRandom
  runDB $ logEvent (show operationId++": Sending report to IR for payevent "++(keyToString payeventId)) (Just companyId)
  
  BasicData.BasicDataEarner {..} <- BasicData.getBasicDataEarner payeventId
  result <- case employeetype of
    Just Tyel -> do
        result <- getReportData payeventId
        sendReport companyId (result::ContractEmployerData) 

    Just Yel -> do
        result <- getReportData payeventId
        sendReport companyId (result::YelEmployeeData)

    _ -> do
        runDB $ logEvent (show operationId++": Sending report to IR for payevent "++keyToString payeventId++" canceled, employee type is not set") (Just companyId)
        sendResponseStatus status404 ("Employee type is not set"::Text)

  case result of
      Right s->runDB $ logEvent (show operationId++": Sending report to IR for payevent "++keyToString payeventId++" completed successfully") (Just companyId)
      Left s->runDB $ logEvent (show operationId++": Sending report to IR for payevent "++keyToString payeventId++" failed") (Just companyId)

  returnJson result 

