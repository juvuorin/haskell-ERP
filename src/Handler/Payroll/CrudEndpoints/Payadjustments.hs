{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Payroll.CrudEndpoints.Payadjustments where
import Import
import Database.Persist.Sql (fromSqlKey)
import Data.Aeson ()

getPayadjustmentsR :: CompanyId -> EmployeeId ->PayeventId->Handler Value
getPayadjustmentsR companyId employeeId payeventId= do
    payadjustments<-runDB $ selectList [ PayadjustmentPayeventId==.payeventId] [] :: Handler [Entity Payadjustment]   
    returnJson payadjustments

postPayadjustmentsR :: CompanyId->EmployeeId ->PayeventId->Handler ()
postPayadjustmentsR c e p= do
    payadjustments <- requireCheckJsonBody :: Handler [Payadjustment]
    ids <-  runDB $ insertMany (payadjustments)
    let rawIds = show $ map (fromSqlKey) ids  
    sendResponseStatus status201 (rawIds) 

postPayadjustmentEntriesR :: PayadjustmentId -> Handler ()
postPayadjustmentEntriesR id = do
      entries <- requireCheckJsonBody::Handler [Payadjustment]
      ids <-  runDB $ insertMany (entries)
      let rawIds = show $ map (fromSqlKey) ids  
      sendResponseStatus status201 (rawIds) 
