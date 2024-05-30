{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Payroll.CrudEndpoints.Employee where
import Import
import System.Directory
import Database.Persist.Sql (ConnectionPool, runSqlPool, fromSqlKey, toSqlKey, unSqlBackendKey)
import System.IO.Error hiding (catch)

putEmployeeR :: CompanyId -> EmployeeId -> Handler Value
putEmployeeR c id  = do
    employee <- (requireCheckJsonBody :: Handler Employee)
    runDB $ replace id employee
    sendResponseStatus status200 ("UPDATED" :: Text)

getEmployeeR :: CompanyId -> EmployeeId -> Handler Value
getEmployeeR c id = do
    employee <- runDB $ get404 id   
    returnJson employee
    

getEmployeeEntriesR:: CompanyId -> EmployeeId -> Handler Value
getEmployeeEntriesR c id = do
    employee <- runDB $ get404 id   
    returnJson employee

deleteEmployeeR :: CompanyId -> EmployeeId -> Handler ()
deleteEmployeeR companyId employeeId = do
    runDB $ delete employeeId
    let key = unSqlBackendKey $ unEmployeeKey employeeId  
    removeDirectoryIfExists $ "employees/"<>(show key)
    sendResponseStatus status200 ("DELETED" :: Text)
