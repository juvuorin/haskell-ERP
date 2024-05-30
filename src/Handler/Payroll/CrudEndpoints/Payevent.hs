{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Payroll.CrudEndpoints.Payevent where
import Import
import System.Directory
import Database.Persist.Sql (ConnectionPool, runSqlPool, fromSqlKey, toSqlKey)
import System.IO.Error hiding (catch)

putPayeventR :: CompanyId->EmployeeId->PayeventId -> Handler Value
putPayeventR c e id  = do
    payevent <- (requireCheckJsonBody :: Handler Payevent)
    runDB $ replace id payevent
    sendResponseStatus status200 ("UPDATED" :: Text)

getPayeventR :: CompanyId->EmployeeId->PayeventId -> Handler Value
getPayeventR companyId employeeId payeventId = do
    payevent <- runDB $ get404 payeventId   
    returnJson payevent

deletePayeventR :: CompanyId -> EmployeeId -> PayeventId -> Handler String
deletePayeventR companyId employeeId payeventId = do
    result<-runDB $ do
        delete payeventId
    sendResponseStatus status200 ("DELETED" :: Text)    

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

