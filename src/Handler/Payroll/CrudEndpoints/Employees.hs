{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}


module Handler.Payroll.CrudEndpoints.Employees where
import Import
import Database.Persist.Sql (ConnectionPool, runSqlPool, fromSqlKey, toSqlKey, rawSql ,rawQuery)
import Data.Fixed
import Data.Time
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import GHC.Exts 
import qualified Data.Vector as V
import Database.Persist.Sql (rawQuery, insert)
import Data.Conduit (($$))
import Data.Conduit.List as CL
import Control.Monad.IO.Class (liftIO)
 
data EmployeeSubset = EmployeeSubset {firstname::Text, lastname::Text, id::Int64} deriving (Show, Generic)
data EmployeeSubsetList = EmployeeSubsetList [EmployeeSubset]  deriving (Generic,Show)
instance ToJSON EmployeeSubset
instance ToJSON EmployeeSubsetList 

getEmployeesR :: CompanyId -> Handler Value
getEmployeesR id = do
    transactions<-runDB $ selectList[ EmployeeCompanyId==.id] [] :: Handler [Entity Employee]   
    returnJson transactions

postEmployeesR :: CompanyId->Handler ()
postEmployeesR c = do
    employee <- requireCheckJsonBody :: Handler Employee
    id  <- runDB $ insert employee
    sendResponseStatus status201 (object ["id" .=( fromSqlKey id)])

data EmployeeList = EmployeeList [Employee]  deriving (Generic,Show)
instance FromJSON EmployeeList 
instance ToJSON EmployeeList

postEmployeeEntriesR :: CompanyId->EmployeeId -> Handler ()
postEmployeeEntriesR c id = do
      entries <- requireCheckJsonBody::Handler [Employee]
      ids <-  runDB $ insertMany (entries)
      let rawIds = show $ Import.map (fromSqlKey) ids  
      sendResponseStatus status201 (rawIds) 
      
deleteEmployeeEntriesR3::CompanyId->EmployeeId -> Handler ()
deleteEmployeeEntriesR3 c id= do 
    runDB $ delete id    
    sendResponseStatus status200 ("DELETED" :: Text)
