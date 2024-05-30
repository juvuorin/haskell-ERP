{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Payroll.CrudEndpoints.Payadjustmenttypes where
import Import
import Database.Persist.Sql (ConnectionPool, runSqlPool, fromSqlKey, toSqlKey)
import Data.Fixed
import Data.Time
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import GHC.Exts 
import qualified Data.Vector as V

getPayadjustmenttypesR :: CompanyId -> Handler Value
getPayadjustmenttypesR id = do
    payadjustmenttypes<-runDB $ selectList ([] :: [Filter Payadjustmenttype]) []   
    returnJson payadjustmenttypes

