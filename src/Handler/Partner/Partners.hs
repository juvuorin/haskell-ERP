{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Partner.Partners where
import Import
import Database.Persist.Sql (ConnectionPool, runSqlPool, fromSqlKey)

getPartnersR :: CompanyId->Handler Value
getPartnersR cid= do
    companies <- runDB $ selectList [] [Asc PartnerId] :: Handler [Entity Partner]
    returnJson companies

postPartnersR :: CompanyId->Handler ()
postPartnersR cid= do
    customer <- requireCheckJsonBodyForceCoherence :: Handler Partner
    id  <- runDB $ insert customer
    sendResponseStatus status201 (object ["id" .=( fromSqlKey id)])

