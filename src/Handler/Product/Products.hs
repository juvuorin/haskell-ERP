{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Product.Products where
import Import
import Database.Persist.Sql (ConnectionPool, runSqlPool, fromSqlKey)

getProductsR :: CompanyId->Handler Value
getProductsR cid= do
    companies <- runDB $ selectList [] [Asc ProductId] :: Handler [Entity Product]
    returnJson companies

postProductsR :: CompanyId->Handler ()
postProductsR cid= do
    product <- requireJsonBody :: Handler Product
    id  <- runDB $ insert product
    sendResponseStatus status201 (object ["id" .=( fromSqlKey id)])