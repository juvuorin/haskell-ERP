{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}


module Handler.Company.Company where
import Import




getCompanyR :: CompanyId -> Handler Value
getCompanyR id = do
    company <- runDB $ get404 id
    returnJson company

getSalesInvoiceVatsR :: CompanyId -> (HandlerFor App) Value
getSalesInvoiceVatsR id  = do
    vats<-runDB $ selectList [][] :: Handler [Entity VatSale]
    returnJson vats

getPurchaseInvoiceVatsR :: CompanyId -> (HandlerFor App) Value
getPurchaseInvoiceVatsR id  = do
    vats<-runDB $ selectList [][] :: Handler [Entity VatPurchase]
    returnJson vats
    
getUnitsR :: CompanyId -> (HandlerFor App) Value
getUnitsR id  = do
    units <- runDB $ selectList [] [Asc UnitId] :: Handler [Entity Unit]
    returnJson units

getVatPctsR :: CompanyId -> (HandlerFor App) Value
getVatPctsR id  = do
  vatpcts<-runDB $ selectList [] [Asc VatPctId] :: Handler [Entity VatPct]
  returnJson vatpcts
  
getInvoiceTypesR :: CompanyId -> (HandlerFor App) Value
getInvoiceTypesR id = do

  sendResponseStatus status404 ("Not implemented"::Text)
putCompanyR :: CompanyId -> (HandlerFor App) Value
putCompanyR id  = do    
    company <- (requireJsonBody :: Handler Company)
    putStrLn(pack $ show company)
    runDB $ replace id company
    sendResponseStatus status200 ("UPDATED" :: Text)
    
deleteCompanyR :: CompanyId -> Handler Value
deleteCompanyR id = do
    putStrLn ("Deleting company")
    runDB $ delete id
    sendResponseStatus status200 ("DELETED" :: Text) 


