{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Product.Product where
import Import
import Data.Text (Text)

getProductR :: CompanyId->ProductId -> Handler Value
getProductR companyId id = do
    product <- runDB $ get404 id
    returnJson product

putProductR :: CompanyId->ProductId -> (HandlerFor App) Value
putProductR companyId id  = do
    product <- (requireJsonBody :: Handler Product)
    putStrLn(pack $ show product)
    runDB $ replace id product
    sendResponseStatus status200 ("UPDATED" :: Text)
    
deleteProductR :: CompanyId->ProductId -> Handler Value
deleteProductR companyId id = do
    putStrLn ("Deleting Product")
    runDB $ delete id
    sendResponseStatus status200 ("DELETED" :: Text) 