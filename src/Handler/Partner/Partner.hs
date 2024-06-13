{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Partner.Partner where
import Import
import Data.Text (Text)

getPartnerR :: CompanyId -> PartnerId -> Handler Value
getPartnerR companyId id = do
    partner <- runDB $ get404 id
    returnJson partner

putPartnerR :: CompanyId->PartnerId -> (HandlerFor App) Value
putPartnerR companyId id  = do
    partner <- (requireCheckJsonBodyForceCoherence:: Handler Partner)
    runDB $ replace id partner
    sendResponseStatus status200 ("UPDATED" :: Text)
  
deletePartnerR :: CompanyId->PartnerId -> Handler Value
deletePartnerR companyId id = do
    runDB $ delete id
    sendResponseStatus status200 ("DELETED" :: Text) 
