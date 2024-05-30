{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Partner.Partner where
import Import
import Data.Text (Text)

getPartnerR :: CompanyId->PartnerId -> Handler Value
getPartnerR cid id = do
    partner <- runDB $ get404 id
    returnJson partner

putPartnerR :: CompanyId->PartnerId -> (HandlerFor App) Value
putPartnerR cid id  = do
    partner <- (requireCheckJsonBodyForceCoherence:: Handler Partner)
    runDB $ replace id partner
    sendResponseStatus status200 ("UPDATED" :: Text)
  
deletePartnerR :: CompanyId->PartnerId -> Handler Value
deletePartnerR cid id = do
    runDB $ delete id
    sendResponseStatus status200 ("DELETED" :: Text) 
