{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Event.Event where
import Import
import Data.Time.LocalTime (utcToLocalZonedTime)
getEventsR :: CompanyId->Handler Value
getEventsR companyId = do
    events <- runDB $ selectList ([EventCompanyId==. Just companyId] ||. [EventCompanyId==.Nothing]) [] :: Handler [Entity Event]
    returnJson events