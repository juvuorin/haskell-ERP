
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DuplicateRecordFields #-}

{- 400 Bad Request
Invalid JWT serialization: Missing dot delimiter(s)

Support reference: bdcfefc2-7446-48c7-943b-17ef8563a47e
 -}
module Handler.Banking.Utils where
import Import hiding (httpLbs, map, fromList)
import External.Banking.Types
import External.Utils
import qualified Data.ByteString.Lazy.UTF8 as LBS
import Data.Aeson
--import Web.JWT
import Crypto.JWT
import Control.Monad.Except
import      Network.HTTP.Client             (httpLbs)

import Data.Aeson.Encode.Pretty
import Text.XML.HaXml (x)


getSetting ::(AppSettings->a)->Handler a 
getSetting x = do
  yesod <-getYesod
  return $ x $ appSettings yesod
