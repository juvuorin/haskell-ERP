{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Company.Companies where
import Import
import Database.Persist.Sql (ConnectionPool, runSqlPool, fromSqlKey, toSqlKey)
import Crypto.Store.PKCS8 (OptProtected, readKeyFile, recover, writeKeyFileToMemory, PrivateKeyFormat (TraditionalFormat))
import Data.X509 (PrivKey)
import qualified Data.ByteString as BS

postAddTestCredsR ::Handler ()
postAddTestCredsR = do

    let file = "certificateStore/999/privatekey.pem"
    result<-liftIO (try (readKeyFile file ) :: IO (Either IOError [OptProtected PrivKey]))

    let certFile = "certificateStore/999/Certificate.pem"
    cert <- liftIO$ (try (BS.readFile certFile ) :: IO (Either IOError ByteString))

    let companyId = toSqlKey 1

    case cert of
      Right x -> runDB $ updateWhere [SecurityInfoCompanyId==.companyId] [SecurityInfoCertificate =. (Just x)]
      Left _ -> sendResponseStatus status404 ("Cannot find certificate!"::Text)

    case result of
      Right (privKey:_)-> do

        $(logInfo) (pack $ "Private key file path to create csr:"++file)
        let k = recover "" privKey
        privKey<- case k of
            Right privKey -> return $ writeKeyFileToMemory TraditionalFormat [privKey]
            Left err  -> sendResponseStatus status404 ("Cannot create a test key from file!"::Text)

        runDB $ updateWhere [SecurityInfoCompanyId==.companyId] [SecurityInfoCurrentprivatekey =. (Just privKey)]


      Right _-> sendResponseStatus status404 ("Cannot create a test key from file!"::Text)

      Left _ -> sendResponseStatus status404 ("Cannot create a test key from file!"::Text)

defineFilter :: Handler [Filter Company]
defineFilter = do
    authId <- maybeAuthId
    
    case authId of
      Nothing -> sendResponseStatus status403 ("Not authenticated"::Text)
      Just key -> do
          allowedCompanies <- runDB $ selectList [UserCompanyUserId ==. key][]
          let keys = map (\item-> userCompanyCompanyId $ entityVal item) allowedCompanies
          return [CompanyId<-.keys]

selectCompanies :: Handler [Entity Company]
selectCompanies = do
  defineFilter >>= (\filter->runDB $ selectList filter [Asc CompanyId] :: Handler [Entity Company])

getCompaniesR :: Handler Value
getCompaniesR = do
  toJSON <$> selectCompanies

postCompaniesR :: Handler ()
postCompaniesR = do
    company <- requireCheckJsonBody :: Handler Company
    id  <- runDB $ insert company
    sendResponseStatus status201 (object ["id" .=( fromSqlKey id)])

