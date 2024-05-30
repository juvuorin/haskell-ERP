{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Certificates.Cert where
import Import
import External.Certificate.CertificateService
    ( runRenewCertificate,
      runSignNewCertificate )

 
postCertRenewR :: CompanyId->Handler Value
postCertRenewR companyId= 
      do
        runDB $ logEvent "Sending certification renewal request" Nothing
        result <- runRenewCertificate companyId 
        case result of 
            Right s ->runDB $ logEvent "Sending certification renewal request completed successfully" Nothing     
            Left s ->runDB $ logEvent "Sending certification renewal request failed" Nothing            
        returnJson result

postCertSignR :: CompanyId->Handler Value
postCertSignR companyId= do
            runDB $ logEvent "Sending sign new certificate request" Nothing

            result <-  runSignNewCertificate companyId
            case result of 
                Right s ->runDB $ logEvent "Sending sign new certificate request completed successfully " Nothing  
                Left s ->runDB $ logEvent "Sending sign new certificate request failed" Nothing            
            returnJson result

