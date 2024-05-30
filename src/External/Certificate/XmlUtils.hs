{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module External.Certificate.XmlUtils where

import Text.XML.HaXml

-- | Attribute order counts! (keep it alphabetical)
addSignNewCertificateRequestAttributes x =
  ( replaceTag "cer:SignNewCertificateRequest"
      `o` addAttribute "xmlns:cer" "http://certificates.vero.fi/2017/10/certificateservices"
      `o` tag "SignNewCertificateRequest"
  )
    (head x)

--   "<ser:GetCertificateRequest xmlns:ser=\"http://certificates.vero.fi/2017/10/certificateservices\">",
addGetCertificateRequestAttributes x =
  ( replaceTag "ser:GetCertificateRequest"
      `o` addAttribute "xmlns:ser" "http://certificates.vero.fi/2017/10/certificateservices"
      `o` tag "GetCertificateRequest"
  )
    (head x)

--                                        "<ser:RenewCertificateRequest xmlns:ser=\"http://certificates.vero.fi/2017/10/certificateservices\">",

addRenewCertificateRequestAttributes x =
  ( replaceTag "ser:RenewCertificateRequest"
      `o` addAttribute "xmlns:ser" "http://certificates.vero.fi/2017/10/certificateservices"
      `o` tag "RenewCertificateRequest"
  )
    (head x)

addPayerSummaryAttributes :: [Content i] -> [Content i]
addPayerSummaryAttributes x =
  ( replaceTag "psrtir:PayerSummaryReportRequestToIR"
      `o` addAttribute "xmlns:psrtir" "http://www.tulorekisteri.fi/2017/1/PayerSummaryReportsToIR"
      `o` addAttribute "xmlns:xsd" "http://www.w3.org/2001/XMLSchema"
      `o` addAttribute "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance"
      `o` tag "PayerSummaryReportRequestToIR"
  )
    (head x)

addApitamoDeliveryDataSendRequestAttributes :: [Content i] -> [Content i]
addApitamoDeliveryDataSendRequestAttributes x =
  ( 
      addAttribute "xmlns" "http://www.vero.fi/xmlschema/ApiTaMo"
      `o` tag "DeliveryDataSendRequest"
  )
    (head x)

