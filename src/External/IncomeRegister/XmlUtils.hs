{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts, DuplicateRecordFields, CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module External.IncomeRegister.XmlUtils
    ( module External.IncomeRegister.XmlUtils
    ) where
import Text.XML.HaXml
    ( tag, o, children, addAttribute, replaceTag )

-- Before signing the SignedInfo one needs to add an attribute for the
-- http://www.w3.org/2000/09/xmldsig# namespace 
addSignedInfoAttributeForSigning x = (
                addAttribute "xmlns" "http://www.w3.org/2000/09/xmldsig#" `o`
                tag "SignedInfo") 
                (head x)

cleanStatusResponseFromIR x = (
                replaceTag "StatusResponseFromIR" `o`
                tag "ns0:StatusResponseFromIR" `o` 
                children `o`
                children)
                (head x)

addWageReportAttributes x = (
                replaceTag "wrtir:WageReportRequestToIR" `o`
                addAttribute "xmlns:wrtir" "http://www.tulorekisteri.fi/2017/1/WageReportsToIR" `o`
                addAttribute "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance" `o`
                addAttribute "xsi:schemaLocation" "http://www.tulorekisteri.fi/2017/1/WageReportsToIR WageReportsToIR.xsd" `o`
                tag "WageReportRequestToIR") 
                (head x)

addPayerSummaryAttributes x = (
                replaceTag "psrtir:PayerSummaryReportRequestToIR" `o`
                addAttribute "xmlns:psrtir" "http://www.tulorekisteri.fi/2017/1/PayerSummaryReportsToIR" `o`
                addAttribute "xmlns:xsd" "http://www.w3.org/2001/XMLSchema"  `o`
                addAttribute "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance" `o`
                tag "PayerSummaryReportRequestToIR") 
                (head x)
