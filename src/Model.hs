{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model where

import ClassyPrelude.Yesod
import Data.List
import Database.Persist.Postgresql (toSqlKey)
import Database.Persist.Quasi
import Types


--import Foundation (Handler)
--import Handler.Invoice.Purchase.CrudEndpoints.PurchaseInvoices
--import AccountType
--import DocumentType
--import CustomerType

type EntityType = Text

type EntityId = Int64

type FileName = Text









-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models.persistentmodels")

listVatPct =
  [ VatPct {vatPctType = Vat24, vatPctPct = 0.24, vatPctDescription = "24%"},
    VatPct {vatPctType = Vat14, vatPctPct = 0.14, vatPctDescription = "14%"},
    VatPct {vatPctType = Vat10, vatPctPct = 0.10, vatPctDescription = "10%"},
    VatPct {vatPctType = Vat0, vatPctPct = 0, vatPctDescription = "0%"}
  ]

listSaleVat =
  [ VatSale {vatSaleType = SaleDomestic, vatSaleDescription = "Kotimaa"},
    VatSale {vatSaleType = SaleEUGoods, vatSaleDescription = "EU yhteisömyynti, tavara"},
    VatSale {vatSaleType = SaleEUServices, vatSaleDescription = "EU yhteisömyynti, palvelu"},
    VatSale {vatSaleType = SaleZeroTaxBaseTurnover, vatSaleDescription = "0-verokannan mukainen liiketoiminta"},
    VatSale {vatSaleType = SaleEUTriangularTrade, vatSaleDescription = "EU kolmikantakauppa"},
    VatSale {vatSaleType = SaleOutsideEU, vatSaleDescription = "EU:n ulkopuolinen"},
    VatSale {vatSaleType = SaleOtherNonTaxed, vatSaleDescription = "Muu veroton myynti"},
    VatSale {vatSaleType = SaleConstructionReverseCharge24, vatSaleDescription = "Rakennuaslan käänteinen ALV 24%"},
    VatSale {vatSaleType = SaleNoVatHandling, vatSaleDescription = "Ei ALV-käsittelyä"}
  ]

listPurchaseVat =
  [ VatPurchase {vatPurchaseType = PurchaseDomestic, vatPurchaseDescription = "Kotimaa"},
    VatPurchase {vatPurchaseType = PurchaseReverseCharge24, vatPurchaseDescription = "Käännetty verovelvollisuus 24%"},
    VatPurchase {vatPurchaseType = PurchaseReverseCharge14, vatPurchaseDescription = "Käännetty verovelvollisuus 14%"},
    VatPurchase {vatPurchaseType = PurchaseReverseCharge10, vatPurchaseDescription = "Käännetty verovelvollisuus 10%"},
    VatPurchase {vatPurchaseType = PurchaseImport24, vatPurchaseDescription = "Maahantuonti 24%"},
    VatPurchase {vatPurchaseType = PurchaseImport14, vatPurchaseDescription = "Maahantuonti 14%"},
    VatPurchase {vatPurchaseType = PurchaseImport10, vatPurchaseDescription = "Maahantuonti 10%"},
    VatPurchase {vatPurchaseType = PurchaseImport0, vatPurchaseDescription = "Maahantuonti 0%"},
    VatPurchase {vatPurchaseType = PurchaseEUGoods24, vatPurchaseDescription = "EU 24% tavara"},
    VatPurchase {vatPurchaseType = PurchaseEUGoods14, vatPurchaseDescription = "EU 14% tavara"},
    VatPurchase {vatPurchaseType = PurchaseEUGoods10, vatPurchaseDescription = "EU 10% tavara"},
    VatPurchase {vatPurchaseType = PurchaseEUGoods0, vatPurchaseDescription = "EU 0% tavara"},
    VatPurchase {vatPurchaseType = PurchaseEUServices24, vatPurchaseDescription = "EU 24% palvelu"},
    VatPurchase {vatPurchaseType = PurchaseEUServices14, vatPurchaseDescription = "EU 14% palvelu"},
    VatPurchase {vatPurchaseType = PurchaseEUServices10, vatPurchaseDescription = "EU 10% palvelu"},
    VatPurchase {vatPurchaseType = PurchaseEUServices0, vatPurchaseDescription = "EU 0% palvelu"},
    VatPurchase {vatPurchaseType = PurchaseOutsideEU, vatPurchaseDescription = "EU:n ulkopuolinen"},
    VatPurchase {vatPurchaseType = PurchaseConstructionReverseCharge24, vatPurchaseDescription = "Rakennuaslan käänteinen ALV 24%"},
    VatPurchase {vatPurchaseType = PurchaseNoVatHandling, vatPurchaseDescription = "Ei ALV-käsittelyä"},
    VatPurchase {vatPurchaseType = PurchaseNoVatRegistration, vatPurchaseDescription = "Ei ALV rek."}
  ]

defTransaction :: Transaction
defTransaction =
  Transaction
    { transactionDate = ModifiedJulianDay 0,
      transactionMemo = (Just "Tosite"),
      transactionCreatedautomatically = Nothing,
      transactionValid = Nothing,
      transactionCompanyId = toSqlKey 0,
      transactionDebt = Nothing,
      transactionReceivable = Nothing,
      transactionSalary = Nothing,
      transactionPayable = Nothing,
      transactionAuto = Nothing,
      transactionType = TypeSalesInvoice,
      transactionEffectivefrom = Nothing,
      transactionEffectiveto = Nothing,
      transactionVatSaleId = Nothing,
      transactionVatPurchaseId = Nothing
    }

defPurchaseInvoice :: PurchaseInvoice
defPurchaseInvoice =
  PurchaseInvoice
    {  
      purchaseInvoiceOurreference = Nothing,
      purchaseInvoiceYourreference = Nothing,
      purchaseInvoiceType = DebitInvoice,
      purchaseInvoiceEntrydate = ModifiedJulianDay 0, --TransactionId Maybe      
      purchaseInvoiceTransactionId = Nothing, --TransactionId Maybe
      purchaseInvoicePartnerId = Nothing, --PartnerId Maybe
      purchaseInvoiceCompanyId = toSqlKey 1, --CompanyId
      purchaseInvoiceDuedate = ModifiedJulianDay 0, --  Day Maybe
      purchaseInvoiceDate = ModifiedJulianDay 0, --Day
      purchaseInvoiceCashdiscountpct = Nothing, --Double Maybe
      purchaseInvoiceBankreference = Nothing, --Text Maybe          -- should be fixed to RefNum tupe
      purchaseInvoiceBankmessage = Nothing, 
      purchaseInvoiceDeliveryterms = Nothing, --Text Maybe
      purchaseInvoicePenaltyinterest = Nothing, --Double Maybe
      purchaseInvoiceNotes = Nothing, --Text Maybe
      purchaseInvoiceNoticeperiod = Nothing, --Int Maybe
      purchaseInvoiceDocumentStatus = PurchaseInvoiceStatus PurchaseInvoiceStatusInvoiceCreated, --PurchaseInvoiceStatus Maybe
      purchaseInvoicePaidat = Nothing, --Day Maybe
      purchaseInvoicePaymentTermId = Nothing,
      purchaseInvoiceCashdiscountdate = Nothing,
      purchaseInvoiceCreditedinvoicenumber = Nothing,
      purchaseInvoiceInvoicenumber = Nothing,      purchaseInvoiceBankaccount = Nothing,
      purchaseInvoiceBankbic = Nothing,
      purchaseInvoiceCurrency = "EUR",
      purchaseInvoiceTotalexcludingvat = Nothing,
      purchaseInvoiceTotalincludingvat = 0

    }

defEntry =
  Entry
    { entryAccountId = toSqlKey 0,
      entryAmount = 0,
      entryTransactionId = toSqlKey 0,
      entryAutomationslavekey = Nothing,
      entryAutomationmasterkey = Nothing,
      entryMemo = Nothing,
      entryVatpct = Nothing,
      entryPropertyId = Nothing,
      entryVatPctId = Just $ toSqlKey 4,
      entryVatSaleId = Nothing,
      entryVatPurchaseId = Nothing
      --                , entryDate = fromGregorian 2022 1 1
    }
