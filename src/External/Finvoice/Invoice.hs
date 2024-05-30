{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts    ***REMOVED***-}
{-# LANGUAGE KindSignatures      ***REMOVED***-}
{-# LANGUAGE NoImplicitPrelude   ***REMOVED***-}
{-# LANGUAGE OverloadedStrings   ***REMOVED***-}
{-# LANGUAGE Rank2Types          ***REMOVED***-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-} 

{- 400 Bad Request
Invalid JWT serialization: Missing dot delimiter(s)

Support reference: bdcfefc2-7446-48c7-943b-17ef8563a47e
 -}
module External.Finvoice.Invoice where
import Import hiding (httpLbs, map, fromList,newManager)
import           External.Utils                  (mkMngr, mkMngrDefaultTls)

import           Network.HTTP.Client             (httpLbs, newManager)
import qualified Data.ByteString.Lazy.UTF8 as LBS
import Data.Aeson
--import Web.JWT
import Crypto.JWT
import Control.Monad.Except ( runExceptT )
import Data.Finvoice30
import Data.Aeson.Encode.Pretty
import Text.XML.HaXml (x, Verbatim (verbatim))
import Network.HTTP.Client.MultipartFormData (formDataBody, partFileRequestBody, partFileSource, partBS)
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Text.XML.HaXml.Schema.Schema as Schema
import Database.Persist.Postgresql (toSqlKey)
import Prelude (read)
import qualified Data.Text
import Data.Time (showGregorian)
import Data.List (head)
import Text.Pretty.Simple (pPrint)

data BasicDataInvoice = BasicDataInvoice {           
      invoiceDetailsType_invoiceDate::Date,
      invoiceDetailsType_invoiceTypeCode :: InvoiceTypeCodeTypeFI,

      epiIdentificationDetailsType_epiDate::Date,
      epiDetailsType_epiIdentificationDetails::EpiIdentificationDetailsType,

      sellerPartyDetailsType_sellerOrganisationName :: [GenericStringType2_70],

      finvoice_invoiceRow ::  InvoiceRowType,
      finvoice_buyerPartyDetails::BuyerPartyDetailsType,
      finvoice_epiDetails::EpiDetailsType, 
      finvoice_invoiceDetails:: InvoiceDetailsType,
      finvoice_version :: Xs.XsdString, 
      finvoice_messageTransmissionDetails:: Maybe MessageTransmissionDetailsType,
      finvoice_sellerPartyDetails:: SellerPartyDetailsType,
        

      messageSenderDetails_fromIdentifier :: ElectronicAddrIdType,
      messageSenderDetails_fromIntermediator:: GenericStringType2_35, 
                
      messageReceiverDetails_toIdentifier :: ElectronicAddrIdType,
      messageReceiverDetails_toIntermediator :: GenericStringType2_35, 
      
      messageDetails_messageIdentifier :: GenericStringType2_48, 
      messageDetails_messageTimeStamp :: Xs.DateTime }

  
buildInvoice= 

  let dueDate = Date (DateType 19700101) (DateAttributes {dateAttributes_format=Xs.XsdString "CCYYMMDD"})
      date = Date (DateType 19700101) (DateAttributes {dateAttributes_format=Xs.XsdString "CCYYMMDD"})
 
    
    
      partyIdentifierType = PartyIdentifierType (GenericStringType0_70 $ Xs.XsdString "partyIdentifier")  partyIdentifierTypeAttributes
      partyIdentifierTypeAttributes = PartyIdentifierTypeAttributes
        { partyIdentifierTypeAttributes_identifierType = Nothing -- :: Maybe GenericTokenType1_20
        , partyIdentifierTypeAttributes_schemeID = Nothing -- :: Maybe Iso6523cid
        }

      sellerPartyDetailsType = SellerPartyDetailsType
              { sellerPartyDetailsType_sellerPartyIdentifier = Nothing -- :: Maybe PartyLegalRegIdType
              , sellerPartyDetailsType_sellerPartyIdentifierUrlText = Nothing -- :: Maybe GenericStringType0_512
              , sellerPartyDetailsType_sellerOrganisationName = [GenericStringType2_70 $ Xs.XsdString "seller organisation name"] -- :: [GenericStringType2_70}]
              , sellerPartyDetailsType_sellerOrganisationTradingName = Nothing -- :: Maybe GenericStringType2_70
              , sellerPartyDetailsType_sellerOrganisationDepartment =[] --  [GenericStringType0_35 $ Xs.XsdString "org dep"] -- :: [GenericStringType0_35]
              , sellerPartyDetailsType_sellerOrganisationTaxCode = Nothing -- :: Maybe GenericStringType0_35
              , sellerPartyDetailsType_sellerOrganisationTaxCodeUrlText = Nothing -- :: Maybe GenericStringType0_512
              , sellerPartyDetailsType_sellerCode = [] -- [partyIdentifierType] -- :: [PartyIdentifierType]
              , sellerPartyDetailsType_sellerPostalAddressDetails = Nothing -- :: Maybe SellerPostalAddressDetailsType
              }

      buyerPartyDetailsType =BuyerPartyDetailsType
              { buyerPartyDetailsType_buyerPartyIdentifier = Nothing  -- :: Maybe PartyLegalRegIdType
              , buyerPartyDetailsType_buyerOrganisationName = [GenericStringType2_70 $ Xs.XsdString "buyer organisation name" ]
              , buyerPartyDetailsType_buyerOrganisationTradingName =Nothing -- :: Maybe GenericStringType2_70
              , buyerPartyDetailsType_buyerOrganisationDepartment = [] -- [GenericStringType0_35 $ Xs.XsdString "buyer dept"]
              , buyerPartyDetailsType_buyerOrganisationTaxCode = Nothing -- :: Maybe GenericStringType0_35
              , buyerPartyDetailsType_buyerCode = Nothing -- :: Maybe PartyIdentifierType
              , buyerPartyDetailsType_buyerPostalAddressDetails = Nothing  -- Maybe BuyerPostalAddressDetailsType
              }

      AnyPartyDetailsType {..}=AnyPartyDetailsType
              { anyPartyDetailsType_anyPartyText = Anypartytexttype0_35 $ Xs.XsdString "any party text" -- :: Anypartytexttype0_35
              , anyPartyDetailsType_anyPartyIdentifier = Nothing -- :: Maybe PartyLegalRegIdType
              , anyPartyDetailsType_anyPartyOrganisationName = [] -- [GenericStringType2_35 $ Xs.XsdString "any party text"] -- :: [GenericStringType2_35]
              , anyPartyDetailsType_anyPartyOrganisationDepartment = [] -- [GenericStringType0_35 $ Xs.XsdString "any party text"] -- :: [GenericStringType0_35]
              , anyPartyDetailsType_anyPartyOrganisationTaxCode = Nothing -- :: Maybe GenericStringType0_35
              , anyPartyDetailsType_anyPartyCode = Nothing -- :: Maybe PartyIdentifierType
              , anyPartyDetailsType_anyPartyContactPersonName = Nothing -- :: Maybe GenericStringType0_70
              , anyPartyDetailsType_anyPartyContactPersonFunction = [] -- [GenericStringType0_35 $ Xs.XsdString "func"] -- :: [GenericStringType0_35]
              , anyPartyDetailsType_anyPartyContactPersonDepartment = [] -- [GenericStringType0_35 $ Xs.XsdString "dept"] -- :: [GenericStringType0_35]
              , anyPartyDetailsType_anyPartyCommunicationDetails  = Nothing -- :: Maybe AnyPartyCommunicationDetailsType
              , anyPartyDetailsType_anyPartyPostalAddressDetails = Nothing -- :: Maybe AnyPartyPostalAddressDetails
              , anyPartyDetailsType_anyPartyOrganisationUnitNumber = Nothing -- :: Maybe GenericStringType0_35
              , anyPartyDetailsType_anyPartySiteCode = Nothing -- :: Maybe GenericStringType0_35
              }

      invoiceTypeCodeTypeFI = InvoiceTypeCodeTypeFI (InvoiceTypeCodePatternFI (Xs.NMTOKEN "INV01"))
            (InvoiceTypeCodeTypeFIAttributes {invoiceTypeCodeTypeFIAttributes_codeListAgencyIdentifier = Nothing })

      originalInvoiceReferenceType = OriginalInvoiceReferenceType
                { originalInvoiceReferenceType_invoiceNumber = Nothing -- :: Maybe GenericStringType1_20
                , originalInvoiceReferenceType_invoiceDate = Nothing -- :: Maybe Date
                }

      definitionDetails = DefinitionDetails
              { definitionDetails_definitionHeaderText = definitionHeaderText  -- :: DefinitionHeaderText
              , definitionDetails_definitionValue= Nothing -- :: Maybe QuantityType0_70
              }
      definitionHeaderText = DefinitionHeaderText (GenericStringType0_70 $ Xs.XsdString "definitionHeaderText") $ definitionHeaderTextAttributes

      definitionHeaderTextAttributes = DefinitionHeaderTextAttributes
          { definitionHeaderTextAttributes_definitionCode = Nothing -- :: Maybe GenericTokenType1_20
          }

      vatSpecificationDetailsType = VatSpecificationDetailsType
              { vatSpecificationDetailsType_vatBaseAmount = Nothing
              , vatSpecificationDetailsType_vatRatePercent = Nothing
              , vatSpecificationDetailsType_vatCode = Nothing
              , vatSpecificationDetailsType_vatRateAmount = Nothing
              , vatSpecificationDetailsType_vatFreeText = [] -- [GenericStringType0_70 $ Xs.XsdString "free vat text"]
              , vatSpecificationDetailsType_vatExemptionReasonCode = Nothing
              }


      headerValueType = HeaderValueType
              { headerValueType_header = Nothing -- :: Maybe GenericStringType1_35
              , headerValueType_value = [] -- [GenericStringType1_70 $ Xs.XsdString "header"]
              }
      cashDiscountVatDetails = CashDiscountVatDetails
              { cashDiscountVatDetails_cashDiscountVatPercent = Percentage $ Xs.XsdString "0"
              , cashDiscountVatDetails_cashDiscountVatAmount = amount -- :: Amount
              }


      paymentTermsDetailsType = PaymentTermsDetailsType
              { paymentTermsDetailsType_paymentTermsFreeText = [] -- [GenericStringType0_70 $ Xs.XsdString "free text"] -- :: [GenericStringType0_70]
              , paymentTermsDetailsType_freeText = [] -- [headerValueType] -- :: [HeaderValueType]
              , paymentTermsDetailsType_invoiceDueDate = Nothing -- :: Maybe Date
              , paymentTermsDetailsType_cashDiscountDate = Nothing -- :: Maybe Date
              , paymentTermsDetailsType_cashDiscountBaseAmount = Nothing  -- :: Maybe Amount
              , paymentTermsDetailsType_cashDiscountPercent = Nothing -- :: Maybe Percentage
              , paymentTermsDetailsType_cashDiscountAmount = Nothing -- :: Maybe Amount
              , paymentTermsDetailsType_cashDiscountExcludingVatAmount = Nothing -- :: Maybe Amount
              , paymentTermsDetailsType_cashDiscountVatDetails = [] -- [cashDiscountVatDetails] -- :: [CashDiscountVatDetails]
              , paymentTermsDetailsType_reducedInvoiceVatIncludedAmount = Nothing -- :: Maybe Amount
              , paymentTermsDetailsType_paymentOverDueFineDetails = Nothing -- :: Maybe PaymentOverDueFineDetailsType
              }
      discountDetailsType = DiscountDetailsType
              { discountDetailsType_freeText = Nothing -- ::Maybe GenericStringType1_70
              , discountDetailsType_reasonCode = Nothing -- :: Maybe Untdid5189
              , discountDetailsType_percent = Nothing -- :: Maybe Percentage
              , discountDetailsType_amount = Nothing -- :: Maybe Amount
              , discountDetailsType_baseAmount = Nothing -- :: Maybe Amount
              , discountDetailsType_vatCategoryCode = Nothing -- :: Maybe VatCategoryCodeType
              , discountDetailsType_vatRatePercent = Nothing -- :: Maybe Percentage
              }

      invoiceChargeDetailsType = InvoiceChargeDetailsType
              { invoiceChargeDetailsType_reasonText = Nothing -- :: Maybe GenericStringType1_70
              , invoiceChargeDetailsType_reasonCode = Nothing -- :: Maybe Untdid7161
              , invoiceChargeDetailsType_percent = Nothing -- :: Maybe Percentage
              , invoiceChargeDetailsType_amount = Nothing -- :: MaybUnitAmountTypeOneOe Amount
              , invoiceChargeDetailsType_baseAmount = Nothing -- :: Maybe Amount
              , invoiceChargeDetailsType_vatCategoryCode = Nothing -- :: Maybe VatCategoryCodeType
              , invoiceChargeDetailsType_vatRatePercent = Nothing -- :: Maybe Percentage
              }


      monetaryAmount = MonetaryAmount $ Xs.Token "0"

      amount = Amount monetaryAmount amountAttributes
      amountAttributes = AmountAttributes
          { amountAttributes_amountCurrencyIdentifier =  Xs.XsdString "EUR"  -- :: Xs.XsdString
          }


      invoiceDetailsType = InvoiceDetailsType
              { invoiceDetailsType_invoiceTypeCode = invoiceTypeCodeTypeFI -- :: InvoiceTypeCodeTypeFI
              , invoiceDetailsType_invoiceTypeCodeUN = Nothing -- :: Maybe Untdid1001
              , invoiceDetailsType_invoiceTypeText = GenericStringType1_35 $ Xs.XsdString "Invoice" -- :: GenericStringType1_35
              , invoiceDetailsType_invoiceClassification = Nothing -- :: Maybe InvoiceClassificationType
              , invoiceDetailsType_originCode = OriginCodeType_Original -- :: OriginCodeType
              , invoiceDetailsType_originText = Nothing -- :: Maybe GenericStringType0_35
              , invoiceDetailsType_invoicedObjectID = Nothing -- :: Maybe InvoicedObjectIDType
              , invoiceDetailsType_invoiceNumber = GenericStringType1_20 $ Xs.XsdString "invoiceNumber" -- :: GenericStringType1_20
              , invoiceDetailsType_invoiceDate = date -- :: Date
              , invoiceDetailsType_originalInvoiceNumber = Nothing -- :: Maybe GenericStringType1_20
              , invoiceDetailsType_originalInvoiceDate = Nothing -- :: Maybe Date
              , invoiceDetailsType_originalInvoiceReference = [] -- [originalInvoiceReferenceType] -- :: [OriginalInvoiceReferenceType]
              , invoiceDetailsType_invoicingPeriodStartDate = Nothing -- :: Maybe Date
              , invoiceDetailsType_invoicingPeriodEndDate = Nothing -- :: Maybe Date
              , invoiceDetailsType_sellerReferenceIdentifier = Nothing -- :: Maybe GenericStringType0_70
              , invoiceDetailsType_sellerReferenceIdentifierUrlText = Nothing -- :: Maybe GenericStringType0_512
              , invoiceDetailsType_buyersSellerIdentifier = Nothing -- :: Maybe PartyIdentifierType
              , invoiceDetailsType_sellersBuyerIdentifier = Nothing -- :: Maybe PartyIdentifierType
              , invoiceDetailsType_orderIdentifier = Nothing -- :: Maybe GenericStringType0_70
              , invoiceDetailsType_orderIdentifierUrlText = Nothing -- :: Maybe GenericStringType0_512
              , invoiceDetailsType_orderDate = Nothing -- :: Maybe Date
              , invoiceDetailsType_ordererName = Nothing -- :: Maybe GenericStringType0_35
              , invoiceDetailsType_salesPersonName = Nothing -- :: Maybe GenericStringType0_35
              , invoiceDetailsType_orderConfirmationIdentifier = Nothing -- :: Maybe GenericStringType0_70
              , invoiceDetailsType_orderConfirmationDate = Nothing -- :: Maybe Date
              , invoiceDetailsType_agreementIdentifier = Nothing -- :: Maybe GenericStringType0_70
              , invoiceDetailsType_agreementIdentifierUrlText = Nothing -- :: Maybe GenericStringType0_512
              , invoiceDetailsType_agreementTypeText = Nothing -- :: Maybe GenericStringType0_35
              , invoiceDetailsType_agreementTypeCode = Nothing -- :: Maybe GenericStringType0_35
              , invoiceDetailsType_agreementDate = Nothing -- :: Maybe Date
              , invoiceDetailsType_notificationIdentifier = Nothing -- :: Maybe GenericStringType0_70
              , invoiceDetailsType_notificationDate = Nothing -- :: Maybe Date
              , invoiceDetailsType_registrationNumberIdentifier = Nothing -- :: Maybe GenericStringType0_70
              , invoiceDetailsType_controllerIdentifier = Nothing -- :: Maybe GenericStringType0_70
              , invoiceDetailsType_controllerName = Nothing -- :: Maybe GenericStringType0_35
              , invoiceDetailsType_controlDate = Nothing -- :: Maybe Date
              , invoiceDetailsType_buyerReferenceIdentifier = Nothing -- :: Maybe GenericStringType0_70
              , invoiceDetailsType_projectReferenceIdentifier = Nothing -- :: Maybe GenericStringType0_70
              , invoiceDetailsType_definitionDetails = [] -- [definitionDetails] -- :: [DefinitionDetails]
              , invoiceDetailsType_rowsTotalVatExcludedAmount = Nothing -- :: Maybe Amount
              , invoiceDetailsType_discountsTotalVatExcludedAmount = Nothing -- :: Maybe Amount
              , invoiceDetailsType_chargesTotalVatExcludedAmount = Nothing -- :: Maybe Amount
              , invoiceDetailsType_invoiceTotalVatExcludedAmount = Nothing -- :: Maybe Amount
              , invoiceDetailsType_invoiceTotalVatAmount = Nothing -- :: Maybe Amount
              , invoiceDetailsType_invoiceTotalVatAccountingAmount = Nothing -- :: Maybe Amount
              , invoiceDetailsType_invoiceTotalVatIncludedAmount = amount -- :: Amount
              , invoiceDetailsType_invoiceTotalRoundoffAmount = Nothing -- :: Maybe Amount
              , invoiceDetailsType_invoicePaidAmount = Nothing -- :: Maybe Amount
              , invoiceDetailsType_exchangeRate = Nothing -- :: Maybe ExchangeRate
              , invoiceDetailsType_otherCurrencyAmountVatExcludedAmount = Nothing -- :: Maybe Amount
              , invoiceDetailsType_otherCurrencyAmountVatIncludedAmount = Nothing -- :: Maybe Amount
              , invoiceDetailsType_creditLimitAmount = Nothing -- :: Maybe Amount
              , invoiceDetailsType_creditInterestPercent = Nothing -- :: Maybe Percentage
              , invoiceDetailsType_operationLimitAmount = Nothing -- :: Maybe Amount
              , invoiceDetailsType_monthlyAmount  = Nothing -- :: Maybe Amount
              , invoiceDetailsType_shortProposedAccountIdentifier = Nothing -- :: Maybe GenericNMtokenType0_4
              , invoiceDetailsType_normalProposedAccountIdentifier = Nothing -- :: Maybe GenericNMtokenType0_4
              , invoiceDetailsType_proposedAccountText = Nothing -- :: Maybe GenericStringType0_35
              , invoiceDetailsType_accountDimensionText = Nothing -- :: Maybe GenericStringType0_35
              , invoiceDetailsType_sellerAccountText = Nothing -- :: Maybe GenericStringType0_35
              , invoiceDetailsType_vatPoint = Nothing -- :: Maybe VatPointType
              , invoiceDetailsType_vatSpecificationDetails = [] -- [vatSpecificationDetailsType] -- :: [VatSpecificationDetailsType]
              , invoiceDetailsType_invoiceFreeText = [] -- [GenericStringType0_512 $ Xs.XsdString "vapaa teksti"] -- :: [GenericStringType0_512]
              , invoiceDetailsType_invoiceVatFreeText = Nothing -- :: Maybe GenericStringType0_70
              , invoiceDetailsType_paymentTermsDetails = [] -- [paymentTermsDetailsType] -- :: [PaymentTermsDetailsType]
              , invoiceDetailsType_discountDetails = [] -- [discountDetailsType] --  :: [DiscountDetailsType]
              , invoiceDetailsType_chargeDetails = [] -- [invoiceChargeDetailsType] -- :: [InvoiceChargeDetailsType]
              , invoiceDetailsType_tenderReference = Nothing -- :: Maybe GenericStringType1_70
              }

    


      partialPaymentDetailsType = PartialPaymentDetailsType
              { partialPaymentDetailsType_paidAmount = amount -- :: Amount
              , partialPaymentDetailsType_paidVatExcludedAmount = Nothing -- :: Maybe Amount
              , partialPaymentDetailsType_unPaidAmount = amount -- :: Amount
              , partialPaymentDetailsType_unPaidVatExcludedAmount = Nothing -- :: Maybe Amount
              , partialPaymentDetailsType_interestPercent = Nothing -- :: Maybe Percentage
              , partialPaymentDetailsType_prosessingCostsAmount = Nothing -- :: Maybe Amount
              , partialPaymentDetailsType_partialPaymentVatIncludedAmount = [] -- [amount] -- :: [Amount]
              , partialPaymentDetailsType_partialPaymentVatExcludedAmount = [] -- [amount] -- :: [Amount]
              , partialPaymentDetailsType_partialPaymentDueDate = [] -- [dueDate] -- :: [Date]
              , partialPaymentDetailsType_partialPaymentReferenceIdentifier =[] --  [GenericStringType2_35 $ Xs.XsdString "3434"]
              }


      --invoiceRowType = InvoiceRowType { invoiceRowType_choice0 = OneOf2 invoiceRowGroup}
      invoiceRowType = InvoiceRowType { invoiceRowType_choice0 = TwoOf2 [invoiceSubInvoiceRow]}
    

      articleGroupIdentifierType = ArticleGroupIdentifierType (GenericStringType0_70 $ Xs.XsdString "articleGroupIdentifier") articleGroupIdentifierTypeAttributes
      articleGroupIdentifierTypeAttributes = ArticleGroupIdentifierTypeAttributes
          { articleGroupIdentifierTypeAttributes_schemeID = Nothing -- :: Maybe Untdid7143
          , articleGroupIdentifierTypeAttributes_schemeVersion = Nothing -- :: Maybe GenericStringType1_35
          }

      rowChargeDetailsType = RowChargeDetailsType
              { rowChargeDetailsType_reasonText = Nothing -- :: Maybe GenericStringType1_70
              , rowChargeDetailsType_reasonCode = Nothing -- :: Maybe Untdid7161
              , rowChargeDetailsType_percent = Nothing -- :: Maybe Percentage
              , rowChargeDetailsType_amount = Nothing -- :: Maybe Amount
              , rowChargeDetailsType_baseAmount = Nothing -- :: Maybe Amount
              }

      rowProgressiveDiscountDetailsType = RowProgressiveDiscountDetailsType
              { rowProgressiveDiscountDetailsType_rowDiscountPercent = Nothing -- :: Maybe Percentage
              , rowProgressiveDiscountDetailsType_rowDiscountAmount = Nothing -- :: Maybe Amount
              , rowProgressiveDiscountDetailsType_rowDiscountBaseAmount = Nothing -- :: Maybe Amount
              , rowProgressiveDiscountDetailsType_rowDiscountTypeCode = Nothing -- :: Maybe Untdid5189
              , rowProgressiveDiscountDetailsType_rowDiscountTypeText = Nothing -- :: Maybe GenericStringType0_35
              }

      rowAnyPartyDetailsType = RowAnyPartyDetailsType
              { rowAnyPartyDetailsType_rowAnyPartyText = Anypartytexttype0_35 $ Xs.XsdString $ "rowAnyPartyText" -- :: Anypartytexttype0_35
              , rowAnyPartyDetailsType_rowAnyPartyIdentifier = Nothing -- :: Maybe PartyLegalRegIdType
              , rowAnyPartyDetailsType_rowAnyPartyOrganisationName = [] -- [GenericStringType2_35 $ Xs.XsdString "org name"] -- :: [GenericStringType2_35]
              , rowAnyPartyDetailsType_rowAnyPartyOrganisationDepartment = [GenericStringType0_35 $ Xs.XsdString "department name"] --  :: [GenericStringType0_35]
              , rowAnyPartyDetailsType_rowAnyPartyOrganisationTaxCode = Nothing -- Maybe GenericStringType0_35
              , rowAnyPartyDetailsType_rowAnyPartyCode = Nothing -- Maybe PartyIdentifierType
              , rowAnyPartyDetailsType_rowAnyPartyPostalAddressDetails = Nothing -- Maybe RowAnyPartyPostalAddressDetails
              , rowAnyPartyDetailsType_rowAnyPartyOrganisationUnitNumber = Nothing -- Maybe GenericStringType0_35
              , rowAnyPartyDetailsType_rowAnyPartySiteCode = Nothing -- Maybe GenericStringType0_35
              }
      -- newtype QuantityType0_14 = QuantityType0_14 Xs.XsdString deriving (Eq,Show)

      rowDefinitionHeaderText = RowDefinitionHeaderText (GenericStringType0_70 $ Xs.XsdString "rowDefinitionHeaderText") rowDefinitionHeaderTextAttributes
      rowDefinitionHeaderTextAttributes = RowDefinitionHeaderTextAttributes
          { rowDefinitionHeaderTextAttributes_definitionCode = Nothing -- :: Maybe GenericTokenType1_20
          }

      rowDefinitionDetailsType = RowDefinitionDetailsType
              { rowDefinitionDetailsType_rowDefinitionHeaderText = rowDefinitionHeaderText -- :: RowDefinitionHeaderText
              , rowDefinitionDetailsType_rowDefinitionValue = Nothing -- :: Maybe QuantityType0_70
              }


      invoiceRowGroup = InvoiceRowGroup
              { invoiceRowGroup_rowSubIdentifier = Nothing -- :: Maybe GenericStringType0_35
              , invoiceRowGroup_invoicedObjectID = Nothing -- :: Maybe InvoicedObjectIDType
              , invoiceRowGroup_articleIdentifier = Nothing -- :: Maybe GenericStringType0_70
              , invoiceRowGroup_articleGroupIdentifier = [] -- [articleGroupIdentifierType] -- ::  [ArticleGroupIdentifierType]
              , invoiceRowGroup_articleName = Nothing -- :: Maybe GenericStringType0_100
              , invoiceRowGroup_articleDescription = Nothing -- :: Maybe GenericStringType0_512
              , invoiceRowGroup_articleInfoUrlText = Nothing -- :: Maybe GenericStringType0_512
              , invoiceRowGroup_buyerArticleIdentifier = Nothing -- :: Maybe GenericStringType0_70
              , invoiceRowGroup_eanCode = Nothing -- :: Maybe EanCodeType
              , invoiceRowGroup_rowRegistrationNumberIdentifier = Nothing -- :: Maybe GenericStringType0_70
              , invoiceRowGroup_serialNumberIdentifier = Nothing -- :: Maybe GenericStringType0_70
              , invoiceRowGroup_rowActionCode = Nothing -- :: Maybe GenericTokenType0_35
              , invoiceRowGroup_rowDefinitionDetails = [] -- [rowDefinitionDetailsType] -- :: [RowDefinitionDetailsType]
              , invoiceRowGroup_offeredQuantity = [] -- [QuantityType0_14 $ Xs.XsdString "100"] -- :: [QuantityType0_14]
              , invoiceRowGroup_deliveredQuantity = [] -- [QuantityType0_14 $ Xs.XsdString "100"] -- :: [QuantityType0_14]
              , invoiceRowGroup_orderedQuantity = Nothing -- :: Maybe QuantityType0_14
              , invoiceRowGroup_confirmedQuantity = Nothing -- :: Maybe QuantityType0_14
              , invoiceRowGroup_postDeliveredQuantity = Nothing -- :: Maybe QuantityType0_14
              , invoiceRowGroup_invoicedQuantity  = [] -- [QuantityType0_14 $ Xs.XsdString "100"] -- :: [QuantityType0_14]
              , invoiceRowGroup_creditRequestedQuantity = Nothing -- :: Maybe QuantityType0_14
              , invoiceRowGroup_returnedQuantity = Nothing -- :: Maybe QuantityType0_14
              , invoiceRowGroup_startDate = Nothing -- :: Maybe Date
              , invoiceRowGroup_endDate = Nothing -- :: Maybe Date
              , invoiceRowGroup_unitPriceAmount = Nothing -- :: Maybe UnitAmountUN
              , invoiceRowGroup_unitPriceDiscountAmount = Nothing -- :: Maybe UnitAmountUN
              , invoiceRowGroup_unitPriceNetAmount = Nothing -- :: Maybe UnitAmountUN
              , invoiceRowGroup_unitPriceVatIncludedAmount = Nothing -- :: Maybe UnitAmountUN
              , invoiceRowGroup_unitPriceBaseQuantity = Nothing -- :: Maybe QuantityType0_14
              , invoiceRowGroup_rowIdentifier = Nothing -- :: Maybe GenericStringType0_35
              , invoiceRowGroup_rowIdentifierUrlText = Nothing -- :: Maybe GenericStringType0_512
              , invoiceRowGroup_rowOrderPositionIdentifier = Nothing -- :: Maybe GenericStringType0_35
              , invoiceRowGroup_rowIdentifierDate = Nothing -- :: Maybe Date
              , invoiceRowGroup_rowPositionIdentifier = Nothing -- :: Maybe GenericStringType0_35
              , invoiceRowGroup_originalInvoiceNumber = Nothing -- :: Maybe GenericStringType1_20
              , invoiceRowGroup_originalInvoiceDate = Nothing -- :: Maybe Date
              , invoiceRowGroup_originalInvoiceReference = [] -- [originalInvoiceReferenceType] -- :: [OriginalInvoiceReferenceType]
              , invoiceRowGroup_rowOrdererName = Nothing -- :: Maybe GenericStringType0_35
              , invoiceRowGroup_rowSalesPersonName = Nothing -- :: Maybe GenericStringType0_35
              , invoiceRowGroup_rowOrderConfirmationIdentifier = Nothing -- :: Maybe GenericStringType0_70
              , invoiceRowGroup_rowOrderConfirmationDate = Nothing -- :: Maybe Date
              , invoiceRowGroup_rowDeliveryIdentifier = Nothing -- :: Maybe GenericStringType0_35
              , invoiceRowGroup_rowDeliveryIdentifierUrlText = Nothing -- :: Maybe GenericStringType0_512
              , invoiceRowGroup_rowDeliveryDate = Nothing -- :: Maybe Date
              , invoiceRowGroup_rowQuotationIdentifier = Nothing -- :: Maybe GenericStringType0_35
              , invoiceRowGroup_rowQuotationIdentifierUrlText = Nothing -- :: Maybe GenericStringType0_512
              , invoiceRowGroup_rowAgreementIdentifier = Nothing -- :: Maybe GenericStringType0_70
              , invoiceRowGroup_rowAgreementIdentifierUrlText = Nothing -- :: Maybe GenericStringType0_512
              , invoiceRowGroup_rowRequestOfQuotationIdentifier = Nothing -- :: Maybe GenericStringType0_35
              , invoiceRowGroup_rowRequestOfQuotationIdentifierUrlText = Nothing -- :: Maybe GenericStringType0_512
              , invoiceRowGroup_rowPriceListIdentifier = Nothing -- :: Maybe GenericStringType0_35
              , invoiceRowGroup_rowPriceListIdentifierUrlText = Nothing -- :: Maybe GenericStringType0_512
              , invoiceRowGroup_rowBuyerReferenceIdentifier = Nothing -- :: Maybe GenericStringType0_70
              , invoiceRowGroup_rowProjectReferenceIdentifier = Nothing -- :: Maybe GenericStringType0_70
              , invoiceRowGroup_rowOverDuePaymentDetails = Nothing -- :: Maybe RowOverDuePaymentDetailsType
              , invoiceRowGroup_rowAnyPartyDetails = [] -- [rowAnyPartyDetailsType] -- :: [RowAnyPartyDetailsType]
              , invoiceRowGroup_rowDeliveryDetails = Nothing -- :: Maybe RowDeliveryDetailsType
              , invoiceRowGroup_rowShortProposedAccountIdentifier = Nothing -- :: Maybe GenericNMtokenType0_4
              , invoiceRowGroup_rowNormalProposedAccountIdentifier = Nothing -- :: Maybe GenericNMtokenType0_4
              , invoiceRowGroup_rowProposedAccountText = Nothing -- :: Maybe GenericStringType0_35
              , invoiceRowGroup_rowAccountDimensionText = Nothing -- :: Maybe GenericStringType0_35
              , invoiceRowGroup_rowSellerAccountText = Nothing -- :: Maybe GenericStringType0_35
              , invoiceRowGroup_rowFreeText = [] -- [GenericStringType0_512 $ Xs.XsdString $ "free text in a row"] -- :: [GenericStringType0_512]
              , invoiceRowGroup_rowUsedQuantity = Nothing -- :: Maybe QuantityType0_14
              , invoiceRowGroup_rowPreviousMeterReadingDate = Nothing -- :: Maybe Date
              , invoiceRowGroup_rowLatestMeterReadingDate = Nothing -- :: Maybe Date
              , invoiceRowGroup_rowCalculatedQuantity = Nothing -- :: Maybe QuantityType0_14
              , invoiceRowGroup_rowAveragePriceAmount = Nothing -- :: Maybe Amount
              , invoiceRowGroup_rowDiscountPercent = Nothing -- :: Maybe Percentage
              , invoiceRowGroup_rowDiscountAmount = Nothing -- :: Maybe Amount
              , invoiceRowGroup_rowDiscountBaseAmount = Nothing -- :: Maybe Amount
              , invoiceRowGroup_rowDiscountTypeCode = Nothing -- :: Maybe Untdid5189
              , invoiceRowGroup_rowDiscountTypeText = Nothing -- :: Maybe GenericStringType0_35
              , invoiceRowGroup_rowProgressiveDiscountDetails = [] -- [rowProgressiveDiscountDetailsType] -- :: [RowProgressiveDiscountDetailsType]
              , invoiceRowGroup_rowChargeDetails = [] -- [rowChargeDetailsType] -- :: [RowChargeDetailsType]
              , invoiceRowGroup_rowVatRatePercent = Nothing -- :: Maybe Percentage
              , invoiceRowGroup_rowVatCode = Nothing -- :: Maybe Untdid5305
              , invoiceRowGroup_rowVatAmount = Nothing -- :: Maybe Amount
              , invoiceRowGroup_rowVatExcludedAmount = Nothing -- :: Maybe Amount
              , invoiceRowGroup_rowAmount = Nothing -- :: Maybe Amount
              , invoiceRowGroup_rowTransactionDetails = Nothing -- :: Maybe TransactionDetailsType
              }


{-   articleGroupIdentifierType = ArticleGroupIdentifierType (GenericStringType0_35 $ Xs.XsdString "ArticleGroupIdentifier") articleGroupIdentifierTypeAttributes
  articleGroupIdentifierTypeAttributes = ArticleGroupIdentifierTypeAttributes
      { articleGroupIdentifierTypeAttributes_schemeID = Nothing -- :: Maybe Untdid7143
      , articleGroupIdentifierTypeAttributes_schemeVersion = Nothing -- :: Maybe GenericStringType1_35
      }
 -}

      invoiceSubInvoiceRow = SubInvoiceRowType
        { subInvoiceRowType_subIdentifier = Nothing -- Maybe GenericStringType0_35
        , subInvoiceRowType_subRowPositionIdentifier = Nothing -- GenericStringType0_35
        , subInvoiceRowType_subInvoicedObjectID = Nothing -- InvoicedObjectIDType
        , subInvoiceRowType_subArticleIdentifier = Nothing -- GenericStringType0_70


        , subInvoiceRowType_subArticleGroupIdentifier = []
        , subInvoiceRowType_subArticleName = Nothing -- GenericStringType0_100
        , subInvoiceRowType_subArticleDescription = Nothing -- GenericStringType0_512
        , subInvoiceRowType_subArticleInfoUrlText = Nothing -- GenericStringType0_512
        , subInvoiceRowType_subBuyerArticleIdentifier = Nothing -- GenericStringType0_70
        , subInvoiceRowType_subEanCode = Nothing -- EanCodeType
        , subInvoiceRowType_subRowRegistrationNumberIdentifier = Nothing -- GenericStringType0_70
        , subInvoiceRowType_subSerialNumberIdentifier = Nothing -- GenericStringType0_70
        , subInvoiceRowType_subRowActionCode = Nothing -- GenericTokenType0_35
        , subInvoiceRowType_subRowDefinitionDetails = [] -- = [] --SubRowDefinitionDetails]
        , subInvoiceRowType_subOfferedQuantity = [] --QuantityType0_14]
        , subInvoiceRowType_subDeliveredQuantity = [] --QuantityType0_14]
        , subInvoiceRowType_subOrderedQuantity = Nothing -- QuantityType0_14
        , subInvoiceRowType_subConfirmedQuantity = Nothing -- QuantityType0_14
        , subInvoiceRowType_subPostDeliveredQuantity = Nothing -- QuantityType0_14
        , subInvoiceRowType_subInvoicedQuantity = [] --QuantityType0_14]
        , subInvoiceRowType_subCreditRequestedQuantity = Nothing -- QuantityType0_14
        , subInvoiceRowType_subReturnedQuantity = Nothing -- QuantityType0_14
        , subInvoiceRowType_subStartDate = Nothing -- Date
        , subInvoiceRowType_subEndDate = Nothing -- Date
        , subInvoiceRowType_subUnitPriceAmount = Nothing -- UnitAmount
        , subInvoiceRowType_subUnitPriceDiscountAmount = Nothing -- UnitAmount
        , subInvoiceRowType_subUnitPriceNetAmount = Nothing -- UnitAmount
        , subInvoiceRowType_subUnitPriceVatIncludedAmount = Nothing -- UnitAmount
        , subInvoiceRowType_subUnitPriceBaseQuantity = Nothing -- QuantityType0_14
        , subInvoiceRowType_subRowIdentifier = Nothing -- GenericStringType0_35
        , subInvoiceRowType_subRowIdentifierUrlText = Nothing -- GenericStringType0_512
        , subInvoiceRowType_subRowIdentifierDate = Nothing -- Date
        , subInvoiceRowType_subRowOrdererName = Nothing -- GenericStringType0_35
        , subInvoiceRowType_subRowSalesPersonName = Nothing -- GenericStringType0_35
        , subInvoiceRowType_subRowOrderConfirmationIdentifier = Nothing -- GenericStringType0_70
        , subInvoiceRowType_subRowOrderConfirmationDate = Nothing -- Date
        , subInvoiceRowType_subOriginalInvoiceNumber = Nothing -- GenericStringType1_20
        , subInvoiceRowType_subOriginalInvoiceDate = Nothing -- Date
        , subInvoiceRowType_subOriginalInvoiceReference = [] --OriginalInvoiceReferenceType]
        , subInvoiceRowType_subRowDeliveryIdentifier = Nothing -- GenericStringType0_35
        , subInvoiceRowType_subRowDeliveryIdentifierUrlText = Nothing -- GenericStringType0_512
        , subInvoiceRowType_subRowDeliveryDate = Nothing -- Date
        , subInvoiceRowType_subRowQuotationIdentifier = Nothing -- GenericStringType0_35
        , subInvoiceRowType_subRowQuotationIdentifierUrlText = Nothing -- GenericStringType0_512
        , subInvoiceRowType_subRowAgreementIdentifier = Nothing -- GenericStringType0_70
        , subInvoiceRowType_subRowAgreementIdentifierUrlText = Nothing -- GenericStringType0_512
        , subInvoiceRowType_subRowRequestOfQuotationIdentifier = Nothing -- GenericStringType0_35
        , subInvoiceRowType_subRowRequestOfQuotationIdentifierUrlText = Nothing -- GenericStringType0_512
        , subInvoiceRowType_subRowPriceListIdentifier = Nothing -- GenericStringType0_35
        , subInvoiceRowType_subRowPriceListIdentifierUrlText = Nothing -- GenericStringType0_512
        , subInvoiceRowType_subRowBuyerReferenceIdentifier = Nothing -- GenericStringType0_70
        , subInvoiceRowType_subRowProjectReferenceIdentifier = Nothing -- GenericStringType0_70
        , subInvoiceRowType_subRowOverDuePaymentDetails = Nothing -- SubRowOverDuePaymentDetails
        , subInvoiceRowType_subRowAnyPartyDetails = [] --SubRowAnyPartyDetails]
        , subInvoiceRowType_subRowDeliveryDetails = Nothing -- SubRowDeliveryDetailsType
        , subInvoiceRowType_subRowShortProposedAccountIdentifier = Nothing -- GenericStringType0_4
        , subInvoiceRowType_subRowNormalProposedAccountIdentifier = Nothing -- GenericStringType0_4
        , subInvoiceRowType_subRowProposedAccountText = Nothing -- GenericStringType0_35
        , subInvoiceRowType_subRowAccountDimensionText = Nothing -- GenericStringType0_35
        , subInvoiceRowType_subRowSellerAccountText = Nothing -- GenericStringType0_35
        , subInvoiceRowType_subRowFreeText = [] --GenericStringType0_512]
        , subInvoiceRowType_subRowUsedQuantity = Nothing -- QuantityType0_14
        , subInvoiceRowType_subRowPreviousMeterReadingDate = Nothing -- Date
        , subInvoiceRowType_subRowLatestMeterReadingDate = Nothing -- Date
        , subInvoiceRowType_subRowCalculatedQuantity = Nothing -- QuantityType0_14
        , subInvoiceRowType_subRowAveragePriceAmount = Nothing -- Amount
        , subInvoiceRowType_subRowDiscountPercent = Nothing -- Percentage
        , subInvoiceRowType_subRowDiscountAmount = Nothing -- Amount
        , subInvoiceRowType_subRowDiscountBaseAmount = Nothing -- Amount
        , subInvoiceRowType_subRowDiscountTypeCode = Nothing -- Untdid5189
        , subInvoiceRowType_subRowDiscountTypeText = Nothing -- GenericStringType0_35
        , subInvoiceRowType_subRowProgressiveDiscountDetails = [] --SubRowProgressiveDiscountDetails]
        , subInvoiceRowType_subRowChargeDetails = [] --RowChargeDetailsType]
        , subInvoiceRowType_subRowVatRatePercent = Nothing -- Percentage
        , subInvoiceRowType_subRowVatCode = Nothing -- Untdid5305
        , subInvoiceRowType_subRowVatAmount = Nothing -- Amount
        , subInvoiceRowType_subRowVatExcludedAmount = Nothing -- Amount
        , subInvoiceRowType_subRowAmount = Nothing -- Amount
        , subInvoiceRowType_subRowTransactionDetails = Nothing -- TransactionDetailsType
        }






      epiBfiPartyDetailsType = EpiBfiPartyDetailsType
              { epiBfiPartyDetailsType_epiBfiIdentifier = Nothing -- :: Maybe EpiBfiIdentifierType
              , epiBfiPartyDetailsType_epiBfiName = Nothing -- :: Maybe GenericStringType1_35
              }

      epiAccountIDType = EpiAccountIDType (GenericNMtokenType1_34 $ Xs.Token "epiAccountIDType") epiAccountIDTypeAttributes
      epiAccountIDTypeAttributes = EpiAccountIDTypeAttributes
          { epiAccountIDTypeAttributes_identificationSchemeName =  Xs.XsdString "identificationSchemeName" -- :: Xs.XsdString
          }

      epiBeneficiaryPartyDetailsType = EpiBeneficiaryPartyDetailsType
              { epiBeneficiaryPartyDetailsType_epiNameAddressDetails = Nothing -- :: Maybe GenericTokenType2_35
              , epiBeneficiaryPartyDetailsType_epiBei = Nothing -- :: Maybe GenericNMtokenType8_11
              , epiBeneficiaryPartyDetailsType_epiAccountID = epiAccountIDType -- :: EpiAccountIDType
              }


      epiPartyDetailsType = EpiPartyDetailsType
              { epiPartyDetailsType_epiBfiPartyDetails = epiBfiPartyDetailsType -- :: EpiBfiPartyDetailsType
              , epiPartyDetailsType_epiBeneficiaryPartyDetails = epiBeneficiaryPartyDetailsType -- :: EpiBeneficiaryPartyDetailsType
              }

      epiChargeType = EpiChargeType (Xs.Token "") epiChargeTypeAttributes
      epiChargeTypeAttributes = EpiChargeTypeAttributes
          { epiChargeTypeAttributes_chargeOption = Xs.XsdString "SLEV" -- :: Xs.XsdString   
          }

      epiMonetaryAmount = EpiMonetaryAmount $ Xs.Token "0" -- :: Xs.Token

      epiAmount = EpiAmount epiMonetaryAmount epiAmountAttributes
      epiAmountAttributes = EpiAmountAttributes
          { epiAmountAttributes_amountCurrencyIdentifier = Xs.XsdString "EUR" -- :: XsdString
          }

      epiRemittanceInfoIdentifierPattern = EpiRemittanceInfoIdentifierPattern $ Xs.NMTOKEN "" -- Xs.NMTOKEN deriving (Eq,Show)

      epiRemittanceInfoIdentifierType = EpiRemittanceInfoIdentifierType epiRemittanceInfoIdentifierPattern epiRemittanceInfoIdentifierTypeAttributes 
      epiRemittanceInfoIdentifierTypeAttributes = EpiRemittanceInfoIdentifierTypeAttributes
          { epiRemittanceInfoIdentifierTypeAttributes_identificationSchemeName = Just $ Xs.XsdString "SPY" -- :: Maybe Xs.XsdString
          }

      epiPaymentInstructionDetailsType = EpiPaymentInstructionDetailsType
              { epiPaymentInstructionDetailsType_epiPaymentInstructionId = Nothing -- :: Maybe GenericStringType0_35
              , epiPaymentInstructionDetailsType_epiTransactionTypeCode = Nothing -- :: Maybe GenericTokenType3
              , epiPaymentInstructionDetailsType_epiInstructionCode = Nothing -- :: Maybe GenericNMtokenType0_35
              , epiPaymentInstructionDetailsType_epiRemittanceInfoIdentifier = Just epiRemittanceInfoIdentifierType -- :: Maybe EpiRemittanceInfoIdentifierType
              , epiPaymentInstructionDetailsType_epiInstructedAmount = epiAmount -- :: EpiAmount
              , epiPaymentInstructionDetailsType_epiCharge = epiChargeType -- :: EpiChargeType 
              , epiPaymentInstructionDetailsType_epiDateOptionDate = dueDate -- :: Date
              , epiPaymentInstructionDetailsType_epiPaymentMeansCode = Nothing -- :: Maybe Untdid4461
              , epiPaymentInstructionDetailsType_epiPaymentMeansText = Nothing -- :: Maybe GenericStringType1_70
              }

      epiDetailsType = EpiDetailsType
              { epiDetailsType_epiIdentificationDetails = epiIdentificationDetailsType -- :: epiIdentificationDetailsType
              , epiDetailsType_epiPartyDetails = epiPartyDetailsType -- :: epiPartyDetailsType
              , epiDetailsType_epiPaymentInstructionDetails = epiPaymentInstructionDetailsType -- :: epiPaymentInstructionDetailsType
              }

      epiIdentificationDetailsType = EpiIdentificationDetailsType
              { epiIdentificationDetailsType_epiDate = dueDate -- :: Date
              , epiIdentificationDetailsType_epiReference = GenericNMtokenType0_35 $ Xs.Token "epiReference" -- :: GenericNMtokenType0_35
              , epiIdentificationDetailsType_epiUrl = Nothing -- :: Maybe GenericNMtokenType0_512
              , epiIdentificationDetailsType_epiEmail = Nothing  -- :: Maybe GenericStringType0_70
              , epiIdentificationDetailsType_epiOrderInfo = [] -- [GenericTokenType0_70 $ Xs.Token "TOK"] -- :: [GenericNMtokenType0_35]
              }

      electronicAddrIdType1 = ElectronicAddrIdType (GenericStringType2_35 $ Xs.XsdString "003724385902") electronicAddrIdTypeAttributes1
      electronicAddrIdTypeAttributes1 = ElectronicAddrIdTypeAttributes
          { electronicAddrIdTypeAttributes_schemeID = Nothing -- Maybe ElectronicAddrSchemeIdType
          }

      electronicAddrIdType2 = ElectronicAddrIdType (GenericStringType2_35 $ Xs.XsdString "003703498392") electronicAddrIdTypeAttributes2

      electronicAddrIdTypeAttributes2 = ElectronicAddrIdTypeAttributes
          { electronicAddrIdTypeAttributes_schemeID = Nothing -- Maybe ElectronicAddrSchemeIdType
          }

      messageSenderDetails = MessageSenderDetails
              { messageSenderDetails_fromIdentifier = electronicAddrIdType1 -- :: ElectronicAddrIdType
              , messageSenderDetails_fromIntermediator = GenericStringType2_35 $ Xs.XsdString "003721291126" -- :: GenericStringType2_35
              }
      messageReceiverDetails = MessageReceiverDetails
              { messageReceiverDetails_toIdentifier = electronicAddrIdType2 -- :: ElectronicAddrIdType
              , messageReceiverDetails_toIntermediator = GenericStringType2_35 $ Xs.XsdString "003721291126" -- :: GenericStringType2_35 
              }
      messageDetails = MessageDetails
              { messageDetails_messageIdentifier = GenericStringType2_48 $ Xs.XsdString "midentifier" -- :: GenericStringType2_48
              , messageDetails_messageTimeStamp = Xs.DateTime $ "2019-08-24T14:15:22Z" -- :: Xs.DateTime
              , messageDetails_refToMessageIdentifier = Nothing -- :: Maybe GenericStringType0_48
              , messageDetails_implementationCode = Nothing -- :: Maybe GenericStringType0_4
              , messageDetails_specificationIdentifier = Nothing -- :: Maybe GenericStringType1_35
              }
    --messageTransmissionDetailsType :: MessageTransmissionDetailsType
      messageTransmissionDetailsType = MessageTransmissionDetailsType
          { messageTransmissionDetailsType_messageSenderDetails = messageSenderDetails -- :: MessageSenderDetails
          , messageTransmissionDetailsType_messageReceiverDetails = messageReceiverDetails -- :: MessageReceiverDetails
          , messageTransmissionDetailsType_messageDetails = messageDetails -- :: MessageDetails
          }

 
      finvoice = Finvoice { finvoice_version = Xs.XsdString "3.0"
          , finvoice_messageTransmissionDetails = Just messageTransmissionDetailsType -- Nothing -- :: Maybe MessageTransmissionDetailsType
          , finvoice_sellerPartyDetails = sellerPartyDetailsType -- :: SellerPartyDetailsType
          , finvoice_sellerOrganisationUnitNumber = Nothing -- :: Maybe GenericStringType0_35
          , finvoice_sellerSiteCode = Nothing -- :: Maybe GenericStringType0_35
          , finvoice_sellerContactPersonName = Nothing -- :: Maybe GenericStringType0_70
          , finvoice_sellerContactPersonFunction =[] -- [GenericStringType0_35 $ Xs.XsdString "person function"]
          , finvoice_sellerContactPersonDepartment =[] -- [GenericStringType0_35 $ Xs.XsdString "person department"]
          , finvoice_sellerCommunicationDetails = Nothing -- :: SellerCommunicationDetailsType
          , finvoice_sellerInformationDetails = Nothing -- :: SellerInformationDetailsType

          , finvoice_invoiceSenderPartyDetails = Nothing -- :: Maybe InvoiceSenderPartyDetailsType
          , finvoice_invoiceRecipientPartyDetails = Nothing -- :: Maybe InvoiceRecipientPartyDetailsType
          , finvoice_invoiceRecipientOrganisationUnitNumber = Nothing -- :: Maybe GenericStringType0_35
          , finvoice_invoiceRecipientSiteCode = Nothing -- :: Maybe GenericStringType0_35
          , finvoice_invoiceRecipientContactPersonName = Nothing -- :: Maybe GenericStringType0_70
          , finvoice_invoiceRecipientContactPersonFunction =[] --  [GenericStringType0_35 $ Xs.XsdString "recipient conctact function"]
          , finvoice_invoiceRecipientContactPersonDepartment =[] --  [GenericStringType0_35 $ Xs.XsdString "recipient contact department" ]
          , finvoice_invoiceRecipientLanguageCode = Nothing
          , finvoice_invoiceRecipientCommunicationDetails = Nothing

          , finvoice_buyerPartyDetails = buyerPartyDetailsType -- :: BuyerPartyDetailsType
          , finvoice_buyerOrganisationUnitNumber = Nothing -- :: Maybe GenericStringType0_35
          , finvoice_buyerSiteCode = Nothing -- :: Maybe GenericStringType0_35
          , finvoice_buyerContactPersonName = Nothing -- :: Maybe GenericStringType0_70
          , finvoice_buyerContactPersonFunction = [] -- [GenericStringType0_35 $ Xs.XsdString "buyer conctact function"]
          , finvoice_buyerContactPersonDepartment = [] -- [GenericStringType0_35 $ Xs.XsdString "uyer conctact department"]
          , finvoice_buyerCommunicationDetails = Nothing -- :: Maybe BuyerCommunicationDetailsType

          , finvoice_deliveryPartyDetails = Nothing -- :: Maybe DeliveryPartyDetailsType
          , finvoice_deliveryOrganisationUnitNumber = Nothing -- :: Maybe GenericStringType0_35
          , finvoice_deliverySiteCode = Nothing -- :: Maybe GenericStringType0_35
          , finvoice_deliveryContactPersonName = Nothing -- :: Maybe GenericStringType0_70


          , finvoice_deliveryContactPersonFunction = [] -- [GenericStringType0_35 $ Xs.XsdString "y conctact function"]
          , finvoice_deliveryContactPersonDepartment = [] -- [GenericStringType0_35 $ Xs.XsdString "y conctact department"]

          , finvoice_deliveryCommunicationDetails = Nothing -- :: Maybe DeliveryCommunicationDetailsType
          , finvoice_deliveryDetails = Nothing -- :: Maybe DeliveryDetailsType

          , finvoice_anyPartyDetails = [] -- [anyPartyDetailsType]
          , finvoice_invoiceDetails = invoiceDetailsType

          , finvoice_paymentCardInfo = Nothing -- :: Maybe PaymentCardInfoType
          , finvoice_directDebitInfo = Nothing -- :: Maybe DirectDebitInfoType
          , finvoice_paymentStatusDetails = Nothing -- :: Maybe PaymentStatusDetailsType

          , finvoice_partialPaymentDetails = [] -- [partialPaymentDetailsType]

          , finvoice_factoringAgreementDetails = Nothing -- :: Maybe FactoringAgreementDetailsType
          , finvoice_virtualBankBarcode = Nothing --  :: Maybe GenericStringType0_512

          , finvoice_invoiceRow =  [invoiceRowType]  --invoiceRowType
          , finvoice_specificationDetails = Nothing -- :: Maybe SpecificationDetailsType
          , finvoice_epiDetails = epiDetailsType
          , finvoice_invoiceUrlNameText = [] -- [GenericStringType0_512 $ Xs.XsdString "name text"]
          , finvoice_invoiceUrlText = [] -- [GenericStringType0_512 $ Xs.XsdString "url text"]

          , finvoice_storageUrlText = Nothing -- :: Maybe GenericStringType0_512
          , finvoice_layOutIdentifier = Nothing -- :: Maybe GenericStringType0_35
          , finvoice_invoiceSegmentIdentifier = Nothing -- :: Maybe GenericStringType0_35
          , finvoice_controlChecksum = Nothing -- :: Maybe GenericStringType0_512
          , finvoice_messageChecksum = Nothing -- :: Maybe GenericStringType0_512
          , finvoice_controlStampText = Nothing -- :: Maybe GenericStringType0_512
          , finvoice_acceptanceStampText = Nothing -- :: Maybe GenericStringType0_512
          , finvoice_originalInvoiceFormat = Nothing -- :: Maybe GenericStringType0_35
          , finvoice_attachmentMessageDetails = Nothing -- :: Maybe AttachmentMessageDetailsType 
        } 
 
      in finvoice

dotToComma :: String->String
dotToComma x = unpack $ Data.Text.replace "." "," $ pack x

dateFinvoice::Day->Date
dateFinvoice date = do  
  let dateInt = read(unpack $ Data.Text.replace "-" "" $ pack $ showGregorian date)::Integer
  Date (DateType dateInt) (DateAttributes {dateAttributes_format=Xs.XsdString "CCYYMMDD"})

amountEuro::Double->Amount 
amountEuro x = do 
    let monetaryAmount = MonetaryAmount $ Xs.Token $ dotToComma $ show x 
    let amountAttributes = AmountAttributes
          { amountAttributes_amountCurrencyIdentifier =  Xs.XsdString "EUR"  -- :: Xs.XsdString
          }
    Amount monetaryAmount amountAttributes


amountUnit :: Double->UnitAmountUN
amountUnit x =do
    let unitAmountAttributes = UnitAmountAttributes
          { unitAmountAttributes_amountCurrencyIdentifier = Xs.XsdString "EUR"
          , unitAmountAttributes_unitPriceUnitCode = Just $ GenericStringType0_14 $ Xs.XsdString "kpl" -- :: Maybe GenericStringType0_14
          }
    let unitAmount = UnitAmount (UnitAmountType $ Xs.Token $ dotToComma $ show x) unitAmountAttributes 
    let unitCodeUN = UnitCodeUN $ Xs.NMTOKEN "PCS"  
    let unitAmountUNAttributes = UnitAmountUNAttributes
          { unitAmountUNAttributes_quantityUnitCodeUN = Just unitCodeUN -- :: Maybe UnitCodeUN
          }
    UnitAmountUN unitAmount unitAmountUNAttributes

mkEpiRemittanceInfoIdentifier::String->EpiRemittanceInfoIdentifierType
mkEpiRemittanceInfoIdentifier x= do
  let epiRemittanceInfoIdentifierPattern = EpiRemittanceInfoIdentifierPattern $ Xs.NMTOKEN x -- Xs.NMTOKEN deriving (Eq,Show)
      epiRemittanceInfoIdentifierTypeAttributes = EpiRemittanceInfoIdentifierTypeAttributes
          { epiRemittanceInfoIdentifierTypeAttributes_identificationSchemeName = Just $ Xs.XsdString "SPY" -- :: Maybe Xs.XsdString
          }
    in EpiRemittanceInfoIdentifierType epiRemittanceInfoIdentifierPattern epiRemittanceInfoIdentifierTypeAttributes 
      

invoice :: SalesInvoiceId -> Handler Finvoice
invoice salesInvoiceId = do

  invoice <- runDB $ getJust salesInvoiceId
  invoiceRows <- runDB $ selectList [SalesInvoiceDetailSalesInvoiceId ==. salesInvoiceId] []

  let finvoice = buildInvoice
  print "OK1"    
  let date = salesInvoiceDate invoice
  dueDate <- case salesInvoiceDuedate invoice of
    Just x -> return x
    Nothing ->  sendResponseStatus status404 ("Invoice is missing due date!"::Text)

  invoiceRows' <- mapM (\(Entity id row)->do
    productRecord <- runDB $ getJust (salesInvoiceDetailProductId row) :: Handler Product
    
    let articleName =Just $ GenericStringType0_100 $ Xs.XsdString (unpack $ productName productRecord)
    let articleDescription = case (productDescription productRecord) of
          Just x -> Just $ GenericStringType0_512 $ Xs.XsdString (unpack x)
          Nothing -> Nothing
    let invoicedQuantity= QuantityType0_14 $ Xs.XsdString $ dotToComma $ show $ salesInvoiceDetailQuantity row -- show $ salesInvoiceDetailQuantity value

    let referenceRow =(Data.List.head $ finvoice.finvoice_invoiceRow).invoiceRowType_choice0
    let referenceRow2 =(Data.List.head $ finvoice.finvoice_invoiceRow).invoiceRowType_choice0
  

    productRecord <- runDB $ getJust (salesInvoiceDetailProductId row)
    let unitPrice = productPrice productRecord
    let quantity = salesInvoiceDetailQuantity row

    vatPctRecord <- runDB $ getJust (salesInvoiceDetailVatPctId row)
    let vatPercentage = round'((vatPctPct vatPctRecord)/100)
    print $ "vatPercentage"++(show vatPercentage)
    let unitPriceAmount = amountUnit unitPrice
    let unitPriceNetAmount = amountUnit unitPrice
    let unitPriceVatIncludedAmount = amountUnit ((1+vatPercentage)*unitPrice)

    let rowAmount = amountEuro (round'(quantity*unitPrice*(1+vatPercentage)))  
    let rowVatAmount = amountEuro (round'(quantity*unitPrice*(1+vatPercentage)-quantity*unitPrice))  

    newRow <- case referenceRow of 
      OneOf2 x -> do
                      let r =  x  { invoiceRowGroup_articleName = articleName
                              , invoiceRowGroup_articleDescription = articleDescription
                              , invoiceRowGroup_invoicedQuantity = [invoicedQuantity]
                              , invoiceRowGroup_unitPriceAmount = Just unitPriceAmount
                              , invoiceRowGroup_unitPriceNetAmount = Just unitPriceNetAmount -- :: Maybe UnitAmountUN
                              , invoiceRowGroup_unitPriceVatIncludedAmount = Just unitPriceVatIncludedAmount --Nothing -- :: Maybe UnitAmountUN
                              , invoiceRowGroup_rowVatAmount = Just rowVatAmount
                              , invoiceRowGroup_rowAmount =Just rowAmount                                                   

                             }
                      return $ InvoiceRowType { invoiceRowType_choice0 = OneOf2 r}
      TwoOf2 x -> return $ InvoiceRowType { invoiceRowType_choice0 = TwoOf2 x} 
                          
   --   _ -> sendResponseStatus status404 ("Only InvoiceRowGroup structure allowed for invoices!"::Text)
    return newRow
    ) invoiceRows        
  

  -- | Buyer and seller information
  seller <- runDB $ getJust (salesInvoiceCompanyId invoice) :: Handler Company
  let sellerOrganisationName = [GenericStringType2_70 $ Xs.XsdString $ unpack $ companyName seller]
  buyerId <- case (salesInvoicePartnerId invoice) of
    Just x ->print x >> return  x
    Nothing -> sendResponseStatus status404 ("Buyer is not set for the invoice!"::Text)
  buyer <- runDB $ getJust buyerId :: Handler Partner
  let buyerOrganisationName = [GenericStringType2_70 $ Xs.XsdString $ unpack $ partnerName buyer]
  let partyLegalRegIdTypeAttributes = PartyLegalRegIdTypeAttributes
        { partyLegalRegIdTypeAttributes_schemeID = Nothing -- :: Maybe Iso6523cid
        }
  
  
  let buyer' = GenericStringType0_35 $ Xs.XsdString $ unpack $ fromMaybe "" (partnerCompanyid buyer)
  let seller' = GenericStringType0_35 $ Xs.XsdString $ unpack $ companyCompanyid seller

  let buyerPartyIdentifier = PartyLegalRegIdType buyer' partyLegalRegIdTypeAttributes 
  let sellerPartyIdentifier = PartyLegalRegIdType seller' partyLegalRegIdTypeAttributes 

  let sellerOrganisationTaxCode = GenericStringType0_35 $ Xs.XsdString $ "FI" ++ (unpack $ Data.Text.replace "-" "" (companyCompanyid seller))


  -- | Invoice totals
  let invoiceTotalVatExcludedAmount = amountEuro $ fromMaybe 0 (salesInvoiceNettotal invoice) -- :: Maybe Amount
  let invoiceTotalVatAmount = amountEuro $ fromMaybe 0 (salesInvoiceVattotal invoice) -- :: Maybe Amount
  let invoiceTotalVatIncludedAmount = amountEuro $ fromMaybe 0 (salesInvoiceTotal invoice) -- :: Amount


  -- | EPI details
  let epiRemittanceInfoIdentifier = let refnumber = unpack $ fromMaybe "" (salesInvoiceReferencenumber invoice)
                                      in mkEpiRemittanceInfoIdentifier refnumber

  let epiInstructedAmount = let epiMonetaryAmount = EpiMonetaryAmount $ Xs.Token $ dotToComma $ show $ fromMaybe 0 (salesInvoiceTotal invoice) -- :: Xs.Token
                                epiAmountAttributes = EpiAmountAttributes
                                  { epiAmountAttributes_amountCurrencyIdentifier = Xs.XsdString "EUR" -- :: XsdString
                                  }
                              in EpiAmount epiMonetaryAmount epiAmountAttributes

  return finvoice{
      finvoice_invoiceRow=invoiceRows' 

    , finvoice_epiDetails.epiDetailsType_epiIdentificationDetails.epiIdentificationDetailsType_epiDate=dateFinvoice dueDate
    , finvoice_epiDetails.epiDetailsType_epiPaymentInstructionDetails.epiPaymentInstructionDetailsType_epiInstructedAmount=epiInstructedAmount
    , finvoice_epiDetails.epiDetailsType_epiPaymentInstructionDetails.epiPaymentInstructionDetailsType_epiDateOptionDate=dateFinvoice dueDate
    , finvoice_epiDetails.epiDetailsType_epiPaymentInstructionDetails.epiPaymentInstructionDetailsType_epiRemittanceInfoIdentifier=Just $ epiRemittanceInfoIdentifier 
    
    , finvoice_buyerPartyDetails.buyerPartyDetailsType_buyerOrganisationName = buyerOrganisationName
    , finvoice_buyerPartyDetails.buyerPartyDetailsType_buyerPartyIdentifier = Just buyerPartyIdentifier
    
    , finvoice_sellerPartyDetails.sellerPartyDetailsType_sellerOrganisationName =sellerOrganisationName
    , finvoice_sellerPartyDetails.sellerPartyDetailsType_sellerPartyIdentifier = Just sellerPartyIdentifier
    , finvoice_sellerPartyDetails.sellerPartyDetailsType_sellerOrganisationTaxCode = Just sellerOrganisationTaxCode 

    , finvoice_invoiceDetails.invoiceDetailsType_invoiceTotalVatExcludedAmount = Just invoiceTotalVatExcludedAmount -- :: Maybe Amount
    , finvoice_invoiceDetails.invoiceDetailsType_invoiceTotalVatAmount =Just invoiceTotalVatAmount -- :: Maybe Amount
    , finvoice_invoiceDetails.invoiceDetailsType_invoiceTotalVatIncludedAmount = invoiceTotalVatIncludedAmount -- :: Amount
    , finvoice_invoiceDetails.invoiceDetailsType_invoiceDate = dateFinvoice date

    }
                         
 
   