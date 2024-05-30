{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module V'home'azureuser'hookkeeperHaskell'src'External'Finvoice'xsd'Finvoice3'0'xsd
  ( module V'home'azureuser'hookkeeperHaskell'src'External'Finvoice'xsd'Finvoice3'0'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
data Finvoice = Finvoice
        { finvoice_version :: Xsd.XsdString
        , finvoice_messageTransmissionDetails :: Maybe MessageTransmissionDetailsType
        , finvoice_sellerPartyDetails :: SellerPartyDetailsType
        , finvoice_sellerOrganisationUnitNumber :: Maybe GenericStringType0_35
        , finvoice_sellerSiteCode :: Maybe GenericStringType0_35
        , finvoice_sellerContactPersonName :: Maybe GenericStringType0_70
        , finvoice_sellerContactPersonFunction :: [GenericStringType0_35]
        , finvoice_sellerContactPersonDepartment :: [GenericStringType0_35]
        , finvoice_sellerCommunicationDetails :: Maybe SellerCommunicationDetailsType
        , finvoice_sellerInformationDetails :: Maybe SellerInformationDetailsType
        , finvoice_invoiceSenderPartyDetails :: Maybe InvoiceSenderPartyDetailsType
        , finvoice_invoiceRecipientPartyDetails :: Maybe InvoiceRecipientPartyDetailsType
        , finvoice_invoiceRecipientOrganisationUnitNumber :: Maybe GenericStringType0_35
        , finvoice_invoiceRecipientSiteCode :: Maybe GenericStringType0_35
        , finvoice_invoiceRecipientContactPersonName :: Maybe GenericStringType0_70
        , finvoice_invoiceRecipientContactPersonFunction :: [GenericStringType0_35]
        , finvoice_invoiceRecipientContactPersonDepartment :: [GenericStringType0_35]
        , finvoice_invoiceRecipientLanguageCode :: Maybe LanguageCodeType
        , finvoice_invoiceRecipientCommunicationDetails :: Maybe InvoiceRecipientCommunicationDetailsType
        , finvoice_buyerPartyDetails :: BuyerPartyDetailsType
        , finvoice_buyerOrganisationUnitNumber :: Maybe GenericStringType0_35
        , finvoice_buyerSiteCode :: Maybe GenericStringType0_35
        , finvoice_buyerContactPersonName :: Maybe GenericStringType0_70
        , finvoice_buyerContactPersonFunction :: [GenericStringType0_35]
        , finvoice_buyerContactPersonDepartment :: [GenericStringType0_35]
        , finvoice_buyerCommunicationDetails :: Maybe BuyerCommunicationDetailsType
        , finvoice_deliveryPartyDetails :: Maybe DeliveryPartyDetailsType
        , finvoice_deliveryOrganisationUnitNumber :: Maybe GenericStringType0_35
        , finvoice_deliverySiteCode :: Maybe GenericStringType0_35
        , finvoice_deliveryContactPersonName :: Maybe GenericStringType0_70
        , finvoice_deliveryContactPersonFunction :: [GenericStringType0_35]
        , finvoice_deliveryContactPersonDepartment :: [GenericStringType0_35]
        , finvoice_deliveryCommunicationDetails :: Maybe DeliveryCommunicationDetailsType
        , finvoice_deliveryDetails :: Maybe DeliveryDetailsType
        , finvoice_anyPartyDetails :: [AnyPartyDetailsType]
        , finvoice_invoiceDetails :: InvoiceDetailsType
        , finvoice_paymentCardInfo :: Maybe PaymentCardInfoType
        , finvoice_directDebitInfo :: Maybe DirectDebitInfoType
        , finvoice_paymentStatusDetails :: Maybe PaymentStatusDetailsType
        , finvoice_partialPaymentDetails :: [PartialPaymentDetailsType]
        , finvoice_factoringAgreementDetails :: Maybe FactoringAgreementDetailsType
        , finvoice_virtualBankBarcode :: Maybe GenericStringType0_512
        , finvoice_invoiceRow :: [InvoiceRowType]
        , finvoice_specificationDetails :: Maybe SpecificationDetailsType
        , finvoice_epiDetails :: EpiDetailsType
        , finvoice_invoiceUrlNameText :: [GenericStringType0_512]
        , finvoice_invoiceUrlText :: [GenericStringType0_512]
        , finvoice_storageUrlText :: Maybe GenericStringType0_512
        , finvoice_layOutIdentifier :: Maybe GenericStringType0_35
        , finvoice_invoiceSegmentIdentifier :: Maybe GenericStringType0_35
        , finvoice_controlChecksum :: Maybe GenericStringType0_512
        , finvoice_messageChecksum :: Maybe GenericStringType0_512
        , finvoice_controlStampText :: Maybe GenericStringType0_512
        , finvoice_acceptanceStampText :: Maybe GenericStringType0_512
        , finvoice_originalInvoiceFormat :: Maybe GenericStringType0_35
        , finvoice_attachmentMessageDetails :: Maybe AttachmentMessageDetailsType
        }
        deriving (Eq,Show)
instance SchemaType Finvoice where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "Version" e pos
        commit $ interior e $ return (Finvoice a0)
            `apply` optional (parseSchemaType "MessageTransmissionDetails")
            `apply` parseSchemaType "SellerPartyDetails"
            `apply` optional (parseSchemaType "SellerOrganisationUnitNumber")
            `apply` optional (parseSchemaType "SellerSiteCode")
            `apply` optional (parseSchemaType "SellerContactPersonName")
            `apply` between (Occurs (Just 0) (Just 2))
                            (parseSchemaType "SellerContactPersonFunction")
            `apply` between (Occurs (Just 0) (Just 2))
                            (parseSchemaType "SellerContactPersonDepartment")
            `apply` optional (parseSchemaType "SellerCommunicationDetails")
            `apply` optional (parseSchemaType "SellerInformationDetails")
            `apply` optional (parseSchemaType "InvoiceSenderPartyDetails")
            `apply` optional (parseSchemaType "InvoiceRecipientPartyDetails")
            `apply` optional (parseSchemaType "InvoiceRecipientOrganisationUnitNumber")
            `apply` optional (parseSchemaType "InvoiceRecipientSiteCode")
            `apply` optional (parseSchemaType "InvoiceRecipientContactPersonName")
            `apply` between (Occurs (Just 0) (Just 2))
                            (parseSchemaType "InvoiceRecipientContactPersonFunction")
            `apply` between (Occurs (Just 0) (Just 2))
                            (parseSchemaType "InvoiceRecipientContactPersonDepartment")
            `apply` optional (parseSchemaType "InvoiceRecipientLanguageCode")
            `apply` optional (parseSchemaType "InvoiceRecipientCommunicationDetails")
            `apply` parseSchemaType "BuyerPartyDetails"
            `apply` optional (parseSchemaType "BuyerOrganisationUnitNumber")
            `apply` optional (parseSchemaType "BuyerSiteCode")
            `apply` optional (parseSchemaType "BuyerContactPersonName")
            `apply` between (Occurs (Just 0) (Just 2))
                            (parseSchemaType "BuyerContactPersonFunction")
            `apply` between (Occurs (Just 0) (Just 2))
                            (parseSchemaType "BuyerContactPersonDepartment")
            `apply` optional (parseSchemaType "BuyerCommunicationDetails")
            `apply` optional (parseSchemaType "DeliveryPartyDetails")
            `apply` optional (parseSchemaType "DeliveryOrganisationUnitNumber")
            `apply` optional (parseSchemaType "DeliverySiteCode")
            `apply` optional (parseSchemaType "DeliveryContactPersonName")
            `apply` between (Occurs (Just 0) (Just 2))
                            (parseSchemaType "DeliveryContactPersonFunction")
            `apply` between (Occurs (Just 0) (Just 2))
                            (parseSchemaType "DeliveryContactPersonDepartment")
            `apply` optional (parseSchemaType "DeliveryCommunicationDetails")
            `apply` optional (parseSchemaType "DeliveryDetails")
            `apply` many (parseSchemaType "AnyPartyDetails")
            `apply` parseSchemaType "InvoiceDetails"
            `apply` optional (parseSchemaType "PaymentCardInfo")
            `apply` optional (parseSchemaType "DirectDebitInfo")
            `apply` optional (parseSchemaType "PaymentStatusDetails")
            `apply` many (parseSchemaType "PartialPaymentDetails")
            `apply` optional (parseSchemaType "FactoringAgreementDetails")
            `apply` optional (parseSchemaType "VirtualBankBarcode")
            `apply` many1 (parseSchemaType "InvoiceRow")
            `apply` optional (parseSchemaType "SpecificationDetails")
            `apply` parseSchemaType "EpiDetails"
            `apply` many (parseSchemaType "InvoiceUrlNameText")
            `apply` many (parseSchemaType "InvoiceUrlText")
            `apply` optional (parseSchemaType "StorageUrlText")
            `apply` optional (parseSchemaType "LayOutIdentifier")
            `apply` optional (parseSchemaType "InvoiceSegmentIdentifier")
            `apply` optional (parseSchemaType "ControlChecksum")
            `apply` optional (parseSchemaType "MessageChecksum")
            `apply` optional (parseSchemaType "ControlStampText")
            `apply` optional (parseSchemaType "AcceptanceStampText")
            `apply` optional (parseSchemaType "OriginalInvoiceFormat")
            `apply` optional (parseSchemaType "AttachmentMessageDetails")
    schemaTypeToXML s x@Finvoice{} =
        toXMLElement s [ toXMLAttribute "Version" $ finvoice_version x
                       ]
            [ maybe [] (schemaTypeToXML "MessageTransmissionDetails") $ finvoice_messageTransmissionDetails x
            , schemaTypeToXML "SellerPartyDetails" $ finvoice_sellerPartyDetails x
            , maybe [] (schemaTypeToXML "SellerOrganisationUnitNumber") $ finvoice_sellerOrganisationUnitNumber x
            , maybe [] (schemaTypeToXML "SellerSiteCode") $ finvoice_sellerSiteCode x
            , maybe [] (schemaTypeToXML "SellerContactPersonName") $ finvoice_sellerContactPersonName x
            , concatMap (schemaTypeToXML "SellerContactPersonFunction") $ finvoice_sellerContactPersonFunction x
            , concatMap (schemaTypeToXML "SellerContactPersonDepartment") $ finvoice_sellerContactPersonDepartment x
            , maybe [] (schemaTypeToXML "SellerCommunicationDetails") $ finvoice_sellerCommunicationDetails x
            , maybe [] (schemaTypeToXML "SellerInformationDetails") $ finvoice_sellerInformationDetails x
            , maybe [] (schemaTypeToXML "InvoiceSenderPartyDetails") $ finvoice_invoiceSenderPartyDetails x
            , maybe [] (schemaTypeToXML "InvoiceRecipientPartyDetails") $ finvoice_invoiceRecipientPartyDetails x
            , maybe [] (schemaTypeToXML "InvoiceRecipientOrganisationUnitNumber") $ finvoice_invoiceRecipientOrganisationUnitNumber x
            , maybe [] (schemaTypeToXML "InvoiceRecipientSiteCode") $ finvoice_invoiceRecipientSiteCode x
            , maybe [] (schemaTypeToXML "InvoiceRecipientContactPersonName") $ finvoice_invoiceRecipientContactPersonName x
            , concatMap (schemaTypeToXML "InvoiceRecipientContactPersonFunction") $ finvoice_invoiceRecipientContactPersonFunction x
            , concatMap (schemaTypeToXML "InvoiceRecipientContactPersonDepartment") $ finvoice_invoiceRecipientContactPersonDepartment x
            , maybe [] (schemaTypeToXML "InvoiceRecipientLanguageCode") $ finvoice_invoiceRecipientLanguageCode x
            , maybe [] (schemaTypeToXML "InvoiceRecipientCommunicationDetails") $ finvoice_invoiceRecipientCommunicationDetails x
            , schemaTypeToXML "BuyerPartyDetails" $ finvoice_buyerPartyDetails x
            , maybe [] (schemaTypeToXML "BuyerOrganisationUnitNumber") $ finvoice_buyerOrganisationUnitNumber x
            , maybe [] (schemaTypeToXML "BuyerSiteCode") $ finvoice_buyerSiteCode x
            , maybe [] (schemaTypeToXML "BuyerContactPersonName") $ finvoice_buyerContactPersonName x
            , concatMap (schemaTypeToXML "BuyerContactPersonFunction") $ finvoice_buyerContactPersonFunction x
            , concatMap (schemaTypeToXML "BuyerContactPersonDepartment") $ finvoice_buyerContactPersonDepartment x
            , maybe [] (schemaTypeToXML "BuyerCommunicationDetails") $ finvoice_buyerCommunicationDetails x
            , maybe [] (schemaTypeToXML "DeliveryPartyDetails") $ finvoice_deliveryPartyDetails x
            , maybe [] (schemaTypeToXML "DeliveryOrganisationUnitNumber") $ finvoice_deliveryOrganisationUnitNumber x
            , maybe [] (schemaTypeToXML "DeliverySiteCode") $ finvoice_deliverySiteCode x
            , maybe [] (schemaTypeToXML "DeliveryContactPersonName") $ finvoice_deliveryContactPersonName x
            , concatMap (schemaTypeToXML "DeliveryContactPersonFunction") $ finvoice_deliveryContactPersonFunction x
            , concatMap (schemaTypeToXML "DeliveryContactPersonDepartment") $ finvoice_deliveryContactPersonDepartment x
            , maybe [] (schemaTypeToXML "DeliveryCommunicationDetails") $ finvoice_deliveryCommunicationDetails x
            , maybe [] (schemaTypeToXML "DeliveryDetails") $ finvoice_deliveryDetails x
            , concatMap (schemaTypeToXML "AnyPartyDetails") $ finvoice_anyPartyDetails x
            , schemaTypeToXML "InvoiceDetails" $ finvoice_invoiceDetails x
            , maybe [] (schemaTypeToXML "PaymentCardInfo") $ finvoice_paymentCardInfo x
            , maybe [] (schemaTypeToXML "DirectDebitInfo") $ finvoice_directDebitInfo x
            , maybe [] (schemaTypeToXML "PaymentStatusDetails") $ finvoice_paymentStatusDetails x
            , concatMap (schemaTypeToXML "PartialPaymentDetails") $ finvoice_partialPaymentDetails x
            , maybe [] (schemaTypeToXML "FactoringAgreementDetails") $ finvoice_factoringAgreementDetails x
            , maybe [] (schemaTypeToXML "VirtualBankBarcode") $ finvoice_virtualBankBarcode x
            , concatMap (schemaTypeToXML "InvoiceRow") $ finvoice_invoiceRow x
            , maybe [] (schemaTypeToXML "SpecificationDetails") $ finvoice_specificationDetails x
            , schemaTypeToXML "EpiDetails" $ finvoice_epiDetails x
            , concatMap (schemaTypeToXML "InvoiceUrlNameText") $ finvoice_invoiceUrlNameText x
            , concatMap (schemaTypeToXML "InvoiceUrlText") $ finvoice_invoiceUrlText x
            , maybe [] (schemaTypeToXML "StorageUrlText") $ finvoice_storageUrlText x
            , maybe [] (schemaTypeToXML "LayOutIdentifier") $ finvoice_layOutIdentifier x
            , maybe [] (schemaTypeToXML "InvoiceSegmentIdentifier") $ finvoice_invoiceSegmentIdentifier x
            , maybe [] (schemaTypeToXML "ControlChecksum") $ finvoice_controlChecksum x
            , maybe [] (schemaTypeToXML "MessageChecksum") $ finvoice_messageChecksum x
            , maybe [] (schemaTypeToXML "ControlStampText") $ finvoice_controlStampText x
            , maybe [] (schemaTypeToXML "AcceptanceStampText") $ finvoice_acceptanceStampText x
            , maybe [] (schemaTypeToXML "OriginalInvoiceFormat") $ finvoice_originalInvoiceFormat x
            , maybe [] (schemaTypeToXML "AttachmentMessageDetails") $ finvoice_attachmentMessageDetails x
            ]
 
elementFinvoice :: XMLParser Finvoice
elementFinvoice = parseSchemaType "Finvoice"
elementToXMLFinvoice :: Finvoice -> [Content ()]
elementToXMLFinvoice = schemaTypeToXML "Finvoice"
 
data MessageTransmissionDetailsType = MessageTransmissionDetailsType
        { messageTransmissionDetailsType_messageSenderDetails :: MessageSenderDetails
        , messageTransmissionDetailsType_messageReceiverDetails :: MessageReceiverDetails
        , messageTransmissionDetailsType_messageDetails :: MessageDetails
        }
        deriving (Eq,Show)
instance SchemaType MessageTransmissionDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return MessageTransmissionDetailsType
            `apply` parseSchemaType "MessageSenderDetails"
            `apply` parseSchemaType "MessageReceiverDetails"
            `apply` parseSchemaType "MessageDetails"
    schemaTypeToXML s x@MessageTransmissionDetailsType{} =
        toXMLElement s []
            [ schemaTypeToXML "MessageSenderDetails" $ messageTransmissionDetailsType_messageSenderDetails x
            , schemaTypeToXML "MessageReceiverDetails" $ messageTransmissionDetailsType_messageReceiverDetails x
            , schemaTypeToXML "MessageDetails" $ messageTransmissionDetailsType_messageDetails x
            ]
 
data AnyPartyDetailsType = AnyPartyDetailsType
        { anyPartyDetailsType_anyPartyText :: Anypartytexttype0_35
        , anyPartyDetailsType_anyPartyIdentifier :: Maybe PartyLegalRegIdType
        , anyPartyDetailsType_anyPartyOrganisationName :: [GenericStringType2_35]
        , anyPartyDetailsType_anyPartyOrganisationDepartment :: [GenericStringType0_35]
        , anyPartyDetailsType_anyPartyOrganisationTaxCode :: Maybe GenericStringType0_35
        , anyPartyDetailsType_anyPartyCode :: Maybe PartyIdentifierType
        , anyPartyDetailsType_anyPartyContactPersonName :: Maybe GenericStringType0_70
        , anyPartyDetailsType_anyPartyContactPersonFunction :: [GenericStringType0_35]
        , anyPartyDetailsType_anyPartyContactPersonDepartment :: [GenericStringType0_35]
        , anyPartyDetailsType_anyPartyCommunicationDetails :: Maybe AnyPartyCommunicationDetailsType
        , anyPartyDetailsType_anyPartyPostalAddressDetails :: Maybe AnyPartyPostalAddressDetails
        , anyPartyDetailsType_anyPartyOrganisationUnitNumber :: Maybe GenericStringType0_35
        , anyPartyDetailsType_anyPartySiteCode :: Maybe GenericStringType0_35
        }
        deriving (Eq,Show)
instance SchemaType AnyPartyDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return AnyPartyDetailsType
            `apply` parseSchemaType "AnyPartyText"
            `apply` optional (parseSchemaType "AnyPartyIdentifier")
            `apply` between (Occurs Nothing (Just 2))
                            (parseSchemaType "AnyPartyOrganisationName")
            `apply` between (Occurs (Just 0) (Just 2))
                            (parseSchemaType "AnyPartyOrganisationDepartment")
            `apply` optional (parseSchemaType "AnyPartyOrganisationTaxCode")
            `apply` optional (parseSchemaType "AnyPartyCode")
            `apply` optional (parseSchemaType "AnyPartyContactPersonName")
            `apply` between (Occurs (Just 0) (Just 2))
                            (parseSchemaType "AnyPartyContactPersonFunction")
            `apply` between (Occurs (Just 0) (Just 2))
                            (parseSchemaType "AnyPartyContactPersonDepartment")
            `apply` optional (parseSchemaType "AnyPartyCommunicationDetails")
            `apply` optional (parseSchemaType "AnyPartyPostalAddressDetails")
            `apply` optional (parseSchemaType "AnyPartyOrganisationUnitNumber")
            `apply` optional (parseSchemaType "AnyPartySiteCode")
    schemaTypeToXML s x@AnyPartyDetailsType{} =
        toXMLElement s []
            [ schemaTypeToXML "AnyPartyText" $ anyPartyDetailsType_anyPartyText x
            , maybe [] (schemaTypeToXML "AnyPartyIdentifier") $ anyPartyDetailsType_anyPartyIdentifier x
            , concatMap (schemaTypeToXML "AnyPartyOrganisationName") $ anyPartyDetailsType_anyPartyOrganisationName x
            , concatMap (schemaTypeToXML "AnyPartyOrganisationDepartment") $ anyPartyDetailsType_anyPartyOrganisationDepartment x
            , maybe [] (schemaTypeToXML "AnyPartyOrganisationTaxCode") $ anyPartyDetailsType_anyPartyOrganisationTaxCode x
            , maybe [] (schemaTypeToXML "AnyPartyCode") $ anyPartyDetailsType_anyPartyCode x
            , maybe [] (schemaTypeToXML "AnyPartyContactPersonName") $ anyPartyDetailsType_anyPartyContactPersonName x
            , concatMap (schemaTypeToXML "AnyPartyContactPersonFunction") $ anyPartyDetailsType_anyPartyContactPersonFunction x
            , concatMap (schemaTypeToXML "AnyPartyContactPersonDepartment") $ anyPartyDetailsType_anyPartyContactPersonDepartment x
            , maybe [] (schemaTypeToXML "AnyPartyCommunicationDetails") $ anyPartyDetailsType_anyPartyCommunicationDetails x
            , maybe [] (schemaTypeToXML "AnyPartyPostalAddressDetails") $ anyPartyDetailsType_anyPartyPostalAddressDetails x
            , maybe [] (schemaTypeToXML "AnyPartyOrganisationUnitNumber") $ anyPartyDetailsType_anyPartyOrganisationUnitNumber x
            , maybe [] (schemaTypeToXML "AnyPartySiteCode") $ anyPartyDetailsType_anyPartySiteCode x
            ]
 
data FactoringAgreementDetailsType = FactoringAgreementDetailsType
        { factoringAgreementDetailsType_factoringAgreementIdentifier :: GenericStringType0_35
        , factoringAgreementDetailsType_transmissionListIdentifier :: Maybe GenericStringType0_70
        , factoringAgreementDetailsType_endorsementClauseCode :: Maybe GenericStringType0_35
        , factoringAgreementDetailsType_factoringTypeCode :: Maybe GenericStringType0_35
        , factoringAgreementDetailsType_factoringFreeText :: [GenericStringType0_70]
        , factoringAgreementDetailsType_factoringPartyIdentifier :: Maybe PartyLegalRegIdType
        , factoringAgreementDetailsType_factoringPartyName :: Maybe GenericStringType0_35
        , factoringAgreementDetailsType_factoringPartyPostalAddressDetails :: Maybe FactoringPartyPostalAddressDetails
        }
        deriving (Eq,Show)
instance SchemaType FactoringAgreementDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return FactoringAgreementDetailsType
            `apply` parseSchemaType "FactoringAgreementIdentifier"
            `apply` optional (parseSchemaType "TransmissionListIdentifier")
            `apply` optional (parseSchemaType "EndorsementClauseCode")
            `apply` optional (parseSchemaType "FactoringTypeCode")
            `apply` many (parseSchemaType "FactoringFreeText")
            `apply` optional (parseSchemaType "FactoringPartyIdentifier")
            `apply` optional (parseSchemaType "FactoringPartyName")
            `apply` optional (parseSchemaType "FactoringPartyPostalAddressDetails")
    schemaTypeToXML s x@FactoringAgreementDetailsType{} =
        toXMLElement s []
            [ schemaTypeToXML "FactoringAgreementIdentifier" $ factoringAgreementDetailsType_factoringAgreementIdentifier x
            , maybe [] (schemaTypeToXML "TransmissionListIdentifier") $ factoringAgreementDetailsType_transmissionListIdentifier x
            , maybe [] (schemaTypeToXML "EndorsementClauseCode") $ factoringAgreementDetailsType_endorsementClauseCode x
            , maybe [] (schemaTypeToXML "FactoringTypeCode") $ factoringAgreementDetailsType_factoringTypeCode x
            , concatMap (schemaTypeToXML "FactoringFreeText") $ factoringAgreementDetailsType_factoringFreeText x
            , maybe [] (schemaTypeToXML "FactoringPartyIdentifier") $ factoringAgreementDetailsType_factoringPartyIdentifier x
            , maybe [] (schemaTypeToXML "FactoringPartyName") $ factoringAgreementDetailsType_factoringPartyName x
            , maybe [] (schemaTypeToXML "FactoringPartyPostalAddressDetails") $ factoringAgreementDetailsType_factoringPartyPostalAddressDetails x
            ]
 
data BuyerCommunicationDetailsType = BuyerCommunicationDetailsType
        { buyerCommunicationDetailsType_buyerPhoneNumberIdentifier :: Maybe GenericStringType0_35
        , buyerCommunicationDetailsType_buyerEmailaddressIdentifier :: Maybe GenericStringType0_70
        }
        deriving (Eq,Show)
instance SchemaType BuyerCommunicationDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return BuyerCommunicationDetailsType
            `apply` optional (parseSchemaType "BuyerPhoneNumberIdentifier")
            `apply` optional (parseSchemaType "BuyerEmailaddressIdentifier")
    schemaTypeToXML s x@BuyerCommunicationDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "BuyerPhoneNumberIdentifier") $ buyerCommunicationDetailsType_buyerPhoneNumberIdentifier x
            , maybe [] (schemaTypeToXML "BuyerEmailaddressIdentifier") $ buyerCommunicationDetailsType_buyerEmailaddressIdentifier x
            ]
 
data BuyerPartyDetailsType = BuyerPartyDetailsType
        { buyerPartyDetailsType_buyerPartyIdentifier :: Maybe PartyLegalRegIdType
        , buyerPartyDetailsType_buyerOrganisationName :: [GenericStringType2_70]
        , buyerPartyDetailsType_buyerOrganisationTradingName :: Maybe GenericStringType2_70
        , buyerPartyDetailsType_buyerOrganisationDepartment :: [GenericStringType0_35]
        , buyerPartyDetailsType_buyerOrganisationTaxCode :: Maybe GenericStringType0_35
        , buyerPartyDetailsType_buyerCode :: Maybe PartyIdentifierType
        , buyerPartyDetailsType_buyerPostalAddressDetails :: Maybe BuyerPostalAddressDetailsType
        }
        deriving (Eq,Show)
instance SchemaType BuyerPartyDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return BuyerPartyDetailsType
            `apply` optional (parseSchemaType "BuyerPartyIdentifier")
            `apply` many1 (parseSchemaType "BuyerOrganisationName")
            `apply` optional (parseSchemaType "BuyerOrganisationTradingName")
            `apply` between (Occurs (Just 0) (Just 2))
                            (parseSchemaType "BuyerOrganisationDepartment")
            `apply` optional (parseSchemaType "BuyerOrganisationTaxCode")
            `apply` optional (parseSchemaType "BuyerCode")
            `apply` optional (parseSchemaType "BuyerPostalAddressDetails")
    schemaTypeToXML s x@BuyerPartyDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "BuyerPartyIdentifier") $ buyerPartyDetailsType_buyerPartyIdentifier x
            , concatMap (schemaTypeToXML "BuyerOrganisationName") $ buyerPartyDetailsType_buyerOrganisationName x
            , maybe [] (schemaTypeToXML "BuyerOrganisationTradingName") $ buyerPartyDetailsType_buyerOrganisationTradingName x
            , concatMap (schemaTypeToXML "BuyerOrganisationDepartment") $ buyerPartyDetailsType_buyerOrganisationDepartment x
            , maybe [] (schemaTypeToXML "BuyerOrganisationTaxCode") $ buyerPartyDetailsType_buyerOrganisationTaxCode x
            , maybe [] (schemaTypeToXML "BuyerCode") $ buyerPartyDetailsType_buyerCode x
            , maybe [] (schemaTypeToXML "BuyerPostalAddressDetails") $ buyerPartyDetailsType_buyerPostalAddressDetails x
            ]
 
data BuyerPostalAddressDetailsType = BuyerPostalAddressDetailsType
        { buyerPostalAddressDetailsType_buyerStreetName :: [GenericStringType2_35]
        , buyerPostalAddressDetailsType_buyerTownName :: GenericStringType2_35
        , buyerPostalAddressDetailsType_buyerPostCodeIdentifier :: GenericStringType2_35
        , buyerPostalAddressDetailsType_buyerCountrySubdivision :: Maybe GenericStringType2_35
        , buyerPostalAddressDetailsType_countryCode :: Maybe CountryCodeType
        , buyerPostalAddressDetailsType_countryName :: Maybe GenericStringType0_35
        , buyerPostalAddressDetailsType_buyerPostOfficeBoxIdentifier :: Maybe GenericStringType0_35
        }
        deriving (Eq,Show)
instance SchemaType BuyerPostalAddressDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return BuyerPostalAddressDetailsType
            `apply` between (Occurs Nothing (Just 3))
                            (parseSchemaType "BuyerStreetName")
            `apply` parseSchemaType "BuyerTownName"
            `apply` parseSchemaType "BuyerPostCodeIdentifier"
            `apply` optional (parseSchemaType "BuyerCountrySubdivision")
            `apply` optional (parseSchemaType "CountryCode")
            `apply` optional (parseSchemaType "CountryName")
            `apply` optional (parseSchemaType "BuyerPostOfficeBoxIdentifier")
    schemaTypeToXML s x@BuyerPostalAddressDetailsType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "BuyerStreetName") $ buyerPostalAddressDetailsType_buyerStreetName x
            , schemaTypeToXML "BuyerTownName" $ buyerPostalAddressDetailsType_buyerTownName x
            , schemaTypeToXML "BuyerPostCodeIdentifier" $ buyerPostalAddressDetailsType_buyerPostCodeIdentifier x
            , maybe [] (schemaTypeToXML "BuyerCountrySubdivision") $ buyerPostalAddressDetailsType_buyerCountrySubdivision x
            , maybe [] (schemaTypeToXML "CountryCode") $ buyerPostalAddressDetailsType_countryCode x
            , maybe [] (schemaTypeToXML "CountryName") $ buyerPostalAddressDetailsType_countryName x
            , maybe [] (schemaTypeToXML "BuyerPostOfficeBoxIdentifier") $ buyerPostalAddressDetailsType_buyerPostOfficeBoxIdentifier x
            ]
 
data DeliveryCommunicationDetailsType = DeliveryCommunicationDetailsType
        { deliveryCommunicationDetailsType_deliveryPhoneNumberIdentifier :: Maybe GenericStringType0_35
        , deliveryCommunicationDetailsType_deliveryEmailaddressIdentifier :: Maybe GenericStringType0_70
        }
        deriving (Eq,Show)
instance SchemaType DeliveryCommunicationDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DeliveryCommunicationDetailsType
            `apply` optional (parseSchemaType "DeliveryPhoneNumberIdentifier")
            `apply` optional (parseSchemaType "DeliveryEmailaddressIdentifier")
    schemaTypeToXML s x@DeliveryCommunicationDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "DeliveryPhoneNumberIdentifier") $ deliveryCommunicationDetailsType_deliveryPhoneNumberIdentifier x
            , maybe [] (schemaTypeToXML "DeliveryEmailaddressIdentifier") $ deliveryCommunicationDetailsType_deliveryEmailaddressIdentifier x
            ]
 
data DeliveryDetailsType = DeliveryDetailsType
        { deliveryDetailsType_deliveryDate :: Maybe Date
        , deliveryDetailsType_deliveryPeriodDetails :: Maybe DeliveryPeriodDetailsType
        , deliveryDetailsType_shipmentPartyDetails :: Maybe ShipmentPartyDetails
        , deliveryDetailsType_deliveryMethodText :: Maybe GenericStringType0_512
        , deliveryDetailsType_deliveryTermsText :: Maybe GenericStringType0_512
        , deliveryDetailsType_deliveryTermsCode :: Maybe GenericStringType1_4
        , deliveryDetailsType_terminalAddressText :: Maybe GenericStringType0_512
        , deliveryDetailsType_waybillIdentifier :: Maybe GenericStringType0_70
        , deliveryDetailsType_waybillTypeCode :: Maybe GenericStringType0_35
        , deliveryDetailsType_clearanceIdentifier :: Maybe GenericStringType0_70
        , deliveryDetailsType_deliveryNoteIdentifier :: Maybe GenericStringType0_70
        , deliveryDetailsType_delivererIdentifier :: Maybe GenericStringType0_35
        , deliveryDetailsType_delivererName :: [GenericStringType0_35]
        , deliveryDetailsType_delivererCountrySubdivision :: Maybe GenericStringType2_35
        , deliveryDetailsType_delivererCountryCode :: Maybe CountryCodeType
        , deliveryDetailsType_delivererCountryName :: Maybe GenericStringType0_35
        , deliveryDetailsType_modeOfTransportIdentifier :: Maybe GenericStringType0_70
        , deliveryDetailsType_carrierName :: Maybe GenericStringType0_35
        , deliveryDetailsType_vesselName :: Maybe GenericStringType0_35
        , deliveryDetailsType_locationIdentifier :: Maybe GenericStringType0_70
        , deliveryDetailsType_transportInformationDate :: Maybe Date
        , deliveryDetailsType_countryOfOrigin :: Maybe GenericStringType0_35
        , deliveryDetailsType_countryOfDestinationName :: Maybe GenericStringType0_35
        , deliveryDetailsType_destinationCountryCode :: Maybe CountryCodeType
        , deliveryDetailsType_placeOfDischarge :: [GenericStringType0_35]
        , deliveryDetailsType_finalDestinationName :: [DestinationNameType]
        , deliveryDetailsType_manufacturerIdentifier :: Maybe GenericStringType0_35
        , deliveryDetailsType_manufacturerName :: [GenericStringType0_35]
        , deliveryDetailsType_manufacturerCountrySubdivision :: Maybe GenericStringType2_35
        , deliveryDetailsType_manufacturerCountryCode :: Maybe CountryCodeType
        , deliveryDetailsType_manufacturerCountryName :: Maybe GenericStringType0_35
        , deliveryDetailsType_manufacturerOrderIdentifier :: Maybe GenericStringType0_70
        , deliveryDetailsType_packageDetails :: Maybe PackageDetails
        }
        deriving (Eq,Show)
instance SchemaType DeliveryDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DeliveryDetailsType
            `apply` optional (parseSchemaType "DeliveryDate")
            `apply` optional (parseSchemaType "DeliveryPeriodDetails")
            `apply` optional (parseSchemaType "ShipmentPartyDetails")
            `apply` optional (parseSchemaType "DeliveryMethodText")
            `apply` optional (parseSchemaType "DeliveryTermsText")
            `apply` optional (parseSchemaType "DeliveryTermsCode")
            `apply` optional (parseSchemaType "TerminalAddressText")
            `apply` optional (parseSchemaType "WaybillIdentifier")
            `apply` optional (parseSchemaType "WaybillTypeCode")
            `apply` optional (parseSchemaType "ClearanceIdentifier")
            `apply` optional (parseSchemaType "DeliveryNoteIdentifier")
            `apply` optional (parseSchemaType "DelivererIdentifier")
            `apply` between (Occurs (Just 0) (Just 3))
                            (parseSchemaType "DelivererName")
            `apply` optional (parseSchemaType "DelivererCountrySubdivision")
            `apply` optional (parseSchemaType "DelivererCountryCode")
            `apply` optional (parseSchemaType "DelivererCountryName")
            `apply` optional (parseSchemaType "ModeOfTransportIdentifier")
            `apply` optional (parseSchemaType "CarrierName")
            `apply` optional (parseSchemaType "VesselName")
            `apply` optional (parseSchemaType "LocationIdentifier")
            `apply` optional (parseSchemaType "TransportInformationDate")
            `apply` optional (parseSchemaType "CountryOfOrigin")
            `apply` optional (parseSchemaType "CountryOfDestinationName")
            `apply` optional (parseSchemaType "DestinationCountryCode")
            `apply` between (Occurs (Just 0) (Just 3))
                            (parseSchemaType "PlaceOfDischarge")
            `apply` between (Occurs (Just 0) (Just 3))
                            (parseSchemaType "FinalDestinationName")
            `apply` optional (parseSchemaType "ManufacturerIdentifier")
            `apply` between (Occurs (Just 0) (Just 3))
                            (parseSchemaType "ManufacturerName")
            `apply` optional (parseSchemaType "ManufacturerCountrySubdivision")
            `apply` optional (parseSchemaType "ManufacturerCountryCode")
            `apply` optional (parseSchemaType "ManufacturerCountryName")
            `apply` optional (parseSchemaType "ManufacturerOrderIdentifier")
            `apply` optional (parseSchemaType "PackageDetails")
    schemaTypeToXML s x@DeliveryDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "DeliveryDate") $ deliveryDetailsType_deliveryDate x
            , maybe [] (schemaTypeToXML "DeliveryPeriodDetails") $ deliveryDetailsType_deliveryPeriodDetails x
            , maybe [] (schemaTypeToXML "ShipmentPartyDetails") $ deliveryDetailsType_shipmentPartyDetails x
            , maybe [] (schemaTypeToXML "DeliveryMethodText") $ deliveryDetailsType_deliveryMethodText x
            , maybe [] (schemaTypeToXML "DeliveryTermsText") $ deliveryDetailsType_deliveryTermsText x
            , maybe [] (schemaTypeToXML "DeliveryTermsCode") $ deliveryDetailsType_deliveryTermsCode x
            , maybe [] (schemaTypeToXML "TerminalAddressText") $ deliveryDetailsType_terminalAddressText x
            , maybe [] (schemaTypeToXML "WaybillIdentifier") $ deliveryDetailsType_waybillIdentifier x
            , maybe [] (schemaTypeToXML "WaybillTypeCode") $ deliveryDetailsType_waybillTypeCode x
            , maybe [] (schemaTypeToXML "ClearanceIdentifier") $ deliveryDetailsType_clearanceIdentifier x
            , maybe [] (schemaTypeToXML "DeliveryNoteIdentifier") $ deliveryDetailsType_deliveryNoteIdentifier x
            , maybe [] (schemaTypeToXML "DelivererIdentifier") $ deliveryDetailsType_delivererIdentifier x
            , concatMap (schemaTypeToXML "DelivererName") $ deliveryDetailsType_delivererName x
            , maybe [] (schemaTypeToXML "DelivererCountrySubdivision") $ deliveryDetailsType_delivererCountrySubdivision x
            , maybe [] (schemaTypeToXML "DelivererCountryCode") $ deliveryDetailsType_delivererCountryCode x
            , maybe [] (schemaTypeToXML "DelivererCountryName") $ deliveryDetailsType_delivererCountryName x
            , maybe [] (schemaTypeToXML "ModeOfTransportIdentifier") $ deliveryDetailsType_modeOfTransportIdentifier x
            , maybe [] (schemaTypeToXML "CarrierName") $ deliveryDetailsType_carrierName x
            , maybe [] (schemaTypeToXML "VesselName") $ deliveryDetailsType_vesselName x
            , maybe [] (schemaTypeToXML "LocationIdentifier") $ deliveryDetailsType_locationIdentifier x
            , maybe [] (schemaTypeToXML "TransportInformationDate") $ deliveryDetailsType_transportInformationDate x
            , maybe [] (schemaTypeToXML "CountryOfOrigin") $ deliveryDetailsType_countryOfOrigin x
            , maybe [] (schemaTypeToXML "CountryOfDestinationName") $ deliveryDetailsType_countryOfDestinationName x
            , maybe [] (schemaTypeToXML "DestinationCountryCode") $ deliveryDetailsType_destinationCountryCode x
            , concatMap (schemaTypeToXML "PlaceOfDischarge") $ deliveryDetailsType_placeOfDischarge x
            , concatMap (schemaTypeToXML "FinalDestinationName") $ deliveryDetailsType_finalDestinationName x
            , maybe [] (schemaTypeToXML "ManufacturerIdentifier") $ deliveryDetailsType_manufacturerIdentifier x
            , concatMap (schemaTypeToXML "ManufacturerName") $ deliveryDetailsType_manufacturerName x
            , maybe [] (schemaTypeToXML "ManufacturerCountrySubdivision") $ deliveryDetailsType_manufacturerCountrySubdivision x
            , maybe [] (schemaTypeToXML "ManufacturerCountryCode") $ deliveryDetailsType_manufacturerCountryCode x
            , maybe [] (schemaTypeToXML "ManufacturerCountryName") $ deliveryDetailsType_manufacturerCountryName x
            , maybe [] (schemaTypeToXML "ManufacturerOrderIdentifier") $ deliveryDetailsType_manufacturerOrderIdentifier x
            , maybe [] (schemaTypeToXML "PackageDetails") $ deliveryDetailsType_packageDetails x
            ]
 
data DeliveryPartyDetailsType = DeliveryPartyDetailsType
        { deliveryPartyDetailsType_deliveryPartyIdentifier :: Maybe PartyLegalRegIdType
        , deliveryPartyDetailsType_deliveryOrganisationName :: [GenericStringType2_35]
        , deliveryPartyDetailsType_deliveryOrganisationDepartment :: [GenericStringType0_35]
        , deliveryPartyDetailsType_deliveryOrganisationTaxCode :: Maybe GenericStringType0_35
        , deliveryPartyDetailsType_deliveryCode :: Maybe PartyIdentifierType
        , deliveryPartyDetailsType_deliveryPostalAddressDetails :: DeliveryPostalAddressDetailsType
        }
        deriving (Eq,Show)
instance SchemaType DeliveryPartyDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DeliveryPartyDetailsType
            `apply` optional (parseSchemaType "DeliveryPartyIdentifier")
            `apply` many1 (parseSchemaType "DeliveryOrganisationName")
            `apply` between (Occurs (Just 0) (Just 2))
                            (parseSchemaType "DeliveryOrganisationDepartment")
            `apply` optional (parseSchemaType "DeliveryOrganisationTaxCode")
            `apply` optional (parseSchemaType "DeliveryCode")
            `apply` parseSchemaType "DeliveryPostalAddressDetails"
    schemaTypeToXML s x@DeliveryPartyDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "DeliveryPartyIdentifier") $ deliveryPartyDetailsType_deliveryPartyIdentifier x
            , concatMap (schemaTypeToXML "DeliveryOrganisationName") $ deliveryPartyDetailsType_deliveryOrganisationName x
            , concatMap (schemaTypeToXML "DeliveryOrganisationDepartment") $ deliveryPartyDetailsType_deliveryOrganisationDepartment x
            , maybe [] (schemaTypeToXML "DeliveryOrganisationTaxCode") $ deliveryPartyDetailsType_deliveryOrganisationTaxCode x
            , maybe [] (schemaTypeToXML "DeliveryCode") $ deliveryPartyDetailsType_deliveryCode x
            , schemaTypeToXML "DeliveryPostalAddressDetails" $ deliveryPartyDetailsType_deliveryPostalAddressDetails x
            ]
 
data DeliveryPeriodDetailsType = DeliveryPeriodDetailsType
        { deliveryPeriodDetailsType_startDate :: Date
        , deliveryPeriodDetailsType_endDate :: Date
        }
        deriving (Eq,Show)
instance SchemaType DeliveryPeriodDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DeliveryPeriodDetailsType
            `apply` parseSchemaType "StartDate"
            `apply` parseSchemaType "EndDate"
    schemaTypeToXML s x@DeliveryPeriodDetailsType{} =
        toXMLElement s []
            [ schemaTypeToXML "StartDate" $ deliveryPeriodDetailsType_startDate x
            , schemaTypeToXML "EndDate" $ deliveryPeriodDetailsType_endDate x
            ]
 
data DeliveryPostalAddressDetailsType = DeliveryPostalAddressDetailsType
        { deliveryPostalAddressDetailsType_deliveryStreetName :: [GenericStringType2_35]
        , deliveryPostalAddressDetailsType_deliveryTownName :: GenericStringType2_35
        , deliveryPostalAddressDetailsType_deliveryPostCodeIdentifier :: GenericStringType2_35
        , deliveryPostalAddressDetailsType_deliveryCountrySubdivision :: Maybe GenericStringType2_35
        , deliveryPostalAddressDetailsType_countryCode :: Maybe CountryCodeType
        , deliveryPostalAddressDetailsType_countryName :: Maybe GenericStringType0_35
        , deliveryPostalAddressDetailsType_deliveryPostofficeBoxIdentifier :: Maybe GenericStringType0_35
        }
        deriving (Eq,Show)
instance SchemaType DeliveryPostalAddressDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DeliveryPostalAddressDetailsType
            `apply` between (Occurs Nothing (Just 3))
                            (parseSchemaType "DeliveryStreetName")
            `apply` parseSchemaType "DeliveryTownName"
            `apply` parseSchemaType "DeliveryPostCodeIdentifier"
            `apply` optional (parseSchemaType "DeliveryCountrySubdivision")
            `apply` optional (parseSchemaType "CountryCode")
            `apply` optional (parseSchemaType "CountryName")
            `apply` optional (parseSchemaType "DeliveryPostofficeBoxIdentifier")
    schemaTypeToXML s x@DeliveryPostalAddressDetailsType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "DeliveryStreetName") $ deliveryPostalAddressDetailsType_deliveryStreetName x
            , schemaTypeToXML "DeliveryTownName" $ deliveryPostalAddressDetailsType_deliveryTownName x
            , schemaTypeToXML "DeliveryPostCodeIdentifier" $ deliveryPostalAddressDetailsType_deliveryPostCodeIdentifier x
            , maybe [] (schemaTypeToXML "DeliveryCountrySubdivision") $ deliveryPostalAddressDetailsType_deliveryCountrySubdivision x
            , maybe [] (schemaTypeToXML "CountryCode") $ deliveryPostalAddressDetailsType_countryCode x
            , maybe [] (schemaTypeToXML "CountryName") $ deliveryPostalAddressDetailsType_countryName x
            , maybe [] (schemaTypeToXML "DeliveryPostofficeBoxIdentifier") $ deliveryPostalAddressDetailsType_deliveryPostofficeBoxIdentifier x
            ]
 
data EpiAccountIDType = EpiAccountIDType GenericNMtokenType1_34 EpiAccountIDTypeAttributes deriving (Eq,Show)
data EpiAccountIDTypeAttributes = EpiAccountIDTypeAttributes
    { epiAccountIDTypeAttributes_identificationSchemeName :: Xsd.XsdString
    }
    deriving (Eq,Show)
instance SchemaType EpiAccountIDType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- getAttribute "IdentificationSchemeName" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ EpiAccountIDType v (EpiAccountIDTypeAttributes a0)
    schemaTypeToXML s (EpiAccountIDType bt at) =
        addXMLAttributes [ toXMLAttribute "IdentificationSchemeName" $ epiAccountIDTypeAttributes_identificationSchemeName at
                         ]
            $ schemaTypeToXML s bt
instance Extension EpiAccountIDType GenericNMtokenType1_34 where
    supertype (EpiAccountIDType s _) = s
 
data EpiBfiIdentifierType = EpiBfiIdentifierType GenericNMtokenType8_11 EpiBfiIdentifierTypeAttributes deriving (Eq,Show)
data EpiBfiIdentifierTypeAttributes = EpiBfiIdentifierTypeAttributes
    { epiBfiIdentifierTypeAttributes_identificationSchemeName :: Xsd.XsdString
    }
    deriving (Eq,Show)
instance SchemaType EpiBfiIdentifierType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- getAttribute "IdentificationSchemeName" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ EpiBfiIdentifierType v (EpiBfiIdentifierTypeAttributes a0)
    schemaTypeToXML s (EpiBfiIdentifierType bt at) =
        addXMLAttributes [ toXMLAttribute "IdentificationSchemeName" $ epiBfiIdentifierTypeAttributes_identificationSchemeName at
                         ]
            $ schemaTypeToXML s bt
instance Extension EpiBfiIdentifierType GenericNMtokenType8_11 where
    supertype (EpiBfiIdentifierType s _) = s
 
data EpiDetailsType = EpiDetailsType
        { epiDetailsType_epiIdentificationDetails :: EpiIdentificationDetailsType
        , epiDetailsType_epiPartyDetails :: EpiPartyDetailsType
        , epiDetailsType_epiPaymentInstructionDetails :: EpiPaymentInstructionDetailsType
        }
        deriving (Eq,Show)
instance SchemaType EpiDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return EpiDetailsType
            `apply` parseSchemaType "EpiIdentificationDetails"
            `apply` parseSchemaType "EpiPartyDetails"
            `apply` parseSchemaType "EpiPaymentInstructionDetails"
    schemaTypeToXML s x@EpiDetailsType{} =
        toXMLElement s []
            [ schemaTypeToXML "EpiIdentificationDetails" $ epiDetailsType_epiIdentificationDetails x
            , schemaTypeToXML "EpiPartyDetails" $ epiDetailsType_epiPartyDetails x
            , schemaTypeToXML "EpiPaymentInstructionDetails" $ epiDetailsType_epiPaymentInstructionDetails x
            ]
 
data EpiIdentificationDetailsType = EpiIdentificationDetailsType
        { epiIdentificationDetailsType_epiDate :: Date
        , epiIdentificationDetailsType_epiReference :: GenericNMtokenType0_35
        , epiIdentificationDetailsType_epiUrl :: Maybe GenericNMtokenType0_512
        , epiIdentificationDetailsType_epiEmail :: Maybe GenericStringType0_70
        , epiIdentificationDetailsType_epiOrderInfo :: [GenericTokenType0_70]
        }
        deriving (Eq,Show)
instance SchemaType EpiIdentificationDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return EpiIdentificationDetailsType
            `apply` parseSchemaType "EpiDate"
            `apply` parseSchemaType "EpiReference"
            `apply` optional (parseSchemaType "EpiUrl")
            `apply` optional (parseSchemaType "EpiEmail")
            `apply` between (Occurs (Just 0) (Just 7))
                            (parseSchemaType "EpiOrderInfo")
    schemaTypeToXML s x@EpiIdentificationDetailsType{} =
        toXMLElement s []
            [ schemaTypeToXML "EpiDate" $ epiIdentificationDetailsType_epiDate x
            , schemaTypeToXML "EpiReference" $ epiIdentificationDetailsType_epiReference x
            , maybe [] (schemaTypeToXML "EpiUrl") $ epiIdentificationDetailsType_epiUrl x
            , maybe [] (schemaTypeToXML "EpiEmail") $ epiIdentificationDetailsType_epiEmail x
            , concatMap (schemaTypeToXML "EpiOrderInfo") $ epiIdentificationDetailsType_epiOrderInfo x
            ]
 
data EpiPartyDetailsType = EpiPartyDetailsType
        { epiPartyDetailsType_epiBfiPartyDetails :: EpiBfiPartyDetailsType
        , epiPartyDetailsType_epiBeneficiaryPartyDetails :: EpiBeneficiaryPartyDetailsType
        }
        deriving (Eq,Show)
instance SchemaType EpiPartyDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return EpiPartyDetailsType
            `apply` parseSchemaType "EpiBfiPartyDetails"
            `apply` parseSchemaType "EpiBeneficiaryPartyDetails"
    schemaTypeToXML s x@EpiPartyDetailsType{} =
        toXMLElement s []
            [ schemaTypeToXML "EpiBfiPartyDetails" $ epiPartyDetailsType_epiBfiPartyDetails x
            , schemaTypeToXML "EpiBeneficiaryPartyDetails" $ epiPartyDetailsType_epiBeneficiaryPartyDetails x
            ]
 
data EpiBfiPartyDetailsType = EpiBfiPartyDetailsType
        { epiBfiPartyDetailsType_epiBfiIdentifier :: Maybe EpiBfiIdentifierType
        , epiBfiPartyDetailsType_epiBfiName :: Maybe GenericStringType1_35
        }
        deriving (Eq,Show)
instance SchemaType EpiBfiPartyDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return EpiBfiPartyDetailsType
            `apply` optional (parseSchemaType "EpiBfiIdentifier")
            `apply` optional (parseSchemaType "EpiBfiName")
    schemaTypeToXML s x@EpiBfiPartyDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "EpiBfiIdentifier") $ epiBfiPartyDetailsType_epiBfiIdentifier x
            , maybe [] (schemaTypeToXML "EpiBfiName") $ epiBfiPartyDetailsType_epiBfiName x
            ]
 
data EpiBeneficiaryPartyDetailsType = EpiBeneficiaryPartyDetailsType
        { epiBeneficiaryPartyDetailsType_epiNameAddressDetails :: Maybe GenericTokenType2_35
        , epiBeneficiaryPartyDetailsType_epiBei :: Maybe GenericNMtokenType8_11
        , epiBeneficiaryPartyDetailsType_epiAccountID :: EpiAccountIDType
        }
        deriving (Eq,Show)
instance SchemaType EpiBeneficiaryPartyDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return EpiBeneficiaryPartyDetailsType
            `apply` optional (parseSchemaType "EpiNameAddressDetails")
            `apply` optional (parseSchemaType "EpiBei")
            `apply` parseSchemaType "EpiAccountID"
    schemaTypeToXML s x@EpiBeneficiaryPartyDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "EpiNameAddressDetails") $ epiBeneficiaryPartyDetailsType_epiNameAddressDetails x
            , maybe [] (schemaTypeToXML "EpiBei") $ epiBeneficiaryPartyDetailsType_epiBei x
            , schemaTypeToXML "EpiAccountID" $ epiBeneficiaryPartyDetailsType_epiAccountID x
            ]
 
data EpiPaymentInstructionDetailsType = EpiPaymentInstructionDetailsType
        { epiPaymentInstructionDetailsType_epiPaymentInstructionId :: Maybe GenericStringType0_35
        , epiPaymentInstructionDetailsType_epiTransactionTypeCode :: Maybe GenericTokenType3
        , epiPaymentInstructionDetailsType_epiInstructionCode :: Maybe GenericNMtokenType0_35
        , epiPaymentInstructionDetailsType_epiRemittanceInfoIdentifier :: Maybe EpiRemittanceInfoIdentifierType
        , epiPaymentInstructionDetailsType_epiInstructedAmount :: EpiAmount
        , epiPaymentInstructionDetailsType_epiCharge :: EpiChargeType
        , epiPaymentInstructionDetailsType_epiDateOptionDate :: Date
        , epiPaymentInstructionDetailsType_epiPaymentMeansCode :: Maybe Untdid4461
        , epiPaymentInstructionDetailsType_epiPaymentMeansText :: Maybe GenericStringType1_70
        }
        deriving (Eq,Show)
instance SchemaType EpiPaymentInstructionDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return EpiPaymentInstructionDetailsType
            `apply` optional (parseSchemaType "EpiPaymentInstructionId")
            `apply` optional (parseSchemaType "EpiTransactionTypeCode")
            `apply` optional (parseSchemaType "EpiInstructionCode")
            `apply` optional (parseSchemaType "EpiRemittanceInfoIdentifier")
            `apply` parseSchemaType "EpiInstructedAmount"
            `apply` parseSchemaType "EpiCharge"
            `apply` parseSchemaType "EpiDateOptionDate"
            `apply` optional (parseSchemaType "EpiPaymentMeansCode")
            `apply` optional (parseSchemaType "EpiPaymentMeansText")
    schemaTypeToXML s x@EpiPaymentInstructionDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "EpiPaymentInstructionId") $ epiPaymentInstructionDetailsType_epiPaymentInstructionId x
            , maybe [] (schemaTypeToXML "EpiTransactionTypeCode") $ epiPaymentInstructionDetailsType_epiTransactionTypeCode x
            , maybe [] (schemaTypeToXML "EpiInstructionCode") $ epiPaymentInstructionDetailsType_epiInstructionCode x
            , maybe [] (schemaTypeToXML "EpiRemittanceInfoIdentifier") $ epiPaymentInstructionDetailsType_epiRemittanceInfoIdentifier x
            , schemaTypeToXML "EpiInstructedAmount" $ epiPaymentInstructionDetailsType_epiInstructedAmount x
            , schemaTypeToXML "EpiCharge" $ epiPaymentInstructionDetailsType_epiCharge x
            , schemaTypeToXML "EpiDateOptionDate" $ epiPaymentInstructionDetailsType_epiDateOptionDate x
            , maybe [] (schemaTypeToXML "EpiPaymentMeansCode") $ epiPaymentInstructionDetailsType_epiPaymentMeansCode x
            , maybe [] (schemaTypeToXML "EpiPaymentMeansText") $ epiPaymentInstructionDetailsType_epiPaymentMeansText x
            ]
 
data EpiRemittanceInfoIdentifierType = EpiRemittanceInfoIdentifierType EpiRemittanceInfoIdentifierPattern EpiRemittanceInfoIdentifierTypeAttributes deriving (Eq,Show)
data EpiRemittanceInfoIdentifierTypeAttributes = EpiRemittanceInfoIdentifierTypeAttributes
    { epiRemittanceInfoIdentifierTypeAttributes_identificationSchemeName :: Maybe Xsd.XsdString
    }
    deriving (Eq,Show)
instance SchemaType EpiRemittanceInfoIdentifierType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- optional $ getAttribute "IdentificationSchemeName" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ EpiRemittanceInfoIdentifierType v (EpiRemittanceInfoIdentifierTypeAttributes a0)
    schemaTypeToXML s (EpiRemittanceInfoIdentifierType bt at) =
        addXMLAttributes [ maybe [] (toXMLAttribute "IdentificationSchemeName") $ epiRemittanceInfoIdentifierTypeAttributes_identificationSchemeName at
                         ]
            $ schemaTypeToXML s bt
instance Extension EpiRemittanceInfoIdentifierType EpiRemittanceInfoIdentifierPattern where
    supertype (EpiRemittanceInfoIdentifierType s _) = s
 
newtype EpiRemittanceInfoIdentifierPattern = EpiRemittanceInfoIdentifierPattern Xs.NMTOKEN deriving (Eq,Show)
instance Restricts EpiRemittanceInfoIdentifierPattern Xs.NMTOKEN where
    restricts (EpiRemittanceInfoIdentifierPattern x) = x
instance SchemaType EpiRemittanceInfoIdentifierPattern where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EpiRemittanceInfoIdentifierPattern x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EpiRemittanceInfoIdentifierPattern where
    acceptingParser = fmap EpiRemittanceInfoIdentifierPattern acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern ([0-9]{2,20})|(RF[0-9][0-9][0-9A-Za-z]{1,21}))
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (EpiRemittanceInfoIdentifierPattern x) = simpleTypeText x
 
data EpiChargeType = EpiChargeType Xs.Token EpiChargeTypeAttributes deriving (Eq,Show)
data EpiChargeTypeAttributes = EpiChargeTypeAttributes
    { epiChargeTypeAttributes_chargeOption :: Xsd.XsdString
    }
    deriving (Eq,Show)
instance SchemaType EpiChargeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- getAttribute "ChargeOption" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ EpiChargeType v (EpiChargeTypeAttributes a0)
    schemaTypeToXML s (EpiChargeType bt at) =
        addXMLAttributes [ toXMLAttribute "ChargeOption" $ epiChargeTypeAttributes_chargeOption at
                         ]
            $ schemaTypeToXML s bt
instance Extension EpiChargeType Xs.Token where
    supertype (EpiChargeType s _) = s
 
data InvoiceDetailsType = InvoiceDetailsType
        { invoiceDetailsType_invoiceTypeCode :: InvoiceTypeCodeTypeFI
        , invoiceDetailsType_invoiceTypeCodeUN :: Maybe Untdid1001
        , invoiceDetailsType_invoiceTypeText :: GenericStringType1_35
        , invoiceDetailsType_invoiceClassification :: Maybe InvoiceClassificationType
        , invoiceDetailsType_originCode :: OriginCodeType
        , invoiceDetailsType_originText :: Maybe GenericStringType0_35
        , invoiceDetailsType_invoicedObjectID :: Maybe InvoicedObjectIDType
        , invoiceDetailsType_invoiceNumber :: GenericStringType1_20
        , invoiceDetailsType_invoiceDate :: Date
        , invoiceDetailsType_originalInvoiceNumber :: Maybe GenericStringType1_20
        , invoiceDetailsType_originalInvoiceDate :: Maybe Date
        , invoiceDetailsType_originalInvoiceReference :: [OriginalInvoiceReferenceType]
        , invoiceDetailsType_invoicingPeriodStartDate :: Maybe Date
        , invoiceDetailsType_invoicingPeriodEndDate :: Maybe Date
        , invoiceDetailsType_sellerReferenceIdentifier :: Maybe GenericStringType0_70
        , invoiceDetailsType_sellerReferenceIdentifierUrlText :: Maybe GenericStringType0_512
        , invoiceDetailsType_buyersSellerIdentifier :: Maybe PartyIdentifierType
        , invoiceDetailsType_sellersBuyerIdentifier :: Maybe PartyIdentifierType
        , invoiceDetailsType_orderIdentifier :: Maybe GenericStringType0_70
        , invoiceDetailsType_orderIdentifierUrlText :: Maybe GenericStringType0_512
        , invoiceDetailsType_orderDate :: Maybe Date
        , invoiceDetailsType_ordererName :: Maybe GenericStringType0_35
        , invoiceDetailsType_salesPersonName :: Maybe GenericStringType0_35
        , invoiceDetailsType_orderConfirmationIdentifier :: Maybe GenericStringType0_70
        , invoiceDetailsType_orderConfirmationDate :: Maybe Date
        , invoiceDetailsType_agreementIdentifier :: Maybe GenericStringType0_70
        , invoiceDetailsType_agreementIdentifierUrlText :: Maybe GenericStringType0_512
        , invoiceDetailsType_agreementTypeText :: Maybe GenericStringType0_35
        , invoiceDetailsType_agreementTypeCode :: Maybe GenericStringType0_35
        , invoiceDetailsType_agreementDate :: Maybe Date
        , invoiceDetailsType_notificationIdentifier :: Maybe GenericStringType0_70
        , invoiceDetailsType_notificationDate :: Maybe Date
        , invoiceDetailsType_registrationNumberIdentifier :: Maybe GenericStringType0_70
        , invoiceDetailsType_controllerIdentifier :: Maybe GenericStringType0_70
        , invoiceDetailsType_controllerName :: Maybe GenericStringType0_35
        , invoiceDetailsType_controlDate :: Maybe Date
        , invoiceDetailsType_buyerReferenceIdentifier :: Maybe GenericStringType0_70
        , invoiceDetailsType_projectReferenceIdentifier :: Maybe GenericStringType0_70
        , invoiceDetailsType_definitionDetails :: [DefinitionDetails]
        , invoiceDetailsType_rowsTotalVatExcludedAmount :: Maybe Amount
        , invoiceDetailsType_discountsTotalVatExcludedAmount :: Maybe Amount
        , invoiceDetailsType_chargesTotalVatExcludedAmount :: Maybe Amount
        , invoiceDetailsType_invoiceTotalVatExcludedAmount :: Maybe Amount
        , invoiceDetailsType_invoiceTotalVatAmount :: Maybe Amount
        , invoiceDetailsType_invoiceTotalVatAccountingAmount :: Maybe Amount
        , invoiceDetailsType_invoiceTotalVatIncludedAmount :: Amount
        , invoiceDetailsType_invoiceTotalRoundoffAmount :: Maybe Amount
        , invoiceDetailsType_invoicePaidAmount :: Maybe Amount
        , invoiceDetailsType_exchangeRate :: Maybe ExchangeRate
        , invoiceDetailsType_otherCurrencyAmountVatExcludedAmount :: Maybe Amount
        , invoiceDetailsType_otherCurrencyAmountVatIncludedAmount :: Maybe Amount
        , invoiceDetailsType_creditLimitAmount :: Maybe Amount
        , invoiceDetailsType_creditInterestPercent :: Maybe Percentage
        , invoiceDetailsType_operationLimitAmount :: Maybe Amount
        , invoiceDetailsType_monthlyAmount :: Maybe Amount
        , invoiceDetailsType_shortProposedAccountIdentifier :: Maybe GenericNMtokenType0_4
        , invoiceDetailsType_normalProposedAccountIdentifier :: Maybe GenericNMtokenType0_4
        , invoiceDetailsType_proposedAccountText :: Maybe GenericStringType0_35
        , invoiceDetailsType_accountDimensionText :: Maybe GenericStringType0_35
        , invoiceDetailsType_sellerAccountText :: Maybe GenericStringType0_35
        , invoiceDetailsType_vatPoint :: Maybe VatPointType
        , invoiceDetailsType_vatSpecificationDetails :: [VatSpecificationDetailsType]
        , invoiceDetailsType_invoiceFreeText :: [GenericStringType0_512]
        , invoiceDetailsType_invoiceVatFreeText :: Maybe GenericStringType0_70
        , invoiceDetailsType_paymentTermsDetails :: [PaymentTermsDetailsType]
        , invoiceDetailsType_discountDetails :: [DiscountDetailsType]
        , invoiceDetailsType_chargeDetails :: [InvoiceChargeDetailsType]
        , invoiceDetailsType_tenderReference :: Maybe GenericStringType1_70
        }
        deriving (Eq,Show)
instance SchemaType InvoiceDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return InvoiceDetailsType
            `apply` parseSchemaType "InvoiceTypeCode"
            `apply` optional (parseSchemaType "InvoiceTypeCodeUN")
            `apply` parseSchemaType "InvoiceTypeText"
            `apply` optional (parseSchemaType "InvoiceClassification")
            `apply` parseSchemaType "OriginCode"
            `apply` optional (parseSchemaType "OriginText")
            `apply` optional (parseSchemaType "InvoicedObjectID")
            `apply` parseSchemaType "InvoiceNumber"
            `apply` parseSchemaType "InvoiceDate"
            `apply` optional (parseSchemaType "OriginalInvoiceNumber")
            `apply` optional (parseSchemaType "OriginalInvoiceDate")
            `apply` many (parseSchemaType "OriginalInvoiceReference")
            `apply` optional (parseSchemaType "InvoicingPeriodStartDate")
            `apply` optional (parseSchemaType "InvoicingPeriodEndDate")
            `apply` optional (parseSchemaType "SellerReferenceIdentifier")
            `apply` optional (parseSchemaType "SellerReferenceIdentifierUrlText")
            `apply` optional (parseSchemaType "BuyersSellerIdentifier")
            `apply` optional (parseSchemaType "SellersBuyerIdentifier")
            `apply` optional (parseSchemaType "OrderIdentifier")
            `apply` optional (parseSchemaType "OrderIdentifierUrlText")
            `apply` optional (parseSchemaType "OrderDate")
            `apply` optional (parseSchemaType "OrdererName")
            `apply` optional (parseSchemaType "SalesPersonName")
            `apply` optional (parseSchemaType "OrderConfirmationIdentifier")
            `apply` optional (parseSchemaType "OrderConfirmationDate")
            `apply` optional (parseSchemaType "AgreementIdentifier")
            `apply` optional (parseSchemaType "AgreementIdentifierUrlText")
            `apply` optional (parseSchemaType "AgreementTypeText")
            `apply` optional (parseSchemaType "AgreementTypeCode")
            `apply` optional (parseSchemaType "AgreementDate")
            `apply` optional (parseSchemaType "NotificationIdentifier")
            `apply` optional (parseSchemaType "NotificationDate")
            `apply` optional (parseSchemaType "RegistrationNumberIdentifier")
            `apply` optional (parseSchemaType "ControllerIdentifier")
            `apply` optional (parseSchemaType "ControllerName")
            `apply` optional (parseSchemaType "ControlDate")
            `apply` optional (parseSchemaType "BuyerReferenceIdentifier")
            `apply` optional (parseSchemaType "ProjectReferenceIdentifier")
            `apply` many (parseSchemaType "DefinitionDetails")
            `apply` optional (parseSchemaType "RowsTotalVatExcludedAmount")
            `apply` optional (parseSchemaType "DiscountsTotalVatExcludedAmount")
            `apply` optional (parseSchemaType "ChargesTotalVatExcludedAmount")
            `apply` optional (parseSchemaType "InvoiceTotalVatExcludedAmount")
            `apply` optional (parseSchemaType "InvoiceTotalVatAmount")
            `apply` optional (parseSchemaType "InvoiceTotalVatAccountingAmount")
            `apply` parseSchemaType "InvoiceTotalVatIncludedAmount"
            `apply` optional (parseSchemaType "InvoiceTotalRoundoffAmount")
            `apply` optional (parseSchemaType "InvoicePaidAmount")
            `apply` optional (parseSchemaType "ExchangeRate")
            `apply` optional (parseSchemaType "OtherCurrencyAmountVatExcludedAmount")
            `apply` optional (parseSchemaType "OtherCurrencyAmountVatIncludedAmount")
            `apply` optional (parseSchemaType "CreditLimitAmount")
            `apply` optional (parseSchemaType "CreditInterestPercent")
            `apply` optional (parseSchemaType "OperationLimitAmount")
            `apply` optional (parseSchemaType "MonthlyAmount")
            `apply` optional (parseSchemaType "ShortProposedAccountIdentifier")
            `apply` optional (parseSchemaType "NormalProposedAccountIdentifier")
            `apply` optional (parseSchemaType "ProposedAccountText")
            `apply` optional (parseSchemaType "AccountDimensionText")
            `apply` optional (parseSchemaType "SellerAccountText")
            `apply` optional (parseSchemaType "VatPoint")
            `apply` many (parseSchemaType "VatSpecificationDetails")
            `apply` many (parseSchemaType "InvoiceFreeText")
            `apply` optional (parseSchemaType "InvoiceVatFreeText")
            `apply` many (parseSchemaType "PaymentTermsDetails")
            `apply` many (parseSchemaType "DiscountDetails")
            `apply` many (parseSchemaType "ChargeDetails")
            `apply` optional (parseSchemaType "TenderReference")
    schemaTypeToXML s x@InvoiceDetailsType{} =
        toXMLElement s []
            [ schemaTypeToXML "InvoiceTypeCode" $ invoiceDetailsType_invoiceTypeCode x
            , maybe [] (schemaTypeToXML "InvoiceTypeCodeUN") $ invoiceDetailsType_invoiceTypeCodeUN x
            , schemaTypeToXML "InvoiceTypeText" $ invoiceDetailsType_invoiceTypeText x
            , maybe [] (schemaTypeToXML "InvoiceClassification") $ invoiceDetailsType_invoiceClassification x
            , schemaTypeToXML "OriginCode" $ invoiceDetailsType_originCode x
            , maybe [] (schemaTypeToXML "OriginText") $ invoiceDetailsType_originText x
            , maybe [] (schemaTypeToXML "InvoicedObjectID") $ invoiceDetailsType_invoicedObjectID x
            , schemaTypeToXML "InvoiceNumber" $ invoiceDetailsType_invoiceNumber x
            , schemaTypeToXML "InvoiceDate" $ invoiceDetailsType_invoiceDate x
            , maybe [] (schemaTypeToXML "OriginalInvoiceNumber") $ invoiceDetailsType_originalInvoiceNumber x
            , maybe [] (schemaTypeToXML "OriginalInvoiceDate") $ invoiceDetailsType_originalInvoiceDate x
            , concatMap (schemaTypeToXML "OriginalInvoiceReference") $ invoiceDetailsType_originalInvoiceReference x
            , maybe [] (schemaTypeToXML "InvoicingPeriodStartDate") $ invoiceDetailsType_invoicingPeriodStartDate x
            , maybe [] (schemaTypeToXML "InvoicingPeriodEndDate") $ invoiceDetailsType_invoicingPeriodEndDate x
            , maybe [] (schemaTypeToXML "SellerReferenceIdentifier") $ invoiceDetailsType_sellerReferenceIdentifier x
            , maybe [] (schemaTypeToXML "SellerReferenceIdentifierUrlText") $ invoiceDetailsType_sellerReferenceIdentifierUrlText x
            , maybe [] (schemaTypeToXML "BuyersSellerIdentifier") $ invoiceDetailsType_buyersSellerIdentifier x
            , maybe [] (schemaTypeToXML "SellersBuyerIdentifier") $ invoiceDetailsType_sellersBuyerIdentifier x
            , maybe [] (schemaTypeToXML "OrderIdentifier") $ invoiceDetailsType_orderIdentifier x
            , maybe [] (schemaTypeToXML "OrderIdentifierUrlText") $ invoiceDetailsType_orderIdentifierUrlText x
            , maybe [] (schemaTypeToXML "OrderDate") $ invoiceDetailsType_orderDate x
            , maybe [] (schemaTypeToXML "OrdererName") $ invoiceDetailsType_ordererName x
            , maybe [] (schemaTypeToXML "SalesPersonName") $ invoiceDetailsType_salesPersonName x
            , maybe [] (schemaTypeToXML "OrderConfirmationIdentifier") $ invoiceDetailsType_orderConfirmationIdentifier x
            , maybe [] (schemaTypeToXML "OrderConfirmationDate") $ invoiceDetailsType_orderConfirmationDate x
            , maybe [] (schemaTypeToXML "AgreementIdentifier") $ invoiceDetailsType_agreementIdentifier x
            , maybe [] (schemaTypeToXML "AgreementIdentifierUrlText") $ invoiceDetailsType_agreementIdentifierUrlText x
            , maybe [] (schemaTypeToXML "AgreementTypeText") $ invoiceDetailsType_agreementTypeText x
            , maybe [] (schemaTypeToXML "AgreementTypeCode") $ invoiceDetailsType_agreementTypeCode x
            , maybe [] (schemaTypeToXML "AgreementDate") $ invoiceDetailsType_agreementDate x
            , maybe [] (schemaTypeToXML "NotificationIdentifier") $ invoiceDetailsType_notificationIdentifier x
            , maybe [] (schemaTypeToXML "NotificationDate") $ invoiceDetailsType_notificationDate x
            , maybe [] (schemaTypeToXML "RegistrationNumberIdentifier") $ invoiceDetailsType_registrationNumberIdentifier x
            , maybe [] (schemaTypeToXML "ControllerIdentifier") $ invoiceDetailsType_controllerIdentifier x
            , maybe [] (schemaTypeToXML "ControllerName") $ invoiceDetailsType_controllerName x
            , maybe [] (schemaTypeToXML "ControlDate") $ invoiceDetailsType_controlDate x
            , maybe [] (schemaTypeToXML "BuyerReferenceIdentifier") $ invoiceDetailsType_buyerReferenceIdentifier x
            , maybe [] (schemaTypeToXML "ProjectReferenceIdentifier") $ invoiceDetailsType_projectReferenceIdentifier x
            , concatMap (schemaTypeToXML "DefinitionDetails") $ invoiceDetailsType_definitionDetails x
            , maybe [] (schemaTypeToXML "RowsTotalVatExcludedAmount") $ invoiceDetailsType_rowsTotalVatExcludedAmount x
            , maybe [] (schemaTypeToXML "DiscountsTotalVatExcludedAmount") $ invoiceDetailsType_discountsTotalVatExcludedAmount x
            , maybe [] (schemaTypeToXML "ChargesTotalVatExcludedAmount") $ invoiceDetailsType_chargesTotalVatExcludedAmount x
            , maybe [] (schemaTypeToXML "InvoiceTotalVatExcludedAmount") $ invoiceDetailsType_invoiceTotalVatExcludedAmount x
            , maybe [] (schemaTypeToXML "InvoiceTotalVatAmount") $ invoiceDetailsType_invoiceTotalVatAmount x
            , maybe [] (schemaTypeToXML "InvoiceTotalVatAccountingAmount") $ invoiceDetailsType_invoiceTotalVatAccountingAmount x
            , schemaTypeToXML "InvoiceTotalVatIncludedAmount" $ invoiceDetailsType_invoiceTotalVatIncludedAmount x
            , maybe [] (schemaTypeToXML "InvoiceTotalRoundoffAmount") $ invoiceDetailsType_invoiceTotalRoundoffAmount x
            , maybe [] (schemaTypeToXML "InvoicePaidAmount") $ invoiceDetailsType_invoicePaidAmount x
            , maybe [] (schemaTypeToXML "ExchangeRate") $ invoiceDetailsType_exchangeRate x
            , maybe [] (schemaTypeToXML "OtherCurrencyAmountVatExcludedAmount") $ invoiceDetailsType_otherCurrencyAmountVatExcludedAmount x
            , maybe [] (schemaTypeToXML "OtherCurrencyAmountVatIncludedAmount") $ invoiceDetailsType_otherCurrencyAmountVatIncludedAmount x
            , maybe [] (schemaTypeToXML "CreditLimitAmount") $ invoiceDetailsType_creditLimitAmount x
            , maybe [] (schemaTypeToXML "CreditInterestPercent") $ invoiceDetailsType_creditInterestPercent x
            , maybe [] (schemaTypeToXML "OperationLimitAmount") $ invoiceDetailsType_operationLimitAmount x
            , maybe [] (schemaTypeToXML "MonthlyAmount") $ invoiceDetailsType_monthlyAmount x
            , maybe [] (schemaTypeToXML "ShortProposedAccountIdentifier") $ invoiceDetailsType_shortProposedAccountIdentifier x
            , maybe [] (schemaTypeToXML "NormalProposedAccountIdentifier") $ invoiceDetailsType_normalProposedAccountIdentifier x
            , maybe [] (schemaTypeToXML "ProposedAccountText") $ invoiceDetailsType_proposedAccountText x
            , maybe [] (schemaTypeToXML "AccountDimensionText") $ invoiceDetailsType_accountDimensionText x
            , maybe [] (schemaTypeToXML "SellerAccountText") $ invoiceDetailsType_sellerAccountText x
            , maybe [] (schemaTypeToXML "VatPoint") $ invoiceDetailsType_vatPoint x
            , concatMap (schemaTypeToXML "VatSpecificationDetails") $ invoiceDetailsType_vatSpecificationDetails x
            , concatMap (schemaTypeToXML "InvoiceFreeText") $ invoiceDetailsType_invoiceFreeText x
            , maybe [] (schemaTypeToXML "InvoiceVatFreeText") $ invoiceDetailsType_invoiceVatFreeText x
            , concatMap (schemaTypeToXML "PaymentTermsDetails") $ invoiceDetailsType_paymentTermsDetails x
            , concatMap (schemaTypeToXML "DiscountDetails") $ invoiceDetailsType_discountDetails x
            , concatMap (schemaTypeToXML "ChargeDetails") $ invoiceDetailsType_chargeDetails x
            , maybe [] (schemaTypeToXML "TenderReference") $ invoiceDetailsType_tenderReference x
            ]
 
data OriginalInvoiceReferenceType = OriginalInvoiceReferenceType
        { originalInvoiceReferenceType_invoiceNumber :: Maybe GenericStringType1_20
        , originalInvoiceReferenceType_invoiceDate :: Maybe Date
        }
        deriving (Eq,Show)
instance SchemaType OriginalInvoiceReferenceType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return OriginalInvoiceReferenceType
            `apply` optional (parseSchemaType "InvoiceNumber")
            `apply` optional (parseSchemaType "InvoiceDate")
    schemaTypeToXML s x@OriginalInvoiceReferenceType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "InvoiceNumber") $ originalInvoiceReferenceType_invoiceNumber x
            , maybe [] (schemaTypeToXML "InvoiceDate") $ originalInvoiceReferenceType_invoiceDate x
            ]
 
data PaymentCardInfoType = PaymentCardInfoType
        { paymentCardInfoType_primaryAccountNumber :: GenericStringType1_19
        , paymentCardInfoType_cardHolderName :: Maybe GenericStringType1_70
        }
        deriving (Eq,Show)
instance SchemaType PaymentCardInfoType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PaymentCardInfoType
            `apply` parseSchemaType "PrimaryAccountNumber"
            `apply` optional (parseSchemaType "CardHolderName")
    schemaTypeToXML s x@PaymentCardInfoType{} =
        toXMLElement s []
            [ schemaTypeToXML "PrimaryAccountNumber" $ paymentCardInfoType_primaryAccountNumber x
            , maybe [] (schemaTypeToXML "CardHolderName") $ paymentCardInfoType_cardHolderName x
            ]
 
data DirectDebitInfoType = DirectDebitInfoType
        { directDebitInfoType_mandateReference :: Maybe GenericStringType1_35
        , directDebitInfoType_creditorIdentifier :: Maybe GenericStringType1_35
        , directDebitInfoType_debitedAccountID :: Maybe EpiAccountIDType
        }
        deriving (Eq,Show)
instance SchemaType DirectDebitInfoType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DirectDebitInfoType
            `apply` optional (parseSchemaType "MandateReference")
            `apply` optional (parseSchemaType "CreditorIdentifier")
            `apply` optional (parseSchemaType "DebitedAccountID")
    schemaTypeToXML s x@DirectDebitInfoType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "MandateReference") $ directDebitInfoType_mandateReference x
            , maybe [] (schemaTypeToXML "CreditorIdentifier") $ directDebitInfoType_creditorIdentifier x
            , maybe [] (schemaTypeToXML "DebitedAccountID") $ directDebitInfoType_debitedAccountID x
            ]
 
data InvoiceRecipientCommunicationDetailsType = InvoiceRecipientCommunicationDetailsType
        { invoiceRecipientCommunicationDetailsType_invoiceRecipientPhoneNumberIdentifier :: Maybe GenericStringType0_35
        , invoiceRecipientCommunicationDetailsType_invoiceRecipientEmailaddressIdentifier :: Maybe GenericStringType0_70
        }
        deriving (Eq,Show)
instance SchemaType InvoiceRecipientCommunicationDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return InvoiceRecipientCommunicationDetailsType
            `apply` optional (parseSchemaType "InvoiceRecipientPhoneNumberIdentifier")
            `apply` optional (parseSchemaType "InvoiceRecipientEmailaddressIdentifier")
    schemaTypeToXML s x@InvoiceRecipientCommunicationDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "InvoiceRecipientPhoneNumberIdentifier") $ invoiceRecipientCommunicationDetailsType_invoiceRecipientPhoneNumberIdentifier x
            , maybe [] (schemaTypeToXML "InvoiceRecipientEmailaddressIdentifier") $ invoiceRecipientCommunicationDetailsType_invoiceRecipientEmailaddressIdentifier x
            ]
 
data InvoiceRecipientDetailsType = InvoiceRecipientDetailsType
        { invoiceRecipientDetailsType_invoiceRecipientAddress :: GenericStringType1_35
        , invoiceRecipientDetailsType_invoiceRecipientIntermediatorAddress :: GenericNMtokenType8_11
        }
        deriving (Eq,Show)
instance SchemaType InvoiceRecipientDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return InvoiceRecipientDetailsType
            `apply` parseSchemaType "InvoiceRecipientAddress"
            `apply` parseSchemaType "InvoiceRecipientIntermediatorAddress"
    schemaTypeToXML s x@InvoiceRecipientDetailsType{} =
        toXMLElement s []
            [ schemaTypeToXML "InvoiceRecipientAddress" $ invoiceRecipientDetailsType_invoiceRecipientAddress x
            , schemaTypeToXML "InvoiceRecipientIntermediatorAddress" $ invoiceRecipientDetailsType_invoiceRecipientIntermediatorAddress x
            ]
 
data InvoiceRecipientPartyDetailsType = InvoiceRecipientPartyDetailsType
        { invoiceRecipientPartyDetailsType_invoiceRecipientPartyIdentifier :: Maybe PartyLegalRegIdType
        , invoiceRecipientPartyDetailsType_invoiceRecipientOrganisationName :: [GenericStringType2_35]
        , invoiceRecipientPartyDetailsType_invoiceRecipientDepartment :: [GenericStringType0_35]
        , invoiceRecipientPartyDetailsType_invoiceRecipientOrganisationTaxCode :: Maybe GenericNMtokenType0_35
        , invoiceRecipientPartyDetailsType_invoiceRecipientCode :: Maybe PartyIdentifierType
        , invoiceRecipientPartyDetailsType_invoiceRecipientPostalAddressDetails :: Maybe InvoiceRecipientPostalAddressDetailsType
        }
        deriving (Eq,Show)
instance SchemaType InvoiceRecipientPartyDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return InvoiceRecipientPartyDetailsType
            `apply` optional (parseSchemaType "InvoiceRecipientPartyIdentifier")
            `apply` many1 (parseSchemaType "InvoiceRecipientOrganisationName")
            `apply` between (Occurs (Just 0) (Just 2))
                            (parseSchemaType "InvoiceRecipientDepartment")
            `apply` optional (parseSchemaType "InvoiceRecipientOrganisationTaxCode")
            `apply` optional (parseSchemaType "InvoiceRecipientCode")
            `apply` optional (parseSchemaType "InvoiceRecipientPostalAddressDetails")
    schemaTypeToXML s x@InvoiceRecipientPartyDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "InvoiceRecipientPartyIdentifier") $ invoiceRecipientPartyDetailsType_invoiceRecipientPartyIdentifier x
            , concatMap (schemaTypeToXML "InvoiceRecipientOrganisationName") $ invoiceRecipientPartyDetailsType_invoiceRecipientOrganisationName x
            , concatMap (schemaTypeToXML "InvoiceRecipientDepartment") $ invoiceRecipientPartyDetailsType_invoiceRecipientDepartment x
            , maybe [] (schemaTypeToXML "InvoiceRecipientOrganisationTaxCode") $ invoiceRecipientPartyDetailsType_invoiceRecipientOrganisationTaxCode x
            , maybe [] (schemaTypeToXML "InvoiceRecipientCode") $ invoiceRecipientPartyDetailsType_invoiceRecipientCode x
            , maybe [] (schemaTypeToXML "InvoiceRecipientPostalAddressDetails") $ invoiceRecipientPartyDetailsType_invoiceRecipientPostalAddressDetails x
            ]
 
data InvoiceRecipientPostalAddressDetailsType = InvoiceRecipientPostalAddressDetailsType
        { invoiceRecipientPostalAddressDetailsType_invoiceRecipientStreetName :: [GenericStringType2_35]
        , invoiceRecipientPostalAddressDetailsType_invoiceRecipientTownName :: GenericStringType2_35
        , invoiceRecipientPostalAddressDetailsType_invoiceRecipientPostCodeIdentifier :: GenericStringType2_35
        , invoiceRecipientPostalAddressDetailsType_invoiceRecipientCountrySubdivision :: Maybe GenericStringType2_35
        , invoiceRecipientPostalAddressDetailsType_countryCode :: Maybe CountryCodeType
        , invoiceRecipientPostalAddressDetailsType_countryName :: Maybe GenericStringType0_35
        , invoiceRecipientPostalAddressDetailsType_invoiceRecipientPostOfficeBoxIdentifier :: Maybe GenericStringType0_35
        }
        deriving (Eq,Show)
instance SchemaType InvoiceRecipientPostalAddressDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return InvoiceRecipientPostalAddressDetailsType
            `apply` between (Occurs Nothing (Just 3))
                            (parseSchemaType "InvoiceRecipientStreetName")
            `apply` parseSchemaType "InvoiceRecipientTownName"
            `apply` parseSchemaType "InvoiceRecipientPostCodeIdentifier"
            `apply` optional (parseSchemaType "InvoiceRecipientCountrySubdivision")
            `apply` optional (parseSchemaType "CountryCode")
            `apply` optional (parseSchemaType "CountryName")
            `apply` optional (parseSchemaType "InvoiceRecipientPostOfficeBoxIdentifier")
    schemaTypeToXML s x@InvoiceRecipientPostalAddressDetailsType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "InvoiceRecipientStreetName") $ invoiceRecipientPostalAddressDetailsType_invoiceRecipientStreetName x
            , schemaTypeToXML "InvoiceRecipientTownName" $ invoiceRecipientPostalAddressDetailsType_invoiceRecipientTownName x
            , schemaTypeToXML "InvoiceRecipientPostCodeIdentifier" $ invoiceRecipientPostalAddressDetailsType_invoiceRecipientPostCodeIdentifier x
            , maybe [] (schemaTypeToXML "InvoiceRecipientCountrySubdivision") $ invoiceRecipientPostalAddressDetailsType_invoiceRecipientCountrySubdivision x
            , maybe [] (schemaTypeToXML "CountryCode") $ invoiceRecipientPostalAddressDetailsType_countryCode x
            , maybe [] (schemaTypeToXML "CountryName") $ invoiceRecipientPostalAddressDetailsType_countryName x
            , maybe [] (schemaTypeToXML "InvoiceRecipientPostOfficeBoxIdentifier") $ invoiceRecipientPostalAddressDetailsType_invoiceRecipientPostOfficeBoxIdentifier x
            ]
 
 
data SubInvoiceRowType = SubInvoiceRowType
        { subInvoiceRowType_subIdentifier :: Maybe GenericStringType0_35
        , subInvoiceRowType_subRowPositionIdentifier :: Maybe GenericStringType0_35
        , subInvoiceRowType_subInvoicedObjectID :: Maybe InvoicedObjectIDType
        , subInvoiceRowType_subArticleIdentifier :: Maybe GenericStringType0_70
        , subInvoiceRowType_subArticleGroupIdentifier :: [ArticleGroupIdentifierType]
        , subInvoiceRowType_subArticleName :: Maybe GenericStringType0_100
        , subInvoiceRowType_subArticleDescription :: Maybe GenericStringType0_512
        , subInvoiceRowType_subArticleInfoUrlText :: Maybe GenericStringType0_512
        , subInvoiceRowType_subBuyerArticleIdentifier :: Maybe GenericStringType0_70
        , subInvoiceRowType_subEanCode :: Maybe EanCodeType
        , subInvoiceRowType_subRowRegistrationNumberIdentifier :: Maybe GenericStringType0_70
        , subInvoiceRowType_subSerialNumberIdentifier :: Maybe GenericStringType0_70
        , subInvoiceRowType_subRowActionCode :: Maybe GenericTokenType0_35
        , subInvoiceRowType_subRowDefinitionDetails :: [SubRowDefinitionDetails]
        , subInvoiceRowType_subOfferedQuantity :: [QuantityType0_14]
        , subInvoiceRowType_subDeliveredQuantity :: [QuantityType0_14]
        , subInvoiceRowType_subOrderedQuantity :: Maybe QuantityType0_14
        , subInvoiceRowType_subConfirmedQuantity :: Maybe QuantityType0_14
        , subInvoiceRowType_subPostDeliveredQuantity :: Maybe QuantityType0_14
        , subInvoiceRowType_subInvoicedQuantity :: [QuantityType0_14]
        , subInvoiceRowType_subCreditRequestedQuantity :: Maybe QuantityType0_14
        , subInvoiceRowType_subReturnedQuantity :: Maybe QuantityType0_14
        , subInvoiceRowType_subStartDate :: Maybe Date
        , subInvoiceRowType_subEndDate :: Maybe Date
        , subInvoiceRowType_subUnitPriceAmount :: Maybe UnitAmount
        , subInvoiceRowType_subUnitPriceDiscountAmount :: Maybe UnitAmount
        , subInvoiceRowType_subUnitPriceNetAmount :: Maybe UnitAmount
        , subInvoiceRowType_subUnitPriceVatIncludedAmount :: Maybe UnitAmount
        , subInvoiceRowType_subUnitPriceBaseQuantity :: Maybe QuantityType0_14
        , subInvoiceRowType_subRowIdentifier :: Maybe GenericStringType0_35
        , subInvoiceRowType_subRowIdentifierUrlText :: Maybe GenericStringType0_512
        , subInvoiceRowType_subRowIdentifierDate :: Maybe Date
        , subInvoiceRowType_subRowOrdererName :: Maybe GenericStringType0_35
        , subInvoiceRowType_subRowSalesPersonName :: Maybe GenericStringType0_35
        , subInvoiceRowType_subRowOrderConfirmationIdentifier :: Maybe GenericStringType0_70
        , subInvoiceRowType_subRowOrderConfirmationDate :: Maybe Date
        , subInvoiceRowType_subOriginalInvoiceNumber :: Maybe GenericStringType1_20
        , subInvoiceRowType_subOriginalInvoiceDate :: Maybe Date
        , subInvoiceRowType_subOriginalInvoiceReference :: [OriginalInvoiceReferenceType]
        , subInvoiceRowType_subRowDeliveryIdentifier :: Maybe GenericStringType0_35
        , subInvoiceRowType_subRowDeliveryIdentifierUrlText :: Maybe GenericStringType0_512
        , subInvoiceRowType_subRowDeliveryDate :: Maybe Date
        , subInvoiceRowType_subRowQuotationIdentifier :: Maybe GenericStringType0_35
        , subInvoiceRowType_subRowQuotationIdentifierUrlText :: Maybe GenericStringType0_512
        , subInvoiceRowType_subRowAgreementIdentifier :: Maybe GenericStringType0_70
        , subInvoiceRowType_subRowAgreementIdentifierUrlText :: Maybe GenericStringType0_512
        , subInvoiceRowType_subRowRequestOfQuotationIdentifier :: Maybe GenericStringType0_35
        , subInvoiceRowType_subRowRequestOfQuotationIdentifierUrlText :: Maybe GenericStringType0_512
        , subInvoiceRowType_subRowPriceListIdentifier :: Maybe GenericStringType0_35
        , subInvoiceRowType_subRowPriceListIdentifierUrlText :: Maybe GenericStringType0_512
        , subInvoiceRowType_subRowBuyerReferenceIdentifier :: Maybe GenericStringType0_70
        , subInvoiceRowType_subRowProjectReferenceIdentifier :: Maybe GenericStringType0_70
        , subInvoiceRowType_subRowOverDuePaymentDetails :: Maybe SubRowOverDuePaymentDetails
        , subInvoiceRowType_subRowAnyPartyDetails :: [SubRowAnyPartyDetails]
        , subInvoiceRowType_subRowDeliveryDetails :: Maybe SubRowDeliveryDetailsType
        , subInvoiceRowType_subRowShortProposedAccountIdentifier :: Maybe GenericStringType0_4
        , subInvoiceRowType_subRowNormalProposedAccountIdentifier :: Maybe GenericStringType0_4
        , subInvoiceRowType_subRowProposedAccountText :: Maybe GenericStringType0_35
        , subInvoiceRowType_subRowAccountDimensionText :: Maybe GenericStringType0_35
        , subInvoiceRowType_subRowSellerAccountText :: Maybe GenericStringType0_35
        , subInvoiceRowType_subRowFreeText :: [GenericStringType0_512]
        , subInvoiceRowType_subRowUsedQuantity :: Maybe QuantityType0_14
        , subInvoiceRowType_subRowPreviousMeterReadingDate :: Maybe Date
        , subInvoiceRowType_subRowLatestMeterReadingDate :: Maybe Date
        , subInvoiceRowType_subRowCalculatedQuantity :: Maybe QuantityType0_14
        , subInvoiceRowType_subRowAveragePriceAmount :: Maybe Amount
        , subInvoiceRowType_subRowDiscountPercent :: Maybe Percentage
        , subInvoiceRowType_subRowDiscountAmount :: Maybe Amount
        , subInvoiceRowType_subRowDiscountBaseAmount :: Maybe Amount
        , subInvoiceRowType_subRowDiscountTypeCode :: Maybe Untdid5189
        , subInvoiceRowType_subRowDiscountTypeText :: Maybe GenericStringType0_35
        , subInvoiceRowType_subRowProgressiveDiscountDetails :: [SubRowProgressiveDiscountDetails]
        , subInvoiceRowType_subRowChargeDetails :: [RowChargeDetailsType]
        , subInvoiceRowType_subRowVatRatePercent :: Maybe Percentage
        , subInvoiceRowType_subRowVatCode :: Maybe Untdid5305
        , subInvoiceRowType_subRowVatAmount :: Maybe Amount
        , subInvoiceRowType_subRowVatExcludedAmount :: Maybe Amount
        , subInvoiceRowType_subRowAmount :: Maybe Amount
        , subInvoiceRowType_subRowTransactionDetails :: Maybe TransactionDetailsType
        }
        deriving (Eq,Show)
instance SchemaType SubInvoiceRowType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return SubInvoiceRowType
            `apply` optional (parseSchemaType "SubIdentifier")
            `apply` optional (parseSchemaType "SubRowPositionIdentifier")
            `apply` optional (parseSchemaType "SubInvoicedObjectID")
            `apply` optional (parseSchemaType "SubArticleIdentifier")
            `apply` many (parseSchemaType "SubArticleGroupIdentifier")
            `apply` optional (parseSchemaType "SubArticleName")
            `apply` optional (parseSchemaType "SubArticleDescription")
            `apply` optional (parseSchemaType "SubArticleInfoUrlText")
            `apply` optional (parseSchemaType "SubBuyerArticleIdentifier")
            `apply` optional (parseSchemaType "SubEanCode")
            `apply` optional (parseSchemaType "SubRowRegistrationNumberIdentifier")
            `apply` optional (parseSchemaType "SubSerialNumberIdentifier")
            `apply` optional (parseSchemaType "SubRowActionCode")
            `apply` many (parseSchemaType "SubRowDefinitionDetails")
            `apply` many (parseSchemaType "SubOfferedQuantity")
            `apply` many (parseSchemaType "SubDeliveredQuantity")
            `apply` optional (parseSchemaType "SubOrderedQuantity")
            `apply` optional (parseSchemaType "SubConfirmedQuantity")
            `apply` optional (parseSchemaType "SubPostDeliveredQuantity")
            `apply` many (parseSchemaType "SubInvoicedQuantity")
            `apply` optional (parseSchemaType "SubCreditRequestedQuantity")
            `apply` optional (parseSchemaType "SubReturnedQuantity")
            `apply` optional (parseSchemaType "SubStartDate")
            `apply` optional (parseSchemaType "SubEndDate")
            `apply` optional (parseSchemaType "SubUnitPriceAmount")
            `apply` optional (parseSchemaType "SubUnitPriceDiscountAmount")
            `apply` optional (parseSchemaType "SubUnitPriceNetAmount")
            `apply` optional (parseSchemaType "SubUnitPriceVatIncludedAmount")
            `apply` optional (parseSchemaType "SubUnitPriceBaseQuantity")
            `apply` optional (parseSchemaType "SubRowIdentifier")
            `apply` optional (parseSchemaType "SubRowIdentifierUrlText")
            `apply` optional (parseSchemaType "SubRowIdentifierDate")
            `apply` optional (parseSchemaType "SubRowOrdererName")
            `apply` optional (parseSchemaType "SubRowSalesPersonName")
            `apply` optional (parseSchemaType "SubRowOrderConfirmationIdentifier")
            `apply` optional (parseSchemaType "SubRowOrderConfirmationDate")
            `apply` optional (parseSchemaType "SubOriginalInvoiceNumber")
            `apply` optional (parseSchemaType "SubOriginalInvoiceDate")
            `apply` many (parseSchemaType "SubOriginalInvoiceReference")
            `apply` optional (parseSchemaType "SubRowDeliveryIdentifier")
            `apply` optional (parseSchemaType "SubRowDeliveryIdentifierUrlText")
            `apply` optional (parseSchemaType "SubRowDeliveryDate")
            `apply` optional (parseSchemaType "SubRowQuotationIdentifier")
            `apply` optional (parseSchemaType "SubRowQuotationIdentifierUrlText")
            `apply` optional (parseSchemaType "SubRowAgreementIdentifier")
            `apply` optional (parseSchemaType "SubRowAgreementIdentifierUrlText")
            `apply` optional (parseSchemaType "SubRowRequestOfQuotationIdentifier")
            `apply` optional (parseSchemaType "SubRowRequestOfQuotationIdentifierUrlText")
            `apply` optional (parseSchemaType "SubRowPriceListIdentifier")
            `apply` optional (parseSchemaType "SubRowPriceListIdentifierUrlText")
            `apply` optional (parseSchemaType "SubRowBuyerReferenceIdentifier")
            `apply` optional (parseSchemaType "SubRowProjectReferenceIdentifier")
            `apply` optional (parseSchemaType "SubRowOverDuePaymentDetails")
            `apply` many (parseSchemaType "SubRowAnyPartyDetails")
            `apply` optional (parseSchemaType "SubRowDeliveryDetails")
            `apply` optional (parseSchemaType "SubRowShortProposedAccountIdentifier")
            `apply` optional (parseSchemaType "SubRowNormalProposedAccountIdentifier")
            `apply` optional (parseSchemaType "SubRowProposedAccountText")
            `apply` optional (parseSchemaType "SubRowAccountDimensionText")
            `apply` optional (parseSchemaType "SubRowSellerAccountText")
            `apply` many (parseSchemaType "SubRowFreeText")
            `apply` optional (parseSchemaType "SubRowUsedQuantity")
            `apply` optional (parseSchemaType "SubRowPreviousMeterReadingDate")
            `apply` optional (parseSchemaType "SubRowLatestMeterReadingDate")
            `apply` optional (parseSchemaType "SubRowCalculatedQuantity")
            `apply` optional (parseSchemaType "SubRowAveragePriceAmount")
            `apply` optional (parseSchemaType "SubRowDiscountPercent")
            `apply` optional (parseSchemaType "SubRowDiscountAmount")
            `apply` optional (parseSchemaType "SubRowDiscountBaseAmount")
            `apply` optional (parseSchemaType "SubRowDiscountTypeCode")
            `apply` optional (parseSchemaType "SubRowDiscountTypeText")
            `apply` many (parseSchemaType "SubRowProgressiveDiscountDetails")
            `apply` many (parseSchemaType "SubRowChargeDetails")
            `apply` optional (parseSchemaType "SubRowVatRatePercent")
            `apply` optional (parseSchemaType "SubRowVatCode")
            `apply` optional (parseSchemaType "SubRowVatAmount")
            `apply` optional (parseSchemaType "SubRowVatExcludedAmount")
            `apply` optional (parseSchemaType "SubRowAmount")
            `apply` optional (parseSchemaType "SubRowTransactionDetails")
    schemaTypeToXML s x@SubInvoiceRowType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "SubIdentifier") $ subInvoiceRowType_subIdentifier x
            , maybe [] (schemaTypeToXML "SubRowPositionIdentifier") $ subInvoiceRowType_subRowPositionIdentifier x
            , maybe [] (schemaTypeToXML "SubInvoicedObjectID") $ subInvoiceRowType_subInvoicedObjectID x
            , maybe [] (schemaTypeToXML "SubArticleIdentifier") $ subInvoiceRowType_subArticleIdentifier x
            , concatMap (schemaTypeToXML "SubArticleGroupIdentifier") $ subInvoiceRowType_subArticleGroupIdentifier x
            , maybe [] (schemaTypeToXML "SubArticleName") $ subInvoiceRowType_subArticleName x
            , maybe [] (schemaTypeToXML "SubArticleDescription") $ subInvoiceRowType_subArticleDescription x
            , maybe [] (schemaTypeToXML "SubArticleInfoUrlText") $ subInvoiceRowType_subArticleInfoUrlText x
            , maybe [] (schemaTypeToXML "SubBuyerArticleIdentifier") $ subInvoiceRowType_subBuyerArticleIdentifier x
            , maybe [] (schemaTypeToXML "SubEanCode") $ subInvoiceRowType_subEanCode x
            , maybe [] (schemaTypeToXML "SubRowRegistrationNumberIdentifier") $ subInvoiceRowType_subRowRegistrationNumberIdentifier x
            , maybe [] (schemaTypeToXML "SubSerialNumberIdentifier") $ subInvoiceRowType_subSerialNumberIdentifier x
            , maybe [] (schemaTypeToXML "SubRowActionCode") $ subInvoiceRowType_subRowActionCode x
            , concatMap (schemaTypeToXML "SubRowDefinitionDetails") $ subInvoiceRowType_subRowDefinitionDetails x
            , concatMap (schemaTypeToXML "SubOfferedQuantity") $ subInvoiceRowType_subOfferedQuantity x
            , concatMap (schemaTypeToXML "SubDeliveredQuantity") $ subInvoiceRowType_subDeliveredQuantity x
            , maybe [] (schemaTypeToXML "SubOrderedQuantity") $ subInvoiceRowType_subOrderedQuantity x
            , maybe [] (schemaTypeToXML "SubConfirmedQuantity") $ subInvoiceRowType_subConfirmedQuantity x
            , maybe [] (schemaTypeToXML "SubPostDeliveredQuantity") $ subInvoiceRowType_subPostDeliveredQuantity x
            , concatMap (schemaTypeToXML "SubInvoicedQuantity") $ subInvoiceRowType_subInvoicedQuantity x
            , maybe [] (schemaTypeToXML "SubCreditRequestedQuantity") $ subInvoiceRowType_subCreditRequestedQuantity x
            , maybe [] (schemaTypeToXML "SubReturnedQuantity") $ subInvoiceRowType_subReturnedQuantity x
            , maybe [] (schemaTypeToXML "SubStartDate") $ subInvoiceRowType_subStartDate x
            , maybe [] (schemaTypeToXML "SubEndDate") $ subInvoiceRowType_subEndDate x
            , maybe [] (schemaTypeToXML "SubUnitPriceAmount") $ subInvoiceRowType_subUnitPriceAmount x
            , maybe [] (schemaTypeToXML "SubUnitPriceDiscountAmount") $ subInvoiceRowType_subUnitPriceDiscountAmount x
            , maybe [] (schemaTypeToXML "SubUnitPriceNetAmount") $ subInvoiceRowType_subUnitPriceNetAmount x
            , maybe [] (schemaTypeToXML "SubUnitPriceVatIncludedAmount") $ subInvoiceRowType_subUnitPriceVatIncludedAmount x
            , maybe [] (schemaTypeToXML "SubUnitPriceBaseQuantity") $ subInvoiceRowType_subUnitPriceBaseQuantity x
            , maybe [] (schemaTypeToXML "SubRowIdentifier") $ subInvoiceRowType_subRowIdentifier x
            , maybe [] (schemaTypeToXML "SubRowIdentifierUrlText") $ subInvoiceRowType_subRowIdentifierUrlText x
            , maybe [] (schemaTypeToXML "SubRowIdentifierDate") $ subInvoiceRowType_subRowIdentifierDate x
            , maybe [] (schemaTypeToXML "SubRowOrdererName") $ subInvoiceRowType_subRowOrdererName x
            , maybe [] (schemaTypeToXML "SubRowSalesPersonName") $ subInvoiceRowType_subRowSalesPersonName x
            , maybe [] (schemaTypeToXML "SubRowOrderConfirmationIdentifier") $ subInvoiceRowType_subRowOrderConfirmationIdentifier x
            , maybe [] (schemaTypeToXML "SubRowOrderConfirmationDate") $ subInvoiceRowType_subRowOrderConfirmationDate x
            , maybe [] (schemaTypeToXML "SubOriginalInvoiceNumber") $ subInvoiceRowType_subOriginalInvoiceNumber x
            , maybe [] (schemaTypeToXML "SubOriginalInvoiceDate") $ subInvoiceRowType_subOriginalInvoiceDate x
            , concatMap (schemaTypeToXML "SubOriginalInvoiceReference") $ subInvoiceRowType_subOriginalInvoiceReference x
            , maybe [] (schemaTypeToXML "SubRowDeliveryIdentifier") $ subInvoiceRowType_subRowDeliveryIdentifier x
            , maybe [] (schemaTypeToXML "SubRowDeliveryIdentifierUrlText") $ subInvoiceRowType_subRowDeliveryIdentifierUrlText x
            , maybe [] (schemaTypeToXML "SubRowDeliveryDate") $ subInvoiceRowType_subRowDeliveryDate x
            , maybe [] (schemaTypeToXML "SubRowQuotationIdentifier") $ subInvoiceRowType_subRowQuotationIdentifier x
            , maybe [] (schemaTypeToXML "SubRowQuotationIdentifierUrlText") $ subInvoiceRowType_subRowQuotationIdentifierUrlText x
            , maybe [] (schemaTypeToXML "SubRowAgreementIdentifier") $ subInvoiceRowType_subRowAgreementIdentifier x
            , maybe [] (schemaTypeToXML "SubRowAgreementIdentifierUrlText") $ subInvoiceRowType_subRowAgreementIdentifierUrlText x
            , maybe [] (schemaTypeToXML "SubRowRequestOfQuotationIdentifier") $ subInvoiceRowType_subRowRequestOfQuotationIdentifier x
            , maybe [] (schemaTypeToXML "SubRowRequestOfQuotationIdentifierUrlText") $ subInvoiceRowType_subRowRequestOfQuotationIdentifierUrlText x
            , maybe [] (schemaTypeToXML "SubRowPriceListIdentifier") $ subInvoiceRowType_subRowPriceListIdentifier x
            , maybe [] (schemaTypeToXML "SubRowPriceListIdentifierUrlText") $ subInvoiceRowType_subRowPriceListIdentifierUrlText x
            , maybe [] (schemaTypeToXML "SubRowBuyerReferenceIdentifier") $ subInvoiceRowType_subRowBuyerReferenceIdentifier x
            , maybe [] (schemaTypeToXML "SubRowProjectReferenceIdentifier") $ subInvoiceRowType_subRowProjectReferenceIdentifier x
            , maybe [] (schemaTypeToXML "SubRowOverDuePaymentDetails") $ subInvoiceRowType_subRowOverDuePaymentDetails x
            , concatMap (schemaTypeToXML "SubRowAnyPartyDetails") $ subInvoiceRowType_subRowAnyPartyDetails x
            , maybe [] (schemaTypeToXML "SubRowDeliveryDetails") $ subInvoiceRowType_subRowDeliveryDetails x
            , maybe [] (schemaTypeToXML "SubRowShortProposedAccountIdentifier") $ subInvoiceRowType_subRowShortProposedAccountIdentifier x
            , maybe [] (schemaTypeToXML "SubRowNormalProposedAccountIdentifier") $ subInvoiceRowType_subRowNormalProposedAccountIdentifier x
            , maybe [] (schemaTypeToXML "SubRowProposedAccountText") $ subInvoiceRowType_subRowProposedAccountText x
            , maybe [] (schemaTypeToXML "SubRowAccountDimensionText") $ subInvoiceRowType_subRowAccountDimensionText x
            , maybe [] (schemaTypeToXML "SubRowSellerAccountText") $ subInvoiceRowType_subRowSellerAccountText x
            , concatMap (schemaTypeToXML "SubRowFreeText") $ subInvoiceRowType_subRowFreeText x
            , maybe [] (schemaTypeToXML "SubRowUsedQuantity") $ subInvoiceRowType_subRowUsedQuantity x
            , maybe [] (schemaTypeToXML "SubRowPreviousMeterReadingDate") $ subInvoiceRowType_subRowPreviousMeterReadingDate x
            , maybe [] (schemaTypeToXML "SubRowLatestMeterReadingDate") $ subInvoiceRowType_subRowLatestMeterReadingDate x
            , maybe [] (schemaTypeToXML "SubRowCalculatedQuantity") $ subInvoiceRowType_subRowCalculatedQuantity x
            , maybe [] (schemaTypeToXML "SubRowAveragePriceAmount") $ subInvoiceRowType_subRowAveragePriceAmount x
            , maybe [] (schemaTypeToXML "SubRowDiscountPercent") $ subInvoiceRowType_subRowDiscountPercent x
            , maybe [] (schemaTypeToXML "SubRowDiscountAmount") $ subInvoiceRowType_subRowDiscountAmount x
            , maybe [] (schemaTypeToXML "SubRowDiscountBaseAmount") $ subInvoiceRowType_subRowDiscountBaseAmount x
            , maybe [] (schemaTypeToXML "SubRowDiscountTypeCode") $ subInvoiceRowType_subRowDiscountTypeCode x
            , maybe [] (schemaTypeToXML "SubRowDiscountTypeText") $ subInvoiceRowType_subRowDiscountTypeText x
            , concatMap (schemaTypeToXML "SubRowProgressiveDiscountDetails") $ subInvoiceRowType_subRowProgressiveDiscountDetails x
            , concatMap (schemaTypeToXML "SubRowChargeDetails") $ subInvoiceRowType_subRowChargeDetails x
            , maybe [] (schemaTypeToXML "SubRowVatRatePercent") $ subInvoiceRowType_subRowVatRatePercent x
            , maybe [] (schemaTypeToXML "SubRowVatCode") $ subInvoiceRowType_subRowVatCode x
            , maybe [] (schemaTypeToXML "SubRowVatAmount") $ subInvoiceRowType_subRowVatAmount x
            , maybe [] (schemaTypeToXML "SubRowVatExcludedAmount") $ subInvoiceRowType_subRowVatExcludedAmount x
            , maybe [] (schemaTypeToXML "SubRowAmount") $ subInvoiceRowType_subRowAmount x
            , maybe [] (schemaTypeToXML "SubRowTransactionDetails") $ subInvoiceRowType_subRowTransactionDetails x
            ]
 
data SubRowDeliveryDetailsType = SubRowDeliveryDetailsType
        { subRowDeliveryDetailsType_subRowTerminalAddressText :: Maybe GenericStringType0_70
        , subRowDeliveryDetailsType_subRowWaybillIdentifier :: Maybe GenericStringType0_70
        , subRowDeliveryDetailsType_subRowWaybillTypeCode :: Maybe GenericNMtokenType0_35
        , subRowDeliveryDetailsType_subRowClearanceIdentifier :: Maybe GenericStringType0_70
        , subRowDeliveryDetailsType_subRowDeliveryNoteIdentifier :: Maybe GenericStringType0_70
        , subRowDeliveryDetailsType_subRowDelivererIdentifier :: Maybe GenericStringType0_35
        , subRowDeliveryDetailsType_subRowDelivererName :: [GenericStringType0_35]
        , subRowDeliveryDetailsType_subRowDelivererCountrySubdivision :: Maybe GenericStringType2_35
        , subRowDeliveryDetailsType_subRowDelivererCountryCode :: Maybe CountryCodeType
        , subRowDeliveryDetailsType_subRowDelivererCountryName :: Maybe GenericStringType0_35
        , subRowDeliveryDetailsType_subRowPlaceOfDischarge :: Maybe GenericStringType0_35
        , subRowDeliveryDetailsType_subRowFinalDestinationName :: [GenericStringType0_35]
        , subRowDeliveryDetailsType_subRowCustomsInfo :: Maybe CustomsInfoType
        , subRowDeliveryDetailsType_subRowManufacturerArticleIdentifier :: Maybe GenericStringType0_70
        , subRowDeliveryDetailsType_subRowManufacturerIdentifier :: Maybe GenericStringType0_35
        , subRowDeliveryDetailsType_subRowManufacturerName :: [GenericStringType0_35]
        , subRowDeliveryDetailsType_subRowManufacturerCountrySubdivision :: Maybe GenericStringType2_35
        , subRowDeliveryDetailsType_subRowManufacturerCountryCode :: Maybe CountryCodeType
        , subRowDeliveryDetailsType_subRowManufacturerCountryName :: Maybe GenericStringType0_35
        , subRowDeliveryDetailsType_subRowManufacturerOrderIdentifier :: Maybe GenericStringType0_70
        , subRowDeliveryDetailsType_subRowPackageDetails :: Maybe SubRowPackageDetails
        }
        deriving (Eq,Show)
instance SchemaType SubRowDeliveryDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return SubRowDeliveryDetailsType
            `apply` optional (parseSchemaType "SubRowTerminalAddressText")
            `apply` optional (parseSchemaType "SubRowWaybillIdentifier")
            `apply` optional (parseSchemaType "SubRowWaybillTypeCode")
            `apply` optional (parseSchemaType "SubRowClearanceIdentifier")
            `apply` optional (parseSchemaType "SubRowDeliveryNoteIdentifier")
            `apply` optional (parseSchemaType "SubRowDelivererIdentifier")
            `apply` between (Occurs (Just 0) (Just 3))
                            (parseSchemaType "SubRowDelivererName")
            `apply` optional (parseSchemaType "SubRowDelivererCountrySubdivision")
            `apply` optional (parseSchemaType "SubRowDelivererCountryCode")
            `apply` optional (parseSchemaType "SubRowDelivererCountryName")
            `apply` optional (parseSchemaType "SubRowPlaceOfDischarge")
            `apply` between (Occurs (Just 0) (Just 3))
                            (parseSchemaType "SubRowFinalDestinationName")
            `apply` optional (parseSchemaType "SubRowCustomsInfo")
            `apply` optional (parseSchemaType "SubRowManufacturerArticleIdentifier")
            `apply` optional (parseSchemaType "SubRowManufacturerIdentifier")
            `apply` between (Occurs (Just 0) (Just 3))
                            (parseSchemaType "SubRowManufacturerName")
            `apply` optional (parseSchemaType "SubRowManufacturerCountrySubdivision")
            `apply` optional (parseSchemaType "SubRowManufacturerCountryCode")
            `apply` optional (parseSchemaType "SubRowManufacturerCountryName")
            `apply` optional (parseSchemaType "SubRowManufacturerOrderIdentifier")
            `apply` optional (parseSchemaType "SubRowPackageDetails")
    schemaTypeToXML s x@SubRowDeliveryDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "SubRowTerminalAddressText") $ subRowDeliveryDetailsType_subRowTerminalAddressText x
            , maybe [] (schemaTypeToXML "SubRowWaybillIdentifier") $ subRowDeliveryDetailsType_subRowWaybillIdentifier x
            , maybe [] (schemaTypeToXML "SubRowWaybillTypeCode") $ subRowDeliveryDetailsType_subRowWaybillTypeCode x
            , maybe [] (schemaTypeToXML "SubRowClearanceIdentifier") $ subRowDeliveryDetailsType_subRowClearanceIdentifier x
            , maybe [] (schemaTypeToXML "SubRowDeliveryNoteIdentifier") $ subRowDeliveryDetailsType_subRowDeliveryNoteIdentifier x
            , maybe [] (schemaTypeToXML "SubRowDelivererIdentifier") $ subRowDeliveryDetailsType_subRowDelivererIdentifier x
            , concatMap (schemaTypeToXML "SubRowDelivererName") $ subRowDeliveryDetailsType_subRowDelivererName x
            , maybe [] (schemaTypeToXML "SubRowDelivererCountrySubdivision") $ subRowDeliveryDetailsType_subRowDelivererCountrySubdivision x
            , maybe [] (schemaTypeToXML "SubRowDelivererCountryCode") $ subRowDeliveryDetailsType_subRowDelivererCountryCode x
            , maybe [] (schemaTypeToXML "SubRowDelivererCountryName") $ subRowDeliveryDetailsType_subRowDelivererCountryName x
            , maybe [] (schemaTypeToXML "SubRowPlaceOfDischarge") $ subRowDeliveryDetailsType_subRowPlaceOfDischarge x
            , concatMap (schemaTypeToXML "SubRowFinalDestinationName") $ subRowDeliveryDetailsType_subRowFinalDestinationName x
            , maybe [] (schemaTypeToXML "SubRowCustomsInfo") $ subRowDeliveryDetailsType_subRowCustomsInfo x
            , maybe [] (schemaTypeToXML "SubRowManufacturerArticleIdentifier") $ subRowDeliveryDetailsType_subRowManufacturerArticleIdentifier x
            , maybe [] (schemaTypeToXML "SubRowManufacturerIdentifier") $ subRowDeliveryDetailsType_subRowManufacturerIdentifier x
            , concatMap (schemaTypeToXML "SubRowManufacturerName") $ subRowDeliveryDetailsType_subRowManufacturerName x
            , maybe [] (schemaTypeToXML "SubRowManufacturerCountrySubdivision") $ subRowDeliveryDetailsType_subRowManufacturerCountrySubdivision x
            , maybe [] (schemaTypeToXML "SubRowManufacturerCountryCode") $ subRowDeliveryDetailsType_subRowManufacturerCountryCode x
            , maybe [] (schemaTypeToXML "SubRowManufacturerCountryName") $ subRowDeliveryDetailsType_subRowManufacturerCountryName x
            , maybe [] (schemaTypeToXML "SubRowManufacturerOrderIdentifier") $ subRowDeliveryDetailsType_subRowManufacturerOrderIdentifier x
            , maybe [] (schemaTypeToXML "SubRowPackageDetails") $ subRowDeliveryDetailsType_subRowPackageDetails x
            ]
 
data InvoiceRowType = InvoiceRowType
        { invoiceRowType_choice0 :: (Maybe (OneOf2 ((Maybe (GenericStringType0_35)),(Maybe (InvoicedObjectIDType)),(Maybe (GenericStringType0_70)),[ArticleGroupIdentifierType],(Maybe (GenericStringType0_100)),(Maybe (GenericStringType0_512)),(Maybe (GenericStringType0_512)),(Maybe (GenericStringType0_70)),(Maybe (EanCodeType)),(Maybe (GenericStringType0_70)),(Maybe (GenericStringType0_70)),(Maybe (GenericTokenType0_35)),[RowDefinitionDetailsType],[QuantityType0_14],[QuantityType0_14],(Maybe (QuantityType0_14)),(Maybe (QuantityType0_14)),(Maybe (QuantityType0_14)),[QuantityType0_14],(Maybe (QuantityType0_14)),(Maybe (QuantityType0_14)),(Maybe (Date)),(Maybe (Date)),(Maybe (UnitAmountUN)),(Maybe (UnitAmountUN)),(Maybe (UnitAmountUN)),(Maybe (UnitAmountUN)),(Maybe (QuantityType0_14)),(Maybe (GenericStringType0_35)),(Maybe (GenericStringType0_512)),(Maybe (GenericStringType0_35)),(Maybe (Date)),(Maybe (GenericStringType0_35)),(Maybe (GenericStringType1_20)),(Maybe (Date)),[OriginalInvoiceReferenceType],(Maybe (GenericStringType0_35)),(Maybe (GenericStringType0_35)),(Maybe (GenericStringType0_70)),(Maybe (Date)),(Maybe (GenericStringType0_35)),(Maybe (GenericStringType0_512)),(Maybe (Date)),(Maybe (GenericStringType0_35)),(Maybe (GenericStringType0_512)),(Maybe (GenericStringType0_70)),(Maybe (GenericStringType0_512)),(Maybe (GenericStringType0_35)),(Maybe (GenericStringType0_512)),(Maybe (GenericStringType0_35)),(Maybe (GenericStringType0_512)),(Maybe (GenericStringType0_70)),(Maybe (GenericStringType0_70)),(Maybe (RowOverDuePaymentDetailsType)),[RowAnyPartyDetailsType],(Maybe (RowDeliveryDetailsType)),(Maybe (GenericNMtokenType0_4)),(Maybe (GenericNMtokenType0_4)),(Maybe (GenericStringType0_35)),(Maybe (GenericStringType0_35)),(Maybe (GenericStringType0_35)),[GenericStringType0_512],(Maybe (QuantityType0_14)),(Maybe (Date)),(Maybe (Date)),(Maybe (QuantityType0_14)),(Maybe (Amount)),(Maybe (Percentage)),(Maybe (Amount)),(Maybe (Amount)),(Maybe (Untdid5189)),(Maybe (GenericStringType0_35)),[RowProgressiveDiscountDetailsType],[RowChargeDetailsType],(Maybe (Percentage)),(Maybe (Untdid5305)),(Maybe (Amount)),(Maybe (Amount)),(Maybe (Amount)),(Maybe (TransactionDetailsType))) [SubInvoiceRowType]))
          -- ^ Choice between:
          --   
          --   (1) Sequence of:
          --   
          --     * RowSubIdentifier
          --   
          --     * InvoicedObjectID
          --   
          --     * ArticleIdentifier
          --   
          --     * ArticleGroupIdentifier
          --   
          --     * ArticleName
          --   
          --     * ArticleDescription
          --   
          --     * ArticleInfoUrlText
          --   
          --     * BuyerArticleIdentifier
          --   
          --     * EanCode
          --   
          --     * RowRegistrationNumberIdentifier
          --   
          --     * SerialNumberIdentifier
          --   
          --     * RowActionCode
          --   
          --     * RowDefinitionDetails
          --   
          --     * OfferedQuantity
          --   
          --     * DeliveredQuantity
          --   
          --     * OrderedQuantity
          --   
          --     * ConfirmedQuantity
          --   
          --     * PostDeliveredQuantity
          --   
          --     * InvoicedQuantity
          --   
          --     * CreditRequestedQuantity
          --   
          --     * ReturnedQuantity
          --   
          --     * StartDate
          --   
          --     * EndDate
          --   
          --     * UnitPriceAmount
          --   
          --     * UnitPriceDiscountAmount
          --   
          --     * UnitPriceNetAmount
          --   
          --     * UnitPriceVatIncludedAmount
          --   
          --     * UnitPriceBaseQuantity
          --   
          --     * RowIdentifier
          --   
          --     * RowIdentifierUrlText
          --   
          --     * RowOrderPositionIdentifier
          --   
          --     * RowIdentifierDate
          --   
          --     * RowPositionIdentifier
          --   
          --     * OriginalInvoiceNumber
          --   
          --     * OriginalInvoiceDate
          --   
          --     * OriginalInvoiceReference
          --   
          --     * RowOrdererName
          --   
          --     * RowSalesPersonName
          --   
          --     * RowOrderConfirmationIdentifier
          --   
          --     * RowOrderConfirmationDate
          --   
          --     * RowDeliveryIdentifier
          --   
          --     * RowDeliveryIdentifierUrlText
          --   
          --     * RowDeliveryDate
          --   
          --     * RowQuotationIdentifier
          --   
          --     * RowQuotationIdentifierUrlText
          --   
          --     * RowAgreementIdentifier
          --   
          --     * RowAgreementIdentifierUrlText
          --   
          --     * RowRequestOfQuotationIdentifier
          --   
          --     * RowRequestOfQuotationIdentifierUrlText
          --   
          --     * RowPriceListIdentifier
          --   
          --     * RowPriceListIdentifierUrlText
          --   
          --     * RowBuyerReferenceIdentifier
          --   
          --     * RowProjectReferenceIdentifier
          --   
          --     * RowOverDuePaymentDetails
          --   
          --     * RowAnyPartyDetails
          --   
          --     * RowDeliveryDetails
          --   
          --     * RowShortProposedAccountIdentifier
          --   
          --     * RowNormalProposedAccountIdentifier
          --   
          --     * RowProposedAccountText
          --   
          --     * RowAccountDimensionText
          --   
          --     * RowSellerAccountText
          --   
          --     * RowFreeText
          --   
          --     * RowUsedQuantity
          --   
          --     * RowPreviousMeterReadingDate
          --   
          --     * RowLatestMeterReadingDate
          --   
          --     * RowCalculatedQuantity
          --   
          --     * RowAveragePriceAmount
          --   
          --     * RowDiscountPercent
          --   
          --     * RowDiscountAmount
          --   
          --     * RowDiscountBaseAmount
          --   
          --     * RowDiscountTypeCode
          --   
          --     * RowDiscountTypeText
          --   
          --     * RowProgressiveDiscountDetails
          --   
          --     * RowChargeDetails
          --   
          --     * RowVatRatePercent
          --   
          --     * RowVatCode
          --   
          --     * RowVatAmount
          --   
          --     * RowVatExcludedAmount
          --   
          --     * RowAmount
          --   
          --     * RowTransactionDetails
          --   
          --   (2) SubInvoiceRow
        }
        deriving (Eq,Show)
instance SchemaType InvoiceRowType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return InvoiceRowType
            `apply` optional (oneOf' [ ("Maybe GenericStringType0_35 Maybe InvoicedObjectIDType Maybe GenericStringType0_70 [ArticleGroupIdentifierType] Maybe GenericStringType0_100 Maybe GenericStringType0_512 Maybe GenericStringType0_512 Maybe GenericStringType0_70 Maybe EanCodeType Maybe GenericStringType0_70 Maybe GenericStringType0_70 Maybe GenericTokenType0_35 [RowDefinitionDetailsType] [QuantityType0_14] [QuantityType0_14] Maybe QuantityType0_14 Maybe QuantityType0_14 Maybe QuantityType0_14 [QuantityType0_14] Maybe QuantityType0_14 Maybe QuantityType0_14 Maybe Date Maybe Date Maybe UnitAmountUN Maybe UnitAmountUN Maybe UnitAmountUN Maybe UnitAmountUN Maybe QuantityType0_14 Maybe GenericStringType0_35 Maybe GenericStringType0_512 Maybe GenericStringType0_35 Maybe Date Maybe GenericStringType0_35 Maybe GenericStringType1_20 Maybe Date [OriginalInvoiceReferenceType] Maybe GenericStringType0_35 Maybe GenericStringType0_35 Maybe GenericStringType0_70 Maybe Date Maybe GenericStringType0_35 Maybe GenericStringType0_512 Maybe Date Maybe GenericStringType0_35 Maybe GenericStringType0_512 Maybe GenericStringType0_70 Maybe GenericStringType0_512 Maybe GenericStringType0_35 Maybe GenericStringType0_512 Maybe GenericStringType0_35 Maybe GenericStringType0_512 Maybe GenericStringType0_70 Maybe GenericStringType0_70 Maybe RowOverDuePaymentDetailsType [RowAnyPartyDetailsType] Maybe RowDeliveryDetailsType Maybe GenericNMtokenType0_4 Maybe GenericNMtokenType0_4 Maybe GenericStringType0_35 Maybe GenericStringType0_35 Maybe GenericStringType0_35 [GenericStringType0_512] Maybe QuantityType0_14 Maybe Date Maybe Date Maybe QuantityType0_14 Maybe Amount Maybe Percentage Maybe Amount Maybe Amount Maybe Untdid5189 Maybe GenericStringType0_35 [RowProgressiveDiscountDetailsType] [RowChargeDetailsType] Maybe Percentage Maybe Untdid5305 Maybe Amount Maybe Amount Maybe Amount Maybe TransactionDetailsType", fmap OneOf2 (return (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) `apply` optional (parseSchemaType "RowSubIdentifier")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "InvoicedObjectID")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "ArticleIdentifier")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` many (parseSchemaType "ArticleGroupIdentifier")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "ArticleName")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "ArticleDescription")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "ArticleInfoUrlText")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "BuyerArticleIdentifier")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "EanCode")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowRegistrationNumberIdentifier")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "SerialNumberIdentifier")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowActionCode")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` many (parseSchemaType "RowDefinitionDetails")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` many (parseSchemaType "OfferedQuantity")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` many (parseSchemaType "DeliveredQuantity")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "OrderedQuantity")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "ConfirmedQuantity")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "PostDeliveredQuantity")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` many (parseSchemaType "InvoicedQuantity")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "CreditRequestedQuantity")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "ReturnedQuantity")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "StartDate")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "EndDate")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "UnitPriceAmount")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "UnitPriceDiscountAmount")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "UnitPriceNetAmount")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "UnitPriceVatIncludedAmount")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "UnitPriceBaseQuantity")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowIdentifier")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowIdentifierUrlText")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowOrderPositionIdentifier")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowIdentifierDate")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowPositionIdentifier")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "OriginalInvoiceNumber")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "OriginalInvoiceDate")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` many (parseSchemaType "OriginalInvoiceReference")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowOrdererName")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowSalesPersonName")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowOrderConfirmationIdentifier")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowOrderConfirmationDate")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowDeliveryIdentifier")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowDeliveryIdentifierUrlText")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowDeliveryDate")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowQuotationIdentifier")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowQuotationIdentifierUrlText")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowAgreementIdentifier")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowAgreementIdentifierUrlText")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowRequestOfQuotationIdentifier")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowRequestOfQuotationIdentifierUrlText")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowPriceListIdentifier")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowPriceListIdentifierUrlText")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowBuyerReferenceIdentifier")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowProjectReferenceIdentifier")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowOverDuePaymentDetails")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` many (parseSchemaType "RowAnyPartyDetails")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowDeliveryDetails")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowShortProposedAccountIdentifier")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowNormalProposedAccountIdentifier")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowProposedAccountText")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowAccountDimensionText")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowSellerAccountText")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` many (parseSchemaType "RowFreeText")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowUsedQuantity")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowPreviousMeterReadingDate")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowLatestMeterReadingDate")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowCalculatedQuantity")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowAveragePriceAmount")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowDiscountPercent")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowDiscountAmount")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowDiscountBaseAmount")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowDiscountTypeCode")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowDiscountTypeText")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` many (parseSchemaType "RowProgressiveDiscountDetails")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` many (parseSchemaType "RowChargeDetails")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowVatRatePercent")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowVatCode")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowVatAmount")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowVatExcludedAmount")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowAmount")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  `apply` optional (parseSchemaType "RowTransactionDetails")))
                                     , ("[SubInvoiceRowType]", fmap TwoOf2 (many1 (parseSchemaType "SubInvoiceRow")))
                                     ])
    schemaTypeToXML s x@InvoiceRowType{} =
        toXMLElement s []
            [ maybe [] (foldOneOf2  (\ (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) -> concat [ maybe [] (schemaTypeToXML "RowSubIdentifier") a
                                                                                                       , maybe [] (schemaTypeToXML "InvoicedObjectID") b
                                                                                                       , maybe [] (schemaTypeToXML "ArticleIdentifier") c
                                                                                                       , concatMap (schemaTypeToXML "ArticleGroupIdentifier") d
                                                                                                       , maybe [] (schemaTypeToXML "ArticleName") e
                                                                                                       , maybe [] (schemaTypeToXML "ArticleDescription") f
                                                                                                       , maybe [] (schemaTypeToXML "ArticleInfoUrlText") g
                                                                                                       , maybe [] (schemaTypeToXML "BuyerArticleIdentifier") h
                                                                                                       , maybe [] (schemaTypeToXML "EanCode") i
                                                                                                       , maybe [] (schemaTypeToXML "RowRegistrationNumberIdentifier") j
                                                                                                       , maybe [] (schemaTypeToXML "SerialNumberIdentifier") k
                                                                                                       , maybe [] (schemaTypeToXML "RowActionCode") l
                                                                                                       , concatMap (schemaTypeToXML "RowDefinitionDetails") m
                                                                                                       , concatMap (schemaTypeToXML "OfferedQuantity") n
                                                                                                       , concatMap (schemaTypeToXML "DeliveredQuantity") o
                                                                                                       , maybe [] (schemaTypeToXML "OrderedQuantity") p
                                                                                                       , maybe [] (schemaTypeToXML "ConfirmedQuantity") q
                                                                                                       , maybe [] (schemaTypeToXML "PostDeliveredQuantity") r
                                                                                                       , concatMap (schemaTypeToXML "InvoicedQuantity") s
                                                                                                       , maybe [] (schemaTypeToXML "CreditRequestedQuantity") t
                                                                                                       , maybe [] (schemaTypeToXML "ReturnedQuantity") u
                                                                                                       , maybe [] (schemaTypeToXML "StartDate") v
                                                                                                       , maybe [] (schemaTypeToXML "EndDate") w
                                                                                                       , maybe [] (schemaTypeToXML "UnitPriceAmount") x
                                                                                                       , maybe [] (schemaTypeToXML "UnitPriceDiscountAmount") y
                                                                                                       , maybe [] (schemaTypeToXML "UnitPriceNetAmount") z
                                                                                                       ])
                                    (concatMap (schemaTypeToXML "SubInvoiceRow"))
                                   ) $ invoiceRowType_choice0 x
            ]
 
data RowDefinitionDetailsType = RowDefinitionDetailsType
        { rowDefinitionDetailsType_rowDefinitionHeaderText :: RowDefinitionHeaderText
        , rowDefinitionDetailsType_rowDefinitionValue :: Maybe QuantityType0_70
        }
        deriving (Eq,Show)
instance SchemaType RowDefinitionDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return RowDefinitionDetailsType
            `apply` parseSchemaType "RowDefinitionHeaderText"
            `apply` optional (parseSchemaType "RowDefinitionValue")
    schemaTypeToXML s x@RowDefinitionDetailsType{} =
        toXMLElement s []
            [ schemaTypeToXML "RowDefinitionHeaderText" $ rowDefinitionDetailsType_rowDefinitionHeaderText x
            , maybe [] (schemaTypeToXML "RowDefinitionValue") $ rowDefinitionDetailsType_rowDefinitionValue x
            ]
 
data RowOverDuePaymentDetailsType = RowOverDuePaymentDetailsType
        { rowOverDuePaymentDetailsType_rowOriginalInvoiceIdentifier :: Maybe GenericStringType0_35
        , rowOverDuePaymentDetailsType_rowOriginalInvoiceDate :: Maybe Date
        , rowOverDuePaymentDetailsType_rowOriginalDueDate :: Maybe Date
        , rowOverDuePaymentDetailsType_rowOriginalInvoiceTotalAmount :: Maybe Amount
        , rowOverDuePaymentDetailsType_rowOriginalEpiRemittanceInfoIdentifier :: Maybe GenericStringType0_35
        , rowOverDuePaymentDetailsType_rowPaidVatExcludedAmount :: Maybe Amount
        , rowOverDuePaymentDetailsType_rowPaidVatIncludedAmount :: Maybe Amount
        , rowOverDuePaymentDetailsType_rowPaidDate :: Maybe Date
        , rowOverDuePaymentDetailsType_rowUnPaidVatExcludedAmount :: Maybe Amount
        , rowOverDuePaymentDetailsType_rowUnPaidVatIncludedAmount :: Maybe Amount
        , rowOverDuePaymentDetailsType_rowCollectionDate :: Maybe Date
        , rowOverDuePaymentDetailsType_rowCollectionQuantity :: Maybe QuantityType0_14
        , rowOverDuePaymentDetailsType_rowCollectionChargeAmount :: Maybe Amount
        , rowOverDuePaymentDetailsType_rowInterestRate :: Maybe Percentage
        , rowOverDuePaymentDetailsType_rowInterestStartDate :: Maybe Date
        , rowOverDuePaymentDetailsType_rowInterestEndDate :: Maybe Date
        , rowOverDuePaymentDetailsType_rowInterestPeriodText :: Maybe GenericStringType0_35
        , rowOverDuePaymentDetailsType_rowInterestDateNumber :: Maybe GenericNMtokenType0_14
        , rowOverDuePaymentDetailsType_rowInterestChargeAmount :: Maybe Amount
        , rowOverDuePaymentDetailsType_rowInterestChargeVatAmount :: Maybe Amount
        }
        deriving (Eq,Show)
instance SchemaType RowOverDuePaymentDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return RowOverDuePaymentDetailsType
            `apply` optional (parseSchemaType "RowOriginalInvoiceIdentifier")
            `apply` optional (parseSchemaType "RowOriginalInvoiceDate")
            `apply` optional (parseSchemaType "RowOriginalDueDate")
            `apply` optional (parseSchemaType "RowOriginalInvoiceTotalAmount")
            `apply` optional (parseSchemaType "RowOriginalEpiRemittanceInfoIdentifier")
            `apply` optional (parseSchemaType "RowPaidVatExcludedAmount")
            `apply` optional (parseSchemaType "RowPaidVatIncludedAmount")
            `apply` optional (parseSchemaType "RowPaidDate")
            `apply` optional (parseSchemaType "RowUnPaidVatExcludedAmount")
            `apply` optional (parseSchemaType "RowUnPaidVatIncludedAmount")
            `apply` optional (parseSchemaType "RowCollectionDate")
            `apply` optional (parseSchemaType "RowCollectionQuantity")
            `apply` optional (parseSchemaType "RowCollectionChargeAmount")
            `apply` optional (parseSchemaType "RowInterestRate")
            `apply` optional (parseSchemaType "RowInterestStartDate")
            `apply` optional (parseSchemaType "RowInterestEndDate")
            `apply` optional (parseSchemaType "RowInterestPeriodText")
            `apply` optional (parseSchemaType "RowInterestDateNumber")
            `apply` optional (parseSchemaType "RowInterestChargeAmount")
            `apply` optional (parseSchemaType "RowInterestChargeVatAmount")
    schemaTypeToXML s x@RowOverDuePaymentDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "RowOriginalInvoiceIdentifier") $ rowOverDuePaymentDetailsType_rowOriginalInvoiceIdentifier x
            , maybe [] (schemaTypeToXML "RowOriginalInvoiceDate") $ rowOverDuePaymentDetailsType_rowOriginalInvoiceDate x
            , maybe [] (schemaTypeToXML "RowOriginalDueDate") $ rowOverDuePaymentDetailsType_rowOriginalDueDate x
            , maybe [] (schemaTypeToXML "RowOriginalInvoiceTotalAmount") $ rowOverDuePaymentDetailsType_rowOriginalInvoiceTotalAmount x
            , maybe [] (schemaTypeToXML "RowOriginalEpiRemittanceInfoIdentifier") $ rowOverDuePaymentDetailsType_rowOriginalEpiRemittanceInfoIdentifier x
            , maybe [] (schemaTypeToXML "RowPaidVatExcludedAmount") $ rowOverDuePaymentDetailsType_rowPaidVatExcludedAmount x
            , maybe [] (schemaTypeToXML "RowPaidVatIncludedAmount") $ rowOverDuePaymentDetailsType_rowPaidVatIncludedAmount x
            , maybe [] (schemaTypeToXML "RowPaidDate") $ rowOverDuePaymentDetailsType_rowPaidDate x
            , maybe [] (schemaTypeToXML "RowUnPaidVatExcludedAmount") $ rowOverDuePaymentDetailsType_rowUnPaidVatExcludedAmount x
            , maybe [] (schemaTypeToXML "RowUnPaidVatIncludedAmount") $ rowOverDuePaymentDetailsType_rowUnPaidVatIncludedAmount x
            , maybe [] (schemaTypeToXML "RowCollectionDate") $ rowOverDuePaymentDetailsType_rowCollectionDate x
            , maybe [] (schemaTypeToXML "RowCollectionQuantity") $ rowOverDuePaymentDetailsType_rowCollectionQuantity x
            , maybe [] (schemaTypeToXML "RowCollectionChargeAmount") $ rowOverDuePaymentDetailsType_rowCollectionChargeAmount x
            , maybe [] (schemaTypeToXML "RowInterestRate") $ rowOverDuePaymentDetailsType_rowInterestRate x
            , maybe [] (schemaTypeToXML "RowInterestStartDate") $ rowOverDuePaymentDetailsType_rowInterestStartDate x
            , maybe [] (schemaTypeToXML "RowInterestEndDate") $ rowOverDuePaymentDetailsType_rowInterestEndDate x
            , maybe [] (schemaTypeToXML "RowInterestPeriodText") $ rowOverDuePaymentDetailsType_rowInterestPeriodText x
            , maybe [] (schemaTypeToXML "RowInterestDateNumber") $ rowOverDuePaymentDetailsType_rowInterestDateNumber x
            , maybe [] (schemaTypeToXML "RowInterestChargeAmount") $ rowOverDuePaymentDetailsType_rowInterestChargeAmount x
            , maybe [] (schemaTypeToXML "RowInterestChargeVatAmount") $ rowOverDuePaymentDetailsType_rowInterestChargeVatAmount x
            ]
 
data InvoiceSenderPartyDetailsType = InvoiceSenderPartyDetailsType
        { invoiceSenderPartyDetailsType_invoiceSenderPartyIdentifier :: Maybe PartyLegalRegIdType
        , invoiceSenderPartyDetailsType_invoiceSenderOrganisationName :: [GenericStringType2_35]
        , invoiceSenderPartyDetailsType_invoiceSenderOrganisationTaxCode :: Maybe GenericNMtokenType0_35
        , invoiceSenderPartyDetailsType_invoiceSenderCode :: Maybe PartyIdentifierType
        }
        deriving (Eq,Show)
instance SchemaType InvoiceSenderPartyDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return InvoiceSenderPartyDetailsType
            `apply` optional (parseSchemaType "InvoiceSenderPartyIdentifier")
            `apply` many1 (parseSchemaType "InvoiceSenderOrganisationName")
            `apply` optional (parseSchemaType "InvoiceSenderOrganisationTaxCode")
            `apply` optional (parseSchemaType "InvoiceSenderCode")
    schemaTypeToXML s x@InvoiceSenderPartyDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "InvoiceSenderPartyIdentifier") $ invoiceSenderPartyDetailsType_invoiceSenderPartyIdentifier x
            , concatMap (schemaTypeToXML "InvoiceSenderOrganisationName") $ invoiceSenderPartyDetailsType_invoiceSenderOrganisationName x
            , maybe [] (schemaTypeToXML "InvoiceSenderOrganisationTaxCode") $ invoiceSenderPartyDetailsType_invoiceSenderOrganisationTaxCode x
            , maybe [] (schemaTypeToXML "InvoiceSenderCode") $ invoiceSenderPartyDetailsType_invoiceSenderCode x
            ]
 
data RowAnyPartyDetailsType = RowAnyPartyDetailsType
        { rowAnyPartyDetailsType_rowAnyPartyText :: Anypartytexttype0_35
        , rowAnyPartyDetailsType_rowAnyPartyIdentifier :: Maybe PartyLegalRegIdType
        , rowAnyPartyDetailsType_rowAnyPartyOrganisationName :: [GenericStringType2_35]
        , rowAnyPartyDetailsType_rowAnyPartyOrganisationDepartment :: [GenericStringType0_35]
        , rowAnyPartyDetailsType_rowAnyPartyOrganisationTaxCode :: Maybe GenericStringType0_35
        , rowAnyPartyDetailsType_rowAnyPartyCode :: Maybe PartyIdentifierType
        , rowAnyPartyDetailsType_rowAnyPartyPostalAddressDetails :: Maybe RowAnyPartyPostalAddressDetails
        , rowAnyPartyDetailsType_rowAnyPartyOrganisationUnitNumber :: Maybe GenericStringType0_35
        , rowAnyPartyDetailsType_rowAnyPartySiteCode :: Maybe GenericStringType0_35
        }
        deriving (Eq,Show)
instance SchemaType RowAnyPartyDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return RowAnyPartyDetailsType
            `apply` parseSchemaType "RowAnyPartyText"
            `apply` optional (parseSchemaType "RowAnyPartyIdentifier")
            `apply` between (Occurs Nothing (Just 2))
                            (parseSchemaType "RowAnyPartyOrganisationName")
            `apply` between (Occurs (Just 0) (Just 2))
                            (parseSchemaType "RowAnyPartyOrganisationDepartment")
            `apply` optional (parseSchemaType "RowAnyPartyOrganisationTaxCode")
            `apply` optional (parseSchemaType "RowAnyPartyCode")
            `apply` optional (parseSchemaType "RowAnyPartyPostalAddressDetails")
            `apply` optional (parseSchemaType "RowAnyPartyOrganisationUnitNumber")
            `apply` optional (parseSchemaType "RowAnyPartySiteCode")
    schemaTypeToXML s x@RowAnyPartyDetailsType{} =
        toXMLElement s []
            [ schemaTypeToXML "RowAnyPartyText" $ rowAnyPartyDetailsType_rowAnyPartyText x
            , maybe [] (schemaTypeToXML "RowAnyPartyIdentifier") $ rowAnyPartyDetailsType_rowAnyPartyIdentifier x
            , concatMap (schemaTypeToXML "RowAnyPartyOrganisationName") $ rowAnyPartyDetailsType_rowAnyPartyOrganisationName x
            , concatMap (schemaTypeToXML "RowAnyPartyOrganisationDepartment") $ rowAnyPartyDetailsType_rowAnyPartyOrganisationDepartment x
            , maybe [] (schemaTypeToXML "RowAnyPartyOrganisationTaxCode") $ rowAnyPartyDetailsType_rowAnyPartyOrganisationTaxCode x
            , maybe [] (schemaTypeToXML "RowAnyPartyCode") $ rowAnyPartyDetailsType_rowAnyPartyCode x
            , maybe [] (schemaTypeToXML "RowAnyPartyPostalAddressDetails") $ rowAnyPartyDetailsType_rowAnyPartyPostalAddressDetails x
            , maybe [] (schemaTypeToXML "RowAnyPartyOrganisationUnitNumber") $ rowAnyPartyDetailsType_rowAnyPartyOrganisationUnitNumber x
            , maybe [] (schemaTypeToXML "RowAnyPartySiteCode") $ rowAnyPartyDetailsType_rowAnyPartySiteCode x
            ]
 
data RowProgressiveDiscountDetailsType = RowProgressiveDiscountDetailsType
        { rowProgressiveDiscountDetailsType_rowDiscountPercent :: Maybe Percentage
        , rowProgressiveDiscountDetailsType_rowDiscountAmount :: Maybe Amount
        , rowProgressiveDiscountDetailsType_rowDiscountBaseAmount :: Maybe Amount
        , rowProgressiveDiscountDetailsType_rowDiscountTypeCode :: Maybe Untdid5189
        , rowProgressiveDiscountDetailsType_rowDiscountTypeText :: Maybe GenericStringType0_35
        }
        deriving (Eq,Show)
instance SchemaType RowProgressiveDiscountDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return RowProgressiveDiscountDetailsType
            `apply` optional (parseSchemaType "RowDiscountPercent")
            `apply` optional (parseSchemaType "RowDiscountAmount")
            `apply` optional (parseSchemaType "RowDiscountBaseAmount")
            `apply` optional (parseSchemaType "RowDiscountTypeCode")
            `apply` optional (parseSchemaType "RowDiscountTypeText")
    schemaTypeToXML s x@RowProgressiveDiscountDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "RowDiscountPercent") $ rowProgressiveDiscountDetailsType_rowDiscountPercent x
            , maybe [] (schemaTypeToXML "RowDiscountAmount") $ rowProgressiveDiscountDetailsType_rowDiscountAmount x
            , maybe [] (schemaTypeToXML "RowDiscountBaseAmount") $ rowProgressiveDiscountDetailsType_rowDiscountBaseAmount x
            , maybe [] (schemaTypeToXML "RowDiscountTypeCode") $ rowProgressiveDiscountDetailsType_rowDiscountTypeCode x
            , maybe [] (schemaTypeToXML "RowDiscountTypeText") $ rowProgressiveDiscountDetailsType_rowDiscountTypeText x
            ]
 
newtype InvoiceTypeCodePatternFI = InvoiceTypeCodePatternFI Xs.NMTOKEN deriving (Eq,Show)
instance Restricts InvoiceTypeCodePatternFI Xs.NMTOKEN where
    restricts (InvoiceTypeCodePatternFI x) = x
instance SchemaType InvoiceTypeCodePatternFI where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (InvoiceTypeCodePatternFI x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType InvoiceTypeCodePatternFI where
    acceptingParser = fmap InvoiceTypeCodePatternFI acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern (REQ|QUO|ORD|ORC|INV|DEV|TES|INF|PRI|DEN|SEI|REC|RES|SDD)[0-9]{2})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (InvoiceTypeCodePatternFI x) = simpleTypeText x
 
data InvoiceTypeCodeTypeFI = InvoiceTypeCodeTypeFI InvoiceTypeCodePatternFI InvoiceTypeCodeTypeFIAttributes deriving (Eq,Show)
data InvoiceTypeCodeTypeFIAttributes = InvoiceTypeCodeTypeFIAttributes
    { invoiceTypeCodeTypeFIAttributes_codeListAgencyIdentifier :: Maybe Xsd.XsdString
    }
    deriving (Eq,Show)
instance SchemaType InvoiceTypeCodeTypeFI where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- optional $ getAttribute "CodeListAgencyIdentifier" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ InvoiceTypeCodeTypeFI v (InvoiceTypeCodeTypeFIAttributes a0)
    schemaTypeToXML s (InvoiceTypeCodeTypeFI bt at) =
        addXMLAttributes [ maybe [] (toXMLAttribute "CodeListAgencyIdentifier") $ invoiceTypeCodeTypeFIAttributes_codeListAgencyIdentifier at
                         ]
            $ schemaTypeToXML s bt
instance Extension InvoiceTypeCodeTypeFI InvoiceTypeCodePatternFI where
    supertype (InvoiceTypeCodeTypeFI s _) = s
 
data InvoiceClassificationType = InvoiceClassificationType
        { invoiceClassificationType_classificationCode :: Maybe GenericStringType1_10
        , invoiceClassificationType_classificationText :: Maybe GenericStringType1_70
        }
        deriving (Eq,Show)
instance SchemaType InvoiceClassificationType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return InvoiceClassificationType
            `apply` optional (parseSchemaType "ClassificationCode")
            `apply` optional (parseSchemaType "ClassificationText")
    schemaTypeToXML s x@InvoiceClassificationType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "ClassificationCode") $ invoiceClassificationType_classificationCode x
            , maybe [] (schemaTypeToXML "ClassificationText") $ invoiceClassificationType_classificationText x
            ]
 
data OriginCodeType
    = OriginCodeType_Original
    | OriginCodeType_Copy
    | OriginCodeType_Cancel
    deriving (Eq,Show,Enum)
instance SchemaType OriginCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType OriginCodeType where
    acceptingParser =  do literal "Original"; return OriginCodeType_Original
                      `onFail` do literal "Copy"; return OriginCodeType_Copy
                      `onFail` do literal "Cancel"; return OriginCodeType_Cancel
                      
    simpleTypeText OriginCodeType_Original = "Original"
    simpleTypeText OriginCodeType_Copy = "Copy"
    simpleTypeText OriginCodeType_Cancel = "Cancel"
 
data PartialPaymentDetailsType = PartialPaymentDetailsType
        { partialPaymentDetailsType_paidAmount :: Amount
        , partialPaymentDetailsType_paidVatExcludedAmount :: Maybe Amount
        , partialPaymentDetailsType_unPaidAmount :: Amount
        , partialPaymentDetailsType_unPaidVatExcludedAmount :: Maybe Amount
        , partialPaymentDetailsType_interestPercent :: Maybe Percentage
        , partialPaymentDetailsType_prosessingCostsAmount :: Maybe Amount
        , partialPaymentDetailsType_partialPaymentVatIncludedAmount :: [Amount]
        , partialPaymentDetailsType_partialPaymentVatExcludedAmount :: [Amount]
        , partialPaymentDetailsType_partialPaymentDueDate :: [Date]
        , partialPaymentDetailsType_partialPaymentReferenceIdentifier :: [GenericStringType2_35]
        }
        deriving (Eq,Show)
instance SchemaType PartialPaymentDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PartialPaymentDetailsType
            `apply` parseSchemaType "PaidAmount"
            `apply` optional (parseSchemaType "PaidVatExcludedAmount")
            `apply` parseSchemaType "UnPaidAmount"
            `apply` optional (parseSchemaType "UnPaidVatExcludedAmount")
            `apply` optional (parseSchemaType "InterestPercent")
            `apply` optional (parseSchemaType "ProsessingCostsAmount")
            `apply` many1 (parseSchemaType "PartialPaymentVatIncludedAmount")
            `apply` many1 (parseSchemaType "PartialPaymentVatExcludedAmount")
            `apply` many1 (parseSchemaType "PartialPaymentDueDate")
            `apply` many1 (parseSchemaType "PartialPaymentReferenceIdentifier")
    schemaTypeToXML s x@PartialPaymentDetailsType{} =
        toXMLElement s []
            [ schemaTypeToXML "PaidAmount" $ partialPaymentDetailsType_paidAmount x
            , maybe [] (schemaTypeToXML "PaidVatExcludedAmount") $ partialPaymentDetailsType_paidVatExcludedAmount x
            , schemaTypeToXML "UnPaidAmount" $ partialPaymentDetailsType_unPaidAmount x
            , maybe [] (schemaTypeToXML "UnPaidVatExcludedAmount") $ partialPaymentDetailsType_unPaidVatExcludedAmount x
            , maybe [] (schemaTypeToXML "InterestPercent") $ partialPaymentDetailsType_interestPercent x
            , maybe [] (schemaTypeToXML "ProsessingCostsAmount") $ partialPaymentDetailsType_prosessingCostsAmount x
            , concatMap (schemaTypeToXML "PartialPaymentVatIncludedAmount") $ partialPaymentDetailsType_partialPaymentVatIncludedAmount x
            , concatMap (schemaTypeToXML "PartialPaymentVatExcludedAmount") $ partialPaymentDetailsType_partialPaymentVatExcludedAmount x
            , concatMap (schemaTypeToXML "PartialPaymentDueDate") $ partialPaymentDetailsType_partialPaymentDueDate x
            , concatMap (schemaTypeToXML "PartialPaymentReferenceIdentifier") $ partialPaymentDetailsType_partialPaymentReferenceIdentifier x
            ]
 
data PaymentOverDueFineDetailsType = PaymentOverDueFineDetailsType
        { paymentOverDueFineDetailsType_paymentOverDueFineFreeText :: [GenericStringType0_70]
        , paymentOverDueFineDetailsType_paymentOverDueFinePercent :: Maybe Percentage
        , paymentOverDueFineDetailsType_paymentOverDueFixedAmount :: Maybe Amount
        }
        deriving (Eq,Show)
instance SchemaType PaymentOverDueFineDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PaymentOverDueFineDetailsType
            `apply` between (Occurs (Just 0) (Just 3))
                            (parseSchemaType "PaymentOverDueFineFreeText")
            `apply` optional (parseSchemaType "PaymentOverDueFinePercent")
            `apply` optional (parseSchemaType "PaymentOverDueFixedAmount")
    schemaTypeToXML s x@PaymentOverDueFineDetailsType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "PaymentOverDueFineFreeText") $ paymentOverDueFineDetailsType_paymentOverDueFineFreeText x
            , maybe [] (schemaTypeToXML "PaymentOverDueFinePercent") $ paymentOverDueFineDetailsType_paymentOverDueFinePercent x
            , maybe [] (schemaTypeToXML "PaymentOverDueFixedAmount") $ paymentOverDueFineDetailsType_paymentOverDueFixedAmount x
            ]
 
data PaymentStatusCodeType
    = PaymentStatusCodeType_PAID
    | PaymentStatusCodeType_NOTPAID
    | PaymentStatusCodeType_PARTLYPAID
    deriving (Eq,Show,Enum)
instance SchemaType PaymentStatusCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PaymentStatusCodeType where
    acceptingParser =  do literal "PAID"; return PaymentStatusCodeType_PAID
                      `onFail` do literal "NOTPAID"; return PaymentStatusCodeType_NOTPAID
                      `onFail` do literal "PARTLYPAID"; return PaymentStatusCodeType_PARTLYPAID
                      
    simpleTypeText PaymentStatusCodeType_PAID = "PAID"
    simpleTypeText PaymentStatusCodeType_NOTPAID = "NOTPAID"
    simpleTypeText PaymentStatusCodeType_PARTLYPAID = "PARTLYPAID"
 
data PaymentStatusDetailsType = PaymentStatusDetailsType
        { paymentStatusDetailsType_paymentStatusCode :: Maybe PaymentStatusCodeType
        , paymentStatusDetailsType_paymentMethodText :: Maybe GenericStringType0_35
        }
        deriving (Eq,Show)
instance SchemaType PaymentStatusDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PaymentStatusDetailsType
            `apply` optional (parseSchemaType "PaymentStatusCode")
            `apply` optional (parseSchemaType "PaymentMethodText")
    schemaTypeToXML s x@PaymentStatusDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "PaymentStatusCode") $ paymentStatusDetailsType_paymentStatusCode x
            , maybe [] (schemaTypeToXML "PaymentMethodText") $ paymentStatusDetailsType_paymentMethodText x
            ]
 
data PaymentTermsDetailsType = PaymentTermsDetailsType
        { paymentTermsDetailsType_paymentTermsFreeText :: [GenericStringType0_70]
        , paymentTermsDetailsType_freeText :: [HeaderValueType]
        , paymentTermsDetailsType_invoiceDueDate :: Maybe Date
        , paymentTermsDetailsType_cashDiscountDate :: Maybe Date
        , paymentTermsDetailsType_cashDiscountBaseAmount :: Maybe Amount
        , paymentTermsDetailsType_cashDiscountPercent :: Maybe Percentage
        , paymentTermsDetailsType_cashDiscountAmount :: Maybe Amount
        , paymentTermsDetailsType_cashDiscountExcludingVatAmount :: Maybe Amount
        , paymentTermsDetailsType_cashDiscountVatDetails :: [CashDiscountVatDetails]
        , paymentTermsDetailsType_reducedInvoiceVatIncludedAmount :: Maybe Amount
        , paymentTermsDetailsType_paymentOverDueFineDetails :: Maybe PaymentOverDueFineDetailsType
        }
        deriving (Eq,Show)
instance SchemaType PaymentTermsDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PaymentTermsDetailsType
            `apply` between (Occurs (Just 0) (Just 2))
                            (parseSchemaType "PaymentTermsFreeText")
            `apply` between (Occurs (Just 0) (Just 2))
                            (parseSchemaType "FreeText")
            `apply` optional (parseSchemaType "InvoiceDueDate")
            `apply` optional (parseSchemaType "CashDiscountDate")
            `apply` optional (parseSchemaType "CashDiscountBaseAmount")
            `apply` optional (parseSchemaType "CashDiscountPercent")
            `apply` optional (parseSchemaType "CashDiscountAmount")
            `apply` optional (parseSchemaType "CashDiscountExcludingVatAmount")
            `apply` many (parseSchemaType "CashDiscountVatDetails")
            `apply` optional (parseSchemaType "ReducedInvoiceVatIncludedAmount")
            `apply` optional (parseSchemaType "PaymentOverDueFineDetails")
    schemaTypeToXML s x@PaymentTermsDetailsType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "PaymentTermsFreeText") $ paymentTermsDetailsType_paymentTermsFreeText x
            , concatMap (schemaTypeToXML "FreeText") $ paymentTermsDetailsType_freeText x
            , maybe [] (schemaTypeToXML "InvoiceDueDate") $ paymentTermsDetailsType_invoiceDueDate x
            , maybe [] (schemaTypeToXML "CashDiscountDate") $ paymentTermsDetailsType_cashDiscountDate x
            , maybe [] (schemaTypeToXML "CashDiscountBaseAmount") $ paymentTermsDetailsType_cashDiscountBaseAmount x
            , maybe [] (schemaTypeToXML "CashDiscountPercent") $ paymentTermsDetailsType_cashDiscountPercent x
            , maybe [] (schemaTypeToXML "CashDiscountAmount") $ paymentTermsDetailsType_cashDiscountAmount x
            , maybe [] (schemaTypeToXML "CashDiscountExcludingVatAmount") $ paymentTermsDetailsType_cashDiscountExcludingVatAmount x
            , concatMap (schemaTypeToXML "CashDiscountVatDetails") $ paymentTermsDetailsType_cashDiscountVatDetails x
            , maybe [] (schemaTypeToXML "ReducedInvoiceVatIncludedAmount") $ paymentTermsDetailsType_reducedInvoiceVatIncludedAmount x
            , maybe [] (schemaTypeToXML "PaymentOverDueFineDetails") $ paymentTermsDetailsType_paymentOverDueFineDetails x
            ]
 
data HeaderValueType = HeaderValueType
        { headerValueType_header :: Maybe GenericStringType1_35
        , headerValueType_value :: [GenericStringType1_70]
        }
        deriving (Eq,Show)
instance SchemaType HeaderValueType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return HeaderValueType
            `apply` optional (parseSchemaType "Header")
            `apply` between (Occurs (Just 0) (Just 2))
                            (parseSchemaType "Value")
    schemaTypeToXML s x@HeaderValueType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "Header") $ headerValueType_header x
            , concatMap (schemaTypeToXML "Value") $ headerValueType_value x
            ]
 
data RowDeliveryDetailsType = RowDeliveryDetailsType
        { rowDeliveryDetailsType_rowTerminalAddressText :: Maybe GenericStringType0_70
        , rowDeliveryDetailsType_rowWaybillIdentifier :: Maybe GenericStringType0_70
        , rowDeliveryDetailsType_rowWaybillTypeCode :: Maybe GenericStringType0_35
        , rowDeliveryDetailsType_rowClearanceIdentifier :: Maybe GenericStringType0_70
        , rowDeliveryDetailsType_rowDeliveryNoteIdentifier :: Maybe GenericStringType0_70
        , rowDeliveryDetailsType_rowDelivererIdentifier :: Maybe GenericStringType0_35
        , rowDeliveryDetailsType_rowDelivererName :: [GenericStringType0_35]
        , rowDeliveryDetailsType_rowDelivererCountrySubdivision :: Maybe GenericStringType2_35
        , rowDeliveryDetailsType_rowDelivererCountryCode :: Maybe CountryCodeType
        , rowDeliveryDetailsType_rowDelivererCountryName :: Maybe GenericStringType0_35
        , rowDeliveryDetailsType_rowModeOfTransportIdentifier :: Maybe GenericStringType0_70
        , rowDeliveryDetailsType_rowCarrierName :: Maybe GenericStringType0_35
        , rowDeliveryDetailsType_rowVesselName :: Maybe GenericStringType0_35
        , rowDeliveryDetailsType_rowLocationIdentifier :: Maybe GenericStringType0_70
        , rowDeliveryDetailsType_rowTransportInformationDate :: Maybe Date
        , rowDeliveryDetailsType_rowCountryOfOrigin :: Maybe GenericStringType0_35
        , rowDeliveryDetailsType_rowCountryOfDestinationName :: Maybe GenericStringType0_35
        , rowDeliveryDetailsType_rowDestinationCountryCode :: Maybe CountryCodeType
        , rowDeliveryDetailsType_rowPlaceOfDischarge :: Maybe GenericStringType0_35
        , rowDeliveryDetailsType_rowFinalDestinationName :: [GenericStringType0_35]
        , rowDeliveryDetailsType_rowCustomsInfo :: Maybe CustomsInfoType
        , rowDeliveryDetailsType_rowManufacturerArticleIdentifier :: Maybe GenericStringType0_70
        , rowDeliveryDetailsType_rowManufacturerIdentifier :: Maybe GenericStringType0_35
        , rowDeliveryDetailsType_rowManufacturerName :: [GenericStringType0_35]
        , rowDeliveryDetailsType_rowManufacturerCountrySubdivision :: Maybe GenericStringType2_35
        , rowDeliveryDetailsType_rowManufacturerCountryCode :: Maybe CountryCodeType
        , rowDeliveryDetailsType_rowManufacturerCountryName :: Maybe GenericStringType0_35
        , rowDeliveryDetailsType_rowManufacturerOrderIdentifier :: Maybe GenericStringType0_70
        , rowDeliveryDetailsType_rowPackageDetails :: Maybe RowPackageDetails
        }
        deriving (Eq,Show)
instance SchemaType RowDeliveryDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return RowDeliveryDetailsType
            `apply` optional (parseSchemaType "RowTerminalAddressText")
            `apply` optional (parseSchemaType "RowWaybillIdentifier")
            `apply` optional (parseSchemaType "RowWaybillTypeCode")
            `apply` optional (parseSchemaType "RowClearanceIdentifier")
            `apply` optional (parseSchemaType "RowDeliveryNoteIdentifier")
            `apply` optional (parseSchemaType "RowDelivererIdentifier")
            `apply` between (Occurs (Just 0) (Just 3))
                            (parseSchemaType "RowDelivererName")
            `apply` optional (parseSchemaType "RowDelivererCountrySubdivision")
            `apply` optional (parseSchemaType "RowDelivererCountryCode")
            `apply` optional (parseSchemaType "RowDelivererCountryName")
            `apply` optional (parseSchemaType "RowModeOfTransportIdentifier")
            `apply` optional (parseSchemaType "RowCarrierName")
            `apply` optional (parseSchemaType "RowVesselName")
            `apply` optional (parseSchemaType "RowLocationIdentifier")
            `apply` optional (parseSchemaType "RowTransportInformationDate")
            `apply` optional (parseSchemaType "RowCountryOfOrigin")
            `apply` optional (parseSchemaType "RowCountryOfDestinationName")
            `apply` optional (parseSchemaType "RowDestinationCountryCode")
            `apply` optional (parseSchemaType "RowPlaceOfDischarge")
            `apply` between (Occurs (Just 0) (Just 3))
                            (parseSchemaType "RowFinalDestinationName")
            `apply` optional (parseSchemaType "RowCustomsInfo")
            `apply` optional (parseSchemaType "RowManufacturerArticleIdentifier")
            `apply` optional (parseSchemaType "RowManufacturerIdentifier")
            `apply` between (Occurs (Just 0) (Just 3))
                            (parseSchemaType "RowManufacturerName")
            `apply` optional (parseSchemaType "RowManufacturerCountrySubdivision")
            `apply` optional (parseSchemaType "RowManufacturerCountryCode")
            `apply` optional (parseSchemaType "RowManufacturerCountryName")
            `apply` optional (parseSchemaType "RowManufacturerOrderIdentifier")
            `apply` optional (parseSchemaType "RowPackageDetails")
    schemaTypeToXML s x@RowDeliveryDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "RowTerminalAddressText") $ rowDeliveryDetailsType_rowTerminalAddressText x
            , maybe [] (schemaTypeToXML "RowWaybillIdentifier") $ rowDeliveryDetailsType_rowWaybillIdentifier x
            , maybe [] (schemaTypeToXML "RowWaybillTypeCode") $ rowDeliveryDetailsType_rowWaybillTypeCode x
            , maybe [] (schemaTypeToXML "RowClearanceIdentifier") $ rowDeliveryDetailsType_rowClearanceIdentifier x
            , maybe [] (schemaTypeToXML "RowDeliveryNoteIdentifier") $ rowDeliveryDetailsType_rowDeliveryNoteIdentifier x
            , maybe [] (schemaTypeToXML "RowDelivererIdentifier") $ rowDeliveryDetailsType_rowDelivererIdentifier x
            , concatMap (schemaTypeToXML "RowDelivererName") $ rowDeliveryDetailsType_rowDelivererName x
            , maybe [] (schemaTypeToXML "RowDelivererCountrySubdivision") $ rowDeliveryDetailsType_rowDelivererCountrySubdivision x
            , maybe [] (schemaTypeToXML "RowDelivererCountryCode") $ rowDeliveryDetailsType_rowDelivererCountryCode x
            , maybe [] (schemaTypeToXML "RowDelivererCountryName") $ rowDeliveryDetailsType_rowDelivererCountryName x
            , maybe [] (schemaTypeToXML "RowModeOfTransportIdentifier") $ rowDeliveryDetailsType_rowModeOfTransportIdentifier x
            , maybe [] (schemaTypeToXML "RowCarrierName") $ rowDeliveryDetailsType_rowCarrierName x
            , maybe [] (schemaTypeToXML "RowVesselName") $ rowDeliveryDetailsType_rowVesselName x
            , maybe [] (schemaTypeToXML "RowLocationIdentifier") $ rowDeliveryDetailsType_rowLocationIdentifier x
            , maybe [] (schemaTypeToXML "RowTransportInformationDate") $ rowDeliveryDetailsType_rowTransportInformationDate x
            , maybe [] (schemaTypeToXML "RowCountryOfOrigin") $ rowDeliveryDetailsType_rowCountryOfOrigin x
            , maybe [] (schemaTypeToXML "RowCountryOfDestinationName") $ rowDeliveryDetailsType_rowCountryOfDestinationName x
            , maybe [] (schemaTypeToXML "RowDestinationCountryCode") $ rowDeliveryDetailsType_rowDestinationCountryCode x
            , maybe [] (schemaTypeToXML "RowPlaceOfDischarge") $ rowDeliveryDetailsType_rowPlaceOfDischarge x
            , concatMap (schemaTypeToXML "RowFinalDestinationName") $ rowDeliveryDetailsType_rowFinalDestinationName x
            , maybe [] (schemaTypeToXML "RowCustomsInfo") $ rowDeliveryDetailsType_rowCustomsInfo x
            , maybe [] (schemaTypeToXML "RowManufacturerArticleIdentifier") $ rowDeliveryDetailsType_rowManufacturerArticleIdentifier x
            , maybe [] (schemaTypeToXML "RowManufacturerIdentifier") $ rowDeliveryDetailsType_rowManufacturerIdentifier x
            , concatMap (schemaTypeToXML "RowManufacturerName") $ rowDeliveryDetailsType_rowManufacturerName x
            , maybe [] (schemaTypeToXML "RowManufacturerCountrySubdivision") $ rowDeliveryDetailsType_rowManufacturerCountrySubdivision x
            , maybe [] (schemaTypeToXML "RowManufacturerCountryCode") $ rowDeliveryDetailsType_rowManufacturerCountryCode x
            , maybe [] (schemaTypeToXML "RowManufacturerCountryName") $ rowDeliveryDetailsType_rowManufacturerCountryName x
            , maybe [] (schemaTypeToXML "RowManufacturerOrderIdentifier") $ rowDeliveryDetailsType_rowManufacturerOrderIdentifier x
            , maybe [] (schemaTypeToXML "RowPackageDetails") $ rowDeliveryDetailsType_rowPackageDetails x
            ]
 
data SellerAccountDetailsType = SellerAccountDetailsType
        { sellerAccountDetailsType_sellerAccountID :: SellerAccountIDType
        , sellerAccountDetailsType_sellerBic :: SellerBicType
        , sellerAccountDetailsType_sellerAccountName :: Maybe GenericStringType1_70
        }
        deriving (Eq,Show)
instance SchemaType SellerAccountDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return SellerAccountDetailsType
            `apply` parseSchemaType "SellerAccountID"
            `apply` parseSchemaType "SellerBic"
            `apply` optional (parseSchemaType "SellerAccountName")
    schemaTypeToXML s x@SellerAccountDetailsType{} =
        toXMLElement s []
            [ schemaTypeToXML "SellerAccountID" $ sellerAccountDetailsType_sellerAccountID x
            , schemaTypeToXML "SellerBic" $ sellerAccountDetailsType_sellerBic x
            , maybe [] (schemaTypeToXML "SellerAccountName") $ sellerAccountDetailsType_sellerAccountName x
            ]
 
data SellerAccountIDType = SellerAccountIDType GenericNMtokenType2_35 SellerAccountIDTypeAttributes deriving (Eq,Show)
data SellerAccountIDTypeAttributes = SellerAccountIDTypeAttributes
    { sellerAccountIDTypeAttributes_identificationSchemeName :: Xsd.XsdString
    }
    deriving (Eq,Show)
instance SchemaType SellerAccountIDType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- getAttribute "IdentificationSchemeName" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ SellerAccountIDType v (SellerAccountIDTypeAttributes a0)
    schemaTypeToXML s (SellerAccountIDType bt at) =
        addXMLAttributes [ toXMLAttribute "IdentificationSchemeName" $ sellerAccountIDTypeAttributes_identificationSchemeName at
                         ]
            $ schemaTypeToXML s bt
instance Extension SellerAccountIDType GenericNMtokenType2_35 where
    supertype (SellerAccountIDType s _) = s
 
data SellerBicType = SellerBicType GenericNMtokenType8_11 SellerBicTypeAttributes deriving (Eq,Show)
data SellerBicTypeAttributes = SellerBicTypeAttributes
    { sellerBicTypeAttributes_identificationSchemeName :: Xsd.XsdString
    }
    deriving (Eq,Show)
instance SchemaType SellerBicType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- getAttribute "IdentificationSchemeName" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ SellerBicType v (SellerBicTypeAttributes a0)
    schemaTypeToXML s (SellerBicType bt at) =
        addXMLAttributes [ toXMLAttribute "IdentificationSchemeName" $ sellerBicTypeAttributes_identificationSchemeName at
                         ]
            $ schemaTypeToXML s bt
instance Extension SellerBicType GenericNMtokenType8_11 where
    supertype (SellerBicType s _) = s
 
data SellerCommunicationDetailsType = SellerCommunicationDetailsType
        { sellerCommunicationDetailsType_sellerPhoneNumberIdentifier :: Maybe GenericStringType0_35
        , sellerCommunicationDetailsType_sellerEmailaddressIdentifier :: Maybe GenericStringType0_70
        }
        deriving (Eq,Show)
instance SchemaType SellerCommunicationDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return SellerCommunicationDetailsType
            `apply` optional (parseSchemaType "SellerPhoneNumberIdentifier")
            `apply` optional (parseSchemaType "SellerEmailaddressIdentifier")
    schemaTypeToXML s x@SellerCommunicationDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "SellerPhoneNumberIdentifier") $ sellerCommunicationDetailsType_sellerPhoneNumberIdentifier x
            , maybe [] (schemaTypeToXML "SellerEmailaddressIdentifier") $ sellerCommunicationDetailsType_sellerEmailaddressIdentifier x
            ]
 
data SellerInformationDetailsType = SellerInformationDetailsType
        { sellerInformationDetailsType_sellerOfficialPostalAddressDetails :: Maybe SellerOfficialPostalAddressDetails
        , sellerInformationDetailsType_sellerHomeTownName :: Maybe GenericStringType0_35
        , sellerInformationDetailsType_sellerVatRegistrationText :: Maybe GenericStringType0_35
        , sellerInformationDetailsType_sellerVatRegistrationDate :: Maybe Date
        , sellerInformationDetailsType_sellerTaxRegistrationText :: Maybe GenericStringType0_35
        , sellerInformationDetailsType_sellerAdditionalLegalInfo :: Maybe GenericStringType0_512
        , sellerInformationDetailsType_sellerPhoneNumber :: Maybe GenericStringType0_35
        , sellerInformationDetailsType_sellerFaxNumber :: Maybe GenericStringType0_35
        , sellerInformationDetailsType_sellerCommonEmailaddressIdentifier :: Maybe GenericStringType0_70
        , sellerInformationDetailsType_sellerWebaddressIdentifier :: Maybe GenericStringType0_70
        , sellerInformationDetailsType_sellerFreeText :: Maybe GenericStringType0_512
        , sellerInformationDetailsType_sellerAccountDetails :: [SellerAccountDetailsType]
        , sellerInformationDetailsType_invoiceRecipientDetails :: [InvoiceRecipientDetailsType]
        }
        deriving (Eq,Show)
instance SchemaType SellerInformationDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return SellerInformationDetailsType
            `apply` optional (parseSchemaType "SellerOfficialPostalAddressDetails")
            `apply` optional (parseSchemaType "SellerHomeTownName")
            `apply` optional (parseSchemaType "SellerVatRegistrationText")
            `apply` optional (parseSchemaType "SellerVatRegistrationDate")
            `apply` optional (parseSchemaType "SellerTaxRegistrationText")
            `apply` optional (parseSchemaType "SellerAdditionalLegalInfo")
            `apply` optional (parseSchemaType "SellerPhoneNumber")
            `apply` optional (parseSchemaType "SellerFaxNumber")
            `apply` optional (parseSchemaType "SellerCommonEmailaddressIdentifier")
            `apply` optional (parseSchemaType "SellerWebaddressIdentifier")
            `apply` optional (parseSchemaType "SellerFreeText")
            `apply` many (parseSchemaType "SellerAccountDetails")
            `apply` many (parseSchemaType "InvoiceRecipientDetails")
    schemaTypeToXML s x@SellerInformationDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "SellerOfficialPostalAddressDetails") $ sellerInformationDetailsType_sellerOfficialPostalAddressDetails x
            , maybe [] (schemaTypeToXML "SellerHomeTownName") $ sellerInformationDetailsType_sellerHomeTownName x
            , maybe [] (schemaTypeToXML "SellerVatRegistrationText") $ sellerInformationDetailsType_sellerVatRegistrationText x
            , maybe [] (schemaTypeToXML "SellerVatRegistrationDate") $ sellerInformationDetailsType_sellerVatRegistrationDate x
            , maybe [] (schemaTypeToXML "SellerTaxRegistrationText") $ sellerInformationDetailsType_sellerTaxRegistrationText x
            , maybe [] (schemaTypeToXML "SellerAdditionalLegalInfo") $ sellerInformationDetailsType_sellerAdditionalLegalInfo x
            , maybe [] (schemaTypeToXML "SellerPhoneNumber") $ sellerInformationDetailsType_sellerPhoneNumber x
            , maybe [] (schemaTypeToXML "SellerFaxNumber") $ sellerInformationDetailsType_sellerFaxNumber x
            , maybe [] (schemaTypeToXML "SellerCommonEmailaddressIdentifier") $ sellerInformationDetailsType_sellerCommonEmailaddressIdentifier x
            , maybe [] (schemaTypeToXML "SellerWebaddressIdentifier") $ sellerInformationDetailsType_sellerWebaddressIdentifier x
            , maybe [] (schemaTypeToXML "SellerFreeText") $ sellerInformationDetailsType_sellerFreeText x
            , concatMap (schemaTypeToXML "SellerAccountDetails") $ sellerInformationDetailsType_sellerAccountDetails x
            , concatMap (schemaTypeToXML "InvoiceRecipientDetails") $ sellerInformationDetailsType_invoiceRecipientDetails x
            ]
 
data SellerPartyDetailsType = SellerPartyDetailsType
        { sellerPartyDetailsType_sellerPartyIdentifier :: Maybe PartyLegalRegIdType
        , sellerPartyDetailsType_sellerPartyIdentifierUrlText :: Maybe GenericStringType0_512
        , sellerPartyDetailsType_sellerOrganisationName :: [GenericStringType2_70]
        , sellerPartyDetailsType_sellerOrganisationTradingName :: Maybe GenericStringType2_70
        , sellerPartyDetailsType_sellerOrganisationDepartment :: [GenericStringType0_35]
        , sellerPartyDetailsType_sellerOrganisationTaxCode :: Maybe GenericStringType0_35
        , sellerPartyDetailsType_sellerOrganisationTaxCodeUrlText :: Maybe GenericStringType0_512
        , sellerPartyDetailsType_sellerCode :: [PartyIdentifierType]
        , sellerPartyDetailsType_sellerPostalAddressDetails :: Maybe SellerPostalAddressDetailsType
        }
        deriving (Eq,Show)
instance SchemaType SellerPartyDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return SellerPartyDetailsType
            `apply` optional (parseSchemaType "SellerPartyIdentifier")
            `apply` optional (parseSchemaType "SellerPartyIdentifierUrlText")
            `apply` many1 (parseSchemaType "SellerOrganisationName")
            `apply` optional (parseSchemaType "SellerOrganisationTradingName")
            `apply` between (Occurs (Just 0) (Just 2))
                            (parseSchemaType "SellerOrganisationDepartment")
            `apply` optional (parseSchemaType "SellerOrganisationTaxCode")
            `apply` optional (parseSchemaType "SellerOrganisationTaxCodeUrlText")
            `apply` many (parseSchemaType "SellerCode")
            `apply` optional (parseSchemaType "SellerPostalAddressDetails")
    schemaTypeToXML s x@SellerPartyDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "SellerPartyIdentifier") $ sellerPartyDetailsType_sellerPartyIdentifier x
            , maybe [] (schemaTypeToXML "SellerPartyIdentifierUrlText") $ sellerPartyDetailsType_sellerPartyIdentifierUrlText x
            , concatMap (schemaTypeToXML "SellerOrganisationName") $ sellerPartyDetailsType_sellerOrganisationName x
            , maybe [] (schemaTypeToXML "SellerOrganisationTradingName") $ sellerPartyDetailsType_sellerOrganisationTradingName x
            , concatMap (schemaTypeToXML "SellerOrganisationDepartment") $ sellerPartyDetailsType_sellerOrganisationDepartment x
            , maybe [] (schemaTypeToXML "SellerOrganisationTaxCode") $ sellerPartyDetailsType_sellerOrganisationTaxCode x
            , maybe [] (schemaTypeToXML "SellerOrganisationTaxCodeUrlText") $ sellerPartyDetailsType_sellerOrganisationTaxCodeUrlText x
            , concatMap (schemaTypeToXML "SellerCode") $ sellerPartyDetailsType_sellerCode x
            , maybe [] (schemaTypeToXML "SellerPostalAddressDetails") $ sellerPartyDetailsType_sellerPostalAddressDetails x
            ]
 
data SellerPostalAddressDetailsType = SellerPostalAddressDetailsType
        { sellerPostalAddressDetailsType_sellerStreetName :: [GenericStringType2_35]
        , sellerPostalAddressDetailsType_sellerTownName :: GenericStringType2_35
        , sellerPostalAddressDetailsType_sellerPostCodeIdentifier :: GenericStringType2_35
        , sellerPostalAddressDetailsType_sellerCountrySubdivision :: Maybe GenericStringType2_35
        , sellerPostalAddressDetailsType_countryCode :: Maybe CountryCodeType
        , sellerPostalAddressDetailsType_countryName :: Maybe GenericStringType0_35
        , sellerPostalAddressDetailsType_sellerPostOfficeBoxIdentifier :: Maybe GenericStringType0_35
        }
        deriving (Eq,Show)
instance SchemaType SellerPostalAddressDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return SellerPostalAddressDetailsType
            `apply` between (Occurs Nothing (Just 3))
                            (parseSchemaType "SellerStreetName")
            `apply` parseSchemaType "SellerTownName"
            `apply` parseSchemaType "SellerPostCodeIdentifier"
            `apply` optional (parseSchemaType "SellerCountrySubdivision")
            `apply` optional (parseSchemaType "CountryCode")
            `apply` optional (parseSchemaType "CountryName")
            `apply` optional (parseSchemaType "SellerPostOfficeBoxIdentifier")
    schemaTypeToXML s x@SellerPostalAddressDetailsType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "SellerStreetName") $ sellerPostalAddressDetailsType_sellerStreetName x
            , schemaTypeToXML "SellerTownName" $ sellerPostalAddressDetailsType_sellerTownName x
            , schemaTypeToXML "SellerPostCodeIdentifier" $ sellerPostalAddressDetailsType_sellerPostCodeIdentifier x
            , maybe [] (schemaTypeToXML "SellerCountrySubdivision") $ sellerPostalAddressDetailsType_sellerCountrySubdivision x
            , maybe [] (schemaTypeToXML "CountryCode") $ sellerPostalAddressDetailsType_countryCode x
            , maybe [] (schemaTypeToXML "CountryName") $ sellerPostalAddressDetailsType_countryName x
            , maybe [] (schemaTypeToXML "SellerPostOfficeBoxIdentifier") $ sellerPostalAddressDetailsType_sellerPostOfficeBoxIdentifier x
            ]
 
data AnyPartyCommunicationDetailsType = AnyPartyCommunicationDetailsType
        { anyPartyCommunicationDetailsType_anyPartyPhoneNumberIdentifier :: Maybe GenericStringType0_35
        , anyPartyCommunicationDetailsType_anyPartyEmailAddressIdentifier :: Maybe GenericStringType0_70
        }
        deriving (Eq,Show)
instance SchemaType AnyPartyCommunicationDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return AnyPartyCommunicationDetailsType
            `apply` optional (parseSchemaType "AnyPartyPhoneNumberIdentifier")
            `apply` optional (parseSchemaType "AnyPartyEmailAddressIdentifier")
    schemaTypeToXML s x@AnyPartyCommunicationDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "AnyPartyPhoneNumberIdentifier") $ anyPartyCommunicationDetailsType_anyPartyPhoneNumberIdentifier x
            , maybe [] (schemaTypeToXML "AnyPartyEmailAddressIdentifier") $ anyPartyCommunicationDetailsType_anyPartyEmailAddressIdentifier x
            ]
 
data SpecificationDetailsType = SpecificationDetailsType
        { specificationDetailsType_specificationFreeText :: [GenericStringType0_80]
        , specificationDetailsType_externalSpecificationDetails :: Maybe ExternalSpecificationDetailsType
        }
        deriving (Eq,Show)
instance SchemaType SpecificationDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return SpecificationDetailsType
            `apply` many (parseSchemaType "SpecificationFreeText")
            `apply` optional (parseSchemaType "ExternalSpecificationDetails")
    schemaTypeToXML s x@SpecificationDetailsType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "SpecificationFreeText") $ specificationDetailsType_specificationFreeText x
            , maybe [] (schemaTypeToXML "ExternalSpecificationDetails") $ specificationDetailsType_externalSpecificationDetails x
            ]
 
data ExternalSpecificationDetailsType = ExternalSpecificationDetailsType
        { externalSpecificationDetailsType_any0 :: [AnyElement]
        }
        deriving (Eq,Show)
instance SchemaType ExternalSpecificationDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return ExternalSpecificationDetailsType
            `apply` many (parseAnyElement)
    schemaTypeToXML s x@ExternalSpecificationDetailsType{} =
        toXMLElement s []
            [ concatMap (toXMLAnyElement) $ externalSpecificationDetailsType_any0 x
            ]
 
data VatSpecificationDetailsType = VatSpecificationDetailsType
        { vatSpecificationDetailsType_vatBaseAmount :: Maybe Amount
        , vatSpecificationDetailsType_vatRatePercent :: Maybe Percentage
        , vatSpecificationDetailsType_vatCode :: Maybe Untdid5305
        , vatSpecificationDetailsType_vatRateAmount :: Maybe Amount
        , vatSpecificationDetailsType_vatFreeText :: [GenericStringType0_70]
        , vatSpecificationDetailsType_vatExemptionReasonCode :: Maybe VatExReasonCodeType
        }
        deriving (Eq,Show)
instance SchemaType VatSpecificationDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return VatSpecificationDetailsType
            `apply` optional (parseSchemaType "VatBaseAmount")
            `apply` optional (parseSchemaType "VatRatePercent")
            `apply` optional (parseSchemaType "VatCode")
            `apply` optional (parseSchemaType "VatRateAmount")
            `apply` between (Occurs (Just 0) (Just 3))
                            (parseSchemaType "VatFreeText")
            `apply` optional (parseSchemaType "VatExemptionReasonCode")
    schemaTypeToXML s x@VatSpecificationDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "VatBaseAmount") $ vatSpecificationDetailsType_vatBaseAmount x
            , maybe [] (schemaTypeToXML "VatRatePercent") $ vatSpecificationDetailsType_vatRatePercent x
            , maybe [] (schemaTypeToXML "VatCode") $ vatSpecificationDetailsType_vatCode x
            , maybe [] (schemaTypeToXML "VatRateAmount") $ vatSpecificationDetailsType_vatRateAmount x
            , concatMap (schemaTypeToXML "VatFreeText") $ vatSpecificationDetailsType_vatFreeText x
            , maybe [] (schemaTypeToXML "VatExemptionReasonCode") $ vatSpecificationDetailsType_vatExemptionReasonCode x
            ]
 
newtype VatExReasonCodeType = VatExReasonCodeType Xs.NMTOKEN deriving (Eq,Show)
instance Restricts VatExReasonCodeType Xs.NMTOKEN where
    restricts (VatExReasonCodeType x) = x
instance SchemaType VatExReasonCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (VatExReasonCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType VatExReasonCodeType where
    acceptingParser = fmap VatExReasonCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Za-z0-9\-]{1,20})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (VatExReasonCodeType x) = simpleTypeText x
 
data VatPointType = VatPointType
        { vatPointType_choice0 :: OneOf2 Date Untdid2005
          -- ^ Choice between:
          --   
          --   (1) VatPointDate
          --   
          --   (2) VatPointDateCode
        }
        deriving (Eq,Show)
instance SchemaType VatPointType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return VatPointType
            `apply` oneOf' [ ("Date", fmap OneOf2 (parseSchemaType "VatPointDate"))
                           , ("Untdid2005", fmap TwoOf2 (parseSchemaType "VatPointDateCode"))
                           ]
    schemaTypeToXML s x@VatPointType{} =
        toXMLElement s []
            [ foldOneOf2  (schemaTypeToXML "VatPointDate")
                          (schemaTypeToXML "VatPointDateCode")
                          $ vatPointType_choice0 x
            ]
 
data PartyIdentifierType = PartyIdentifierType GenericStringType0_70 PartyIdentifierTypeAttributes deriving (Eq,Show)
data PartyIdentifierTypeAttributes = PartyIdentifierTypeAttributes
    { partyIdentifierTypeAttributes_identifierType :: Maybe GenericTokenType1_20
    , partyIdentifierTypeAttributes_schemeID :: Maybe Iso6523cid
    }
    deriving (Eq,Show)
instance SchemaType PartyIdentifierType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- optional $ getAttribute "IdentifierType" e pos
          a1 <- optional $ getAttribute "SchemeID" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ PartyIdentifierType v (PartyIdentifierTypeAttributes a0 a1)
    schemaTypeToXML s (PartyIdentifierType bt at) =
        addXMLAttributes [ maybe [] (toXMLAttribute "IdentifierType") $ partyIdentifierTypeAttributes_identifierType at
                         , maybe [] (toXMLAttribute "SchemeID") $ partyIdentifierTypeAttributes_schemeID at
                         ]
            $ schemaTypeToXML s bt
instance Extension PartyIdentifierType GenericStringType0_70 where
    supertype (PartyIdentifierType s _) = s
 
data PartyLegalRegIdType = PartyLegalRegIdType GenericStringType0_35 PartyLegalRegIdTypeAttributes deriving (Eq,Show)
data PartyLegalRegIdTypeAttributes = PartyLegalRegIdTypeAttributes
    { partyLegalRegIdTypeAttributes_schemeID :: Maybe Iso6523cid
    }
    deriving (Eq,Show)
instance SchemaType PartyLegalRegIdType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- optional $ getAttribute "SchemeID" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ PartyLegalRegIdType v (PartyLegalRegIdTypeAttributes a0)
    schemaTypeToXML s (PartyLegalRegIdType bt at) =
        addXMLAttributes [ maybe [] (toXMLAttribute "SchemeID") $ partyLegalRegIdTypeAttributes_schemeID at
                         ]
            $ schemaTypeToXML s bt
instance Extension PartyLegalRegIdType GenericStringType0_35 where
    supertype (PartyLegalRegIdType s _) = s
 
newtype VatCategoryCodeType = VatCategoryCodeType Untdid5305 deriving (Eq,Show)
instance Restricts VatCategoryCodeType Untdid5305 where
    restricts (VatCategoryCodeType x) = x
instance SchemaType VatCategoryCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (VatCategoryCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType VatCategoryCodeType where
    acceptingParser = fmap VatCategoryCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (VatCategoryCodeType x) = simpleTypeText x
 
data DiscountDetailsType = DiscountDetailsType
        { discountDetailsType_freeText :: Maybe GenericStringType1_70
        , discountDetailsType_reasonCode :: Maybe Untdid5189
        , discountDetailsType_percent :: Maybe Percentage
        , discountDetailsType_amount :: Maybe Amount
        , discountDetailsType_baseAmount :: Maybe Amount
        , discountDetailsType_vatCategoryCode :: Maybe VatCategoryCodeType
        , discountDetailsType_vatRatePercent :: Maybe Percentage
        }
        deriving (Eq,Show)
instance SchemaType DiscountDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DiscountDetailsType
            `apply` optional (parseSchemaType "FreeText")
            `apply` optional (parseSchemaType "ReasonCode")
            `apply` optional (parseSchemaType "Percent")
            `apply` optional (parseSchemaType "Amount")
            `apply` optional (parseSchemaType "BaseAmount")
            `apply` optional (parseSchemaType "VatCategoryCode")
            `apply` optional (parseSchemaType "VatRatePercent")
    schemaTypeToXML s x@DiscountDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "FreeText") $ discountDetailsType_freeText x
            , maybe [] (schemaTypeToXML "ReasonCode") $ discountDetailsType_reasonCode x
            , maybe [] (schemaTypeToXML "Percent") $ discountDetailsType_percent x
            , maybe [] (schemaTypeToXML "Amount") $ discountDetailsType_amount x
            , maybe [] (schemaTypeToXML "BaseAmount") $ discountDetailsType_baseAmount x
            , maybe [] (schemaTypeToXML "VatCategoryCode") $ discountDetailsType_vatCategoryCode x
            , maybe [] (schemaTypeToXML "VatRatePercent") $ discountDetailsType_vatRatePercent x
            ]
 
data InvoiceChargeDetailsType = InvoiceChargeDetailsType
        { invoiceChargeDetailsType_reasonText :: Maybe GenericStringType1_70
        , invoiceChargeDetailsType_reasonCode :: Maybe Untdid7161
        , invoiceChargeDetailsType_percent :: Maybe Percentage
        , invoiceChargeDetailsType_amount :: Maybe Amount
        , invoiceChargeDetailsType_baseAmount :: Maybe Amount
        , invoiceChargeDetailsType_vatCategoryCode :: Maybe VatCategoryCodeType
        , invoiceChargeDetailsType_vatRatePercent :: Maybe Percentage
        }
        deriving (Eq,Show)
instance SchemaType InvoiceChargeDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return InvoiceChargeDetailsType
            `apply` optional (parseSchemaType "ReasonText")
            `apply` optional (parseSchemaType "ReasonCode")
            `apply` optional (parseSchemaType "Percent")
            `apply` optional (parseSchemaType "Amount")
            `apply` optional (parseSchemaType "BaseAmount")
            `apply` optional (parseSchemaType "VatCategoryCode")
            `apply` optional (parseSchemaType "VatRatePercent")
    schemaTypeToXML s x@InvoiceChargeDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "ReasonText") $ invoiceChargeDetailsType_reasonText x
            , maybe [] (schemaTypeToXML "ReasonCode") $ invoiceChargeDetailsType_reasonCode x
            , maybe [] (schemaTypeToXML "Percent") $ invoiceChargeDetailsType_percent x
            , maybe [] (schemaTypeToXML "Amount") $ invoiceChargeDetailsType_amount x
            , maybe [] (schemaTypeToXML "BaseAmount") $ invoiceChargeDetailsType_baseAmount x
            , maybe [] (schemaTypeToXML "VatCategoryCode") $ invoiceChargeDetailsType_vatCategoryCode x
            , maybe [] (schemaTypeToXML "VatRatePercent") $ invoiceChargeDetailsType_vatRatePercent x
            ]
 
data RowChargeDetailsType = RowChargeDetailsType
        { rowChargeDetailsType_reasonText :: Maybe GenericStringType1_70
        , rowChargeDetailsType_reasonCode :: Maybe Untdid7161
        , rowChargeDetailsType_percent :: Maybe Percentage
        , rowChargeDetailsType_amount :: Maybe Amount
        , rowChargeDetailsType_baseAmount :: Maybe Amount
        }
        deriving (Eq,Show)
instance SchemaType RowChargeDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return RowChargeDetailsType
            `apply` optional (parseSchemaType "ReasonText")
            `apply` optional (parseSchemaType "ReasonCode")
            `apply` optional (parseSchemaType "Percent")
            `apply` optional (parseSchemaType "Amount")
            `apply` optional (parseSchemaType "BaseAmount")
    schemaTypeToXML s x@RowChargeDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "ReasonText") $ rowChargeDetailsType_reasonText x
            , maybe [] (schemaTypeToXML "ReasonCode") $ rowChargeDetailsType_reasonCode x
            , maybe [] (schemaTypeToXML "Percent") $ rowChargeDetailsType_percent x
            , maybe [] (schemaTypeToXML "Amount") $ rowChargeDetailsType_amount x
            , maybe [] (schemaTypeToXML "BaseAmount") $ rowChargeDetailsType_baseAmount x
            ]
 
data CustomsInfoType = CustomsInfoType
        { customsInfoType_cNCode :: Maybe GenericStringType1_8
        , customsInfoType_cNName :: Maybe GenericStringType1_35
        , customsInfoType_cNOriginCountrySubdivision :: Maybe GenericStringType2_35
        , customsInfoType_cNOriginCountryCode :: Maybe CountryCodeType
        , customsInfoType_cNOriginCountryName :: Maybe GenericStringType1_35
        }
        deriving (Eq,Show)
instance SchemaType CustomsInfoType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return CustomsInfoType
            `apply` optional (parseSchemaType "CNCode")
            `apply` optional (parseSchemaType "CNName")
            `apply` optional (parseSchemaType "CNOriginCountrySubdivision")
            `apply` optional (parseSchemaType "CNOriginCountryCode")
            `apply` optional (parseSchemaType "CNOriginCountryName")
    schemaTypeToXML s x@CustomsInfoType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "CNCode") $ customsInfoType_cNCode x
            , maybe [] (schemaTypeToXML "CNName") $ customsInfoType_cNName x
            , maybe [] (schemaTypeToXML "CNOriginCountrySubdivision") $ customsInfoType_cNOriginCountrySubdivision x
            , maybe [] (schemaTypeToXML "CNOriginCountryCode") $ customsInfoType_cNOriginCountryCode x
            , maybe [] (schemaTypeToXML "CNOriginCountryName") $ customsInfoType_cNOriginCountryName x
            ]
 
data TransactionDetailsType = TransactionDetailsType
        { transactionDetailsType_otherCurrencyAmount :: Maybe Amount
        , transactionDetailsType_exchangeRate :: Maybe ExchangeRate
        , transactionDetailsType_exchangeDate :: Maybe Date
        }
        deriving (Eq,Show)
instance SchemaType TransactionDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return TransactionDetailsType
            `apply` optional (parseSchemaType "OtherCurrencyAmount")
            `apply` optional (parseSchemaType "ExchangeRate")
            `apply` optional (parseSchemaType "ExchangeDate")
    schemaTypeToXML s x@TransactionDetailsType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "OtherCurrencyAmount") $ transactionDetailsType_otherCurrencyAmount x
            , maybe [] (schemaTypeToXML "ExchangeRate") $ transactionDetailsType_exchangeRate x
            , maybe [] (schemaTypeToXML "ExchangeDate") $ transactionDetailsType_exchangeDate x
            ]
 
data AttachmentMessageDetailsType = AttachmentMessageDetailsType
        { attachmentMessageDetailsType_attachmentMessageIdentifier :: AttachmentsIdentifierType
        }
        deriving (Eq,Show)
instance SchemaType AttachmentMessageDetailsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return AttachmentMessageDetailsType
            `apply` parseSchemaType "AttachmentMessageIdentifier"
    schemaTypeToXML s x@AttachmentMessageDetailsType{} =
        toXMLElement s []
            [ schemaTypeToXML "AttachmentMessageIdentifier" $ attachmentMessageDetailsType_attachmentMessageIdentifier x
            ]
 
data QuantityType = QuantityType Xsd.XsdString QuantityTypeAttributes deriving (Eq,Show)
data QuantityTypeAttributes = QuantityTypeAttributes
    { quantityTypeAttributes_quantityUnitCode :: Maybe GenericTokenType0_14
    , quantityTypeAttributes_quantityUnitCodeUN :: Maybe UnitCodeUN
    }
    deriving (Eq,Show)
instance SchemaType QuantityType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- optional $ getAttribute "QuantityUnitCode" e pos
          a1 <- optional $ getAttribute "QuantityUnitCodeUN" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ QuantityType v (QuantityTypeAttributes a0 a1)
    schemaTypeToXML s (QuantityType bt at) =
        addXMLAttributes [ maybe [] (toXMLAttribute "QuantityUnitCode") $ quantityTypeAttributes_quantityUnitCode at
                         , maybe [] (toXMLAttribute "QuantityUnitCodeUN") $ quantityTypeAttributes_quantityUnitCodeUN at
                         ]
            $ schemaTypeToXML s bt
instance Extension QuantityType Xsd.XsdString where
    supertype (QuantityType s _) = s
 
newtype QuantityType0_14 = QuantityType0_14 Unimplemented deriving (Eq,Show)
instance Restricts QuantityType0_14 Unimplemented where
    restricts (QuantityType0_14 x) = x
instance SchemaType QuantityType0_14 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (QuantityType0_14 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType QuantityType0_14 where
    acceptingParser = fmap QuantityType0_14 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (QuantityType0_14 x) = simpleTypeText x
 
newtype QuantityType0_70 = QuantityType0_70 Unimplemented deriving (Eq,Show)
instance Restricts QuantityType0_70 Unimplemented where
    restricts (QuantityType0_70 x) = x
instance SchemaType QuantityType0_70 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (QuantityType0_70 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType QuantityType0_70 where
    acceptingParser = fmap QuantityType0_70 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (QuantityType0_70 x) = simpleTypeText x
 
data AnyPartyTextType = AnyPartyTextType Xsd.XsdString AnyPartyTextTypeAttributes deriving (Eq,Show)
data AnyPartyTextTypeAttributes = AnyPartyTextTypeAttributes
    { anyPartyTextTypeAttributes_anyPartyCode :: GenericTokenType0_35
    }
    deriving (Eq,Show)
instance SchemaType AnyPartyTextType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- getAttribute "AnyPartyCode" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ AnyPartyTextType v (AnyPartyTextTypeAttributes a0)
    schemaTypeToXML s (AnyPartyTextType bt at) =
        addXMLAttributes [ toXMLAttribute "AnyPartyCode" $ anyPartyTextTypeAttributes_anyPartyCode at
                         ]
            $ schemaTypeToXML s bt
instance Extension AnyPartyTextType Xsd.XsdString where
    supertype (AnyPartyTextType s _) = s
 
newtype Anypartytexttype0_35 = Anypartytexttype0_35 Unimplemented deriving (Eq,Show)
instance Restricts Anypartytexttype0_35 Unimplemented where
    restricts (Anypartytexttype0_35 x) = x
instance SchemaType Anypartytexttype0_35 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (Anypartytexttype0_35 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Anypartytexttype0_35 where
    acceptingParser = fmap Anypartytexttype0_35 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (Anypartytexttype0_35 x) = simpleTypeText x
 
newtype AttachmentsIdentifierType = AttachmentsIdentifierType Xsd.XsdString deriving (Eq,Show)
instance Restricts AttachmentsIdentifierType Xsd.XsdString where
    restricts (AttachmentsIdentifierType x) = x
instance SchemaType AttachmentsIdentifierType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (AttachmentsIdentifierType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AttachmentsIdentifierType where
    acceptingParser = fmap AttachmentsIdentifierType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern .{2,48}::attachments)
    --      (Enumeration)
    simpleTypeText (AttachmentsIdentifierType x) = simpleTypeText x
 
newtype GenericTokenType3 = GenericTokenType3 Xs.Token deriving (Eq,Show)
instance Restricts GenericTokenType3 Xs.Token where
    restricts (GenericTokenType3 x) = x
instance SchemaType GenericTokenType3 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericTokenType3 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericTokenType3 where
    acceptingParser = fmap GenericTokenType3 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericTokenType3 x) = simpleTypeText x
 
newtype GenericTokenType0_14 = GenericTokenType0_14 Xs.Token deriving (Eq,Show)
instance Restricts GenericTokenType0_14 Xs.Token where
    restricts (GenericTokenType0_14 x) = x
instance SchemaType GenericTokenType0_14 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericTokenType0_14 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericTokenType0_14 where
    acceptingParser = fmap GenericTokenType0_14 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericTokenType0_14 x) = simpleTypeText x
 
newtype GenericTokenType1_20 = GenericTokenType1_20 Xs.Token deriving (Eq,Show)
instance Restricts GenericTokenType1_20 Xs.Token where
    restricts (GenericTokenType1_20 x) = x
instance SchemaType GenericTokenType1_20 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericTokenType1_20 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericTokenType1_20 where
    acceptingParser = fmap GenericTokenType1_20 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericTokenType1_20 x) = simpleTypeText x
 
newtype GenericTokenType0_35 = GenericTokenType0_35 Xs.Token deriving (Eq,Show)
instance Restricts GenericTokenType0_35 Xs.Token where
    restricts (GenericTokenType0_35 x) = x
instance SchemaType GenericTokenType0_35 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericTokenType0_35 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericTokenType0_35 where
    acceptingParser = fmap GenericTokenType0_35 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericTokenType0_35 x) = simpleTypeText x
 
newtype GenericTokenType2_35 = GenericTokenType2_35 Xs.Token deriving (Eq,Show)
instance Restricts GenericTokenType2_35 Xs.Token where
    restricts (GenericTokenType2_35 x) = x
instance SchemaType GenericTokenType2_35 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericTokenType2_35 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericTokenType2_35 where
    acceptingParser = fmap GenericTokenType2_35 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericTokenType2_35 x) = simpleTypeText x
 
newtype GenericTokenType0_70 = GenericTokenType0_70 Xs.Token deriving (Eq,Show)
instance Restricts GenericTokenType0_70 Xs.Token where
    restricts (GenericTokenType0_70 x) = x
instance SchemaType GenericTokenType0_70 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericTokenType0_70 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericTokenType0_70 where
    acceptingParser = fmap GenericTokenType0_70 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericTokenType0_70 x) = simpleTypeText x
 
newtype GenericNMtokenType0_4 = GenericNMtokenType0_4 Xs.Token deriving (Eq,Show)
instance Restricts GenericNMtokenType0_4 Xs.Token where
    restricts (GenericNMtokenType0_4 x) = x
instance SchemaType GenericNMtokenType0_4 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericNMtokenType0_4 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericNMtokenType0_4 where
    acceptingParser = fmap GenericNMtokenType0_4 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern \c*)
    --      (Enumeration)
    simpleTypeText (GenericNMtokenType0_4 x) = simpleTypeText x
 
newtype GenericNMtokenType8_11 = GenericNMtokenType8_11 Xs.Token deriving (Eq,Show)
instance Restricts GenericNMtokenType8_11 Xs.Token where
    restricts (GenericNMtokenType8_11 x) = x
instance SchemaType GenericNMtokenType8_11 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericNMtokenType8_11 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericNMtokenType8_11 where
    acceptingParser = fmap GenericNMtokenType8_11 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern \c*)
    --      (Enumeration)
    simpleTypeText (GenericNMtokenType8_11 x) = simpleTypeText x
 
newtype GenericNMtokenType0_14 = GenericNMtokenType0_14 Xs.Token deriving (Eq,Show)
instance Restricts GenericNMtokenType0_14 Xs.Token where
    restricts (GenericNMtokenType0_14 x) = x
instance SchemaType GenericNMtokenType0_14 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericNMtokenType0_14 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericNMtokenType0_14 where
    acceptingParser = fmap GenericNMtokenType0_14 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern \c*)
    --      (Enumeration)
    simpleTypeText (GenericNMtokenType0_14 x) = simpleTypeText x
 
newtype GenericNMtokenType1_34 = GenericNMtokenType1_34 Xs.Token deriving (Eq,Show)
instance Restricts GenericNMtokenType1_34 Xs.Token where
    restricts (GenericNMtokenType1_34 x) = x
instance SchemaType GenericNMtokenType1_34 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericNMtokenType1_34 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericNMtokenType1_34 where
    acceptingParser = fmap GenericNMtokenType1_34 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern \c*)
    --      (Enumeration)
    simpleTypeText (GenericNMtokenType1_34 x) = simpleTypeText x
 
newtype GenericNMtokenType0_35 = GenericNMtokenType0_35 Xs.Token deriving (Eq,Show)
instance Restricts GenericNMtokenType0_35 Xs.Token where
    restricts (GenericNMtokenType0_35 x) = x
instance SchemaType GenericNMtokenType0_35 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericNMtokenType0_35 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericNMtokenType0_35 where
    acceptingParser = fmap GenericNMtokenType0_35 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern \c*)
    --      (Enumeration)
    simpleTypeText (GenericNMtokenType0_35 x) = simpleTypeText x
 
newtype GenericNMtokenType2_35 = GenericNMtokenType2_35 Xs.Token deriving (Eq,Show)
instance Restricts GenericNMtokenType2_35 Xs.Token where
    restricts (GenericNMtokenType2_35 x) = x
instance SchemaType GenericNMtokenType2_35 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericNMtokenType2_35 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericNMtokenType2_35 where
    acceptingParser = fmap GenericNMtokenType2_35 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern \c*)
    --      (Enumeration)
    simpleTypeText (GenericNMtokenType2_35 x) = simpleTypeText x
 
newtype GenericNMtokenType0_512 = GenericNMtokenType0_512 Xs.Token deriving (Eq,Show)
instance Restricts GenericNMtokenType0_512 Xs.Token where
    restricts (GenericNMtokenType0_512 x) = x
instance SchemaType GenericNMtokenType0_512 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericNMtokenType0_512 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericNMtokenType0_512 where
    acceptingParser = fmap GenericNMtokenType0_512 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern \c*)
    --      (Enumeration)
    simpleTypeText (GenericNMtokenType0_512 x) = simpleTypeText x
 
newtype GenericStringType0_4 = GenericStringType0_4 Xsd.XsdString deriving (Eq,Show)
instance Restricts GenericStringType0_4 Xsd.XsdString where
    restricts (GenericStringType0_4 x) = x
instance SchemaType GenericStringType0_4 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericStringType0_4 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericStringType0_4 where
    acceptingParser = fmap GenericStringType0_4 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericStringType0_4 x) = simpleTypeText x
 
newtype GenericStringType1_4 = GenericStringType1_4 Xsd.XsdString deriving (Eq,Show)
instance Restricts GenericStringType1_4 Xsd.XsdString where
    restricts (GenericStringType1_4 x) = x
instance SchemaType GenericStringType1_4 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericStringType1_4 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericStringType1_4 where
    acceptingParser = fmap GenericStringType1_4 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericStringType1_4 x) = simpleTypeText x
 
newtype GenericStringType1_8 = GenericStringType1_8 Xsd.XsdString deriving (Eq,Show)
instance Restricts GenericStringType1_8 Xsd.XsdString where
    restricts (GenericStringType1_8 x) = x
instance SchemaType GenericStringType1_8 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericStringType1_8 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericStringType1_8 where
    acceptingParser = fmap GenericStringType1_8 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericStringType1_8 x) = simpleTypeText x
 
newtype GenericStringType0_10 = GenericStringType0_10 Xsd.XsdString deriving (Eq,Show)
instance Restricts GenericStringType0_10 Xsd.XsdString where
    restricts (GenericStringType0_10 x) = x
instance SchemaType GenericStringType0_10 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericStringType0_10 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericStringType0_10 where
    acceptingParser = fmap GenericStringType0_10 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericStringType0_10 x) = simpleTypeText x
 
newtype GenericStringType1_10 = GenericStringType1_10 Xsd.XsdString deriving (Eq,Show)
instance Restricts GenericStringType1_10 Xsd.XsdString where
    restricts (GenericStringType1_10 x) = x
instance SchemaType GenericStringType1_10 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericStringType1_10 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericStringType1_10 where
    acceptingParser = fmap GenericStringType1_10 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericStringType1_10 x) = simpleTypeText x
 
newtype GenericStringType0_14 = GenericStringType0_14 Xsd.XsdString deriving (Eq,Show)
instance Restricts GenericStringType0_14 Xsd.XsdString where
    restricts (GenericStringType0_14 x) = x
instance SchemaType GenericStringType0_14 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericStringType0_14 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericStringType0_14 where
    acceptingParser = fmap GenericStringType0_14 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericStringType0_14 x) = simpleTypeText x
 
newtype GenericStringType1_13 = GenericStringType1_13 Xsd.XsdString deriving (Eq,Show)
instance Restricts GenericStringType1_13 Xsd.XsdString where
    restricts (GenericStringType1_13 x) = x
instance SchemaType GenericStringType1_13 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericStringType1_13 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericStringType1_13 where
    acceptingParser = fmap GenericStringType1_13 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericStringType1_13 x) = simpleTypeText x
 
newtype GenericStringType1_19 = GenericStringType1_19 Xsd.XsdString deriving (Eq,Show)
instance Restricts GenericStringType1_19 Xsd.XsdString where
    restricts (GenericStringType1_19 x) = x
instance SchemaType GenericStringType1_19 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericStringType1_19 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericStringType1_19 where
    acceptingParser = fmap GenericStringType1_19 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericStringType1_19 x) = simpleTypeText x
 
newtype GenericStringType1_20 = GenericStringType1_20 Xsd.XsdString deriving (Eq,Show)
instance Restricts GenericStringType1_20 Xsd.XsdString where
    restricts (GenericStringType1_20 x) = x
instance SchemaType GenericStringType1_20 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericStringType1_20 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericStringType1_20 where
    acceptingParser = fmap GenericStringType1_20 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericStringType1_20 x) = simpleTypeText x
 
newtype GenericStringType0_35 = GenericStringType0_35 Xsd.XsdString deriving (Eq,Show)
instance Restricts GenericStringType0_35 Xsd.XsdString where
    restricts (GenericStringType0_35 x) = x
instance SchemaType GenericStringType0_35 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericStringType0_35 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericStringType0_35 where
    acceptingParser = fmap GenericStringType0_35 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericStringType0_35 x) = simpleTypeText x
 
newtype GenericStringType1_35 = GenericStringType1_35 Xsd.XsdString deriving (Eq,Show)
instance Restricts GenericStringType1_35 Xsd.XsdString where
    restricts (GenericStringType1_35 x) = x
instance SchemaType GenericStringType1_35 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericStringType1_35 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericStringType1_35 where
    acceptingParser = fmap GenericStringType1_35 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericStringType1_35 x) = simpleTypeText x
 
newtype GenericStringType2_35 = GenericStringType2_35 Xsd.XsdString deriving (Eq,Show)
instance Restricts GenericStringType2_35 Xsd.XsdString where
    restricts (GenericStringType2_35 x) = x
instance SchemaType GenericStringType2_35 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericStringType2_35 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericStringType2_35 where
    acceptingParser = fmap GenericStringType2_35 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericStringType2_35 x) = simpleTypeText x
 
newtype GenericStringType0_48 = GenericStringType0_48 Xsd.XsdString deriving (Eq,Show)
instance Restricts GenericStringType0_48 Xsd.XsdString where
    restricts (GenericStringType0_48 x) = x
instance SchemaType GenericStringType0_48 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericStringType0_48 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericStringType0_48 where
    acceptingParser = fmap GenericStringType0_48 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericStringType0_48 x) = simpleTypeText x
 
newtype GenericStringType2_48 = GenericStringType2_48 Xsd.XsdString deriving (Eq,Show)
instance Restricts GenericStringType2_48 Xsd.XsdString where
    restricts (GenericStringType2_48 x) = x
instance SchemaType GenericStringType2_48 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericStringType2_48 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericStringType2_48 where
    acceptingParser = fmap GenericStringType2_48 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericStringType2_48 x) = simpleTypeText x
 
newtype GenericStringType0_70 = GenericStringType0_70 Xsd.XsdString deriving (Eq,Show)
instance Restricts GenericStringType0_70 Xsd.XsdString where
    restricts (GenericStringType0_70 x) = x
instance SchemaType GenericStringType0_70 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericStringType0_70 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericStringType0_70 where
    acceptingParser = fmap GenericStringType0_70 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericStringType0_70 x) = simpleTypeText x
 
newtype GenericStringType1_70 = GenericStringType1_70 Xsd.XsdString deriving (Eq,Show)
instance Restricts GenericStringType1_70 Xsd.XsdString where
    restricts (GenericStringType1_70 x) = x
instance SchemaType GenericStringType1_70 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericStringType1_70 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericStringType1_70 where
    acceptingParser = fmap GenericStringType1_70 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericStringType1_70 x) = simpleTypeText x
 
newtype GenericStringType2_70 = GenericStringType2_70 Xsd.XsdString deriving (Eq,Show)
instance Restricts GenericStringType2_70 Xsd.XsdString where
    restricts (GenericStringType2_70 x) = x
instance SchemaType GenericStringType2_70 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericStringType2_70 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericStringType2_70 where
    acceptingParser = fmap GenericStringType2_70 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericStringType2_70 x) = simpleTypeText x
 
newtype GenericStringType0_80 = GenericStringType0_80 Xsd.XsdString deriving (Eq,Show)
instance Restricts GenericStringType0_80 Xsd.XsdString where
    restricts (GenericStringType0_80 x) = x
instance SchemaType GenericStringType0_80 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericStringType0_80 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericStringType0_80 where
    acceptingParser = fmap GenericStringType0_80 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericStringType0_80 x) = simpleTypeText x
 
newtype GenericStringType0_100 = GenericStringType0_100 Xsd.XsdString deriving (Eq,Show)
instance Restricts GenericStringType0_100 Xsd.XsdString where
    restricts (GenericStringType0_100 x) = x
instance SchemaType GenericStringType0_100 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericStringType0_100 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericStringType0_100 where
    acceptingParser = fmap GenericStringType0_100 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericStringType0_100 x) = simpleTypeText x
 
newtype GenericStringType0_512 = GenericStringType0_512 Xsd.XsdString deriving (Eq,Show)
instance Restricts GenericStringType0_512 Xsd.XsdString where
    restricts (GenericStringType0_512 x) = x
instance SchemaType GenericStringType0_512 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (GenericStringType0_512 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GenericStringType0_512 where
    acceptingParser = fmap GenericStringType0_512 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GenericStringType0_512 x) = simpleTypeText x
 
newtype Percentage = Percentage Xsd.XsdString deriving (Eq,Show)
instance Restricts Percentage Xsd.XsdString where
    restricts (Percentage x) = x
instance SchemaType Percentage where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (Percentage x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Percentage where
    acceptingParser = fmap Percentage acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [1-9]?[0-9]{1,2}(,[0-9]{1,3})?)
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (Percentage x) = simpleTypeText x
 
newtype ExchangeRate = ExchangeRate Xsd.XsdString deriving (Eq,Show)
instance Restricts ExchangeRate Xsd.XsdString where
    restricts (ExchangeRate x) = x
instance SchemaType ExchangeRate where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (ExchangeRate x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ExchangeRate where
    acceptingParser = fmap ExchangeRate acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [0-9]{1,15}(,[0-9]{1,6})?)
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (ExchangeRate x) = simpleTypeText x
 
data Date = Date DateType DateAttributes deriving (Eq,Show)
data DateAttributes = DateAttributes
    { dateAttributes_format :: Xsd.XsdString
    }
    deriving (Eq,Show)
instance SchemaType Date where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- getAttribute "Format" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ Date v (DateAttributes a0)
    schemaTypeToXML s (Date bt at) =
        addXMLAttributes [ toXMLAttribute "Format" $ dateAttributes_format at
                         ]
            $ schemaTypeToXML s bt
instance Extension Date DateType where
    supertype (Date s _) = s
 
newtype DateType = DateType Xsd.Integer deriving (Eq,Show)
instance Restricts DateType Xsd.Integer where
    restricts (DateType x) = x
instance SchemaType DateType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (DateType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DateType where
    acceptingParser = fmap DateType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9])
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (DateType x) = simpleTypeText x
 
data Amount = Amount MonetaryAmount AmountAttributes deriving (Eq,Show)
data AmountAttributes = AmountAttributes
    { amountAttributes_amountCurrencyIdentifier :: Xsd.XsdString
    }
    deriving (Eq,Show)
instance SchemaType Amount where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- getAttribute "AmountCurrencyIdentifier" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ Amount v (AmountAttributes a0)
    schemaTypeToXML s (Amount bt at) =
        addXMLAttributes [ toXMLAttribute "AmountCurrencyIdentifier" $ amountAttributes_amountCurrencyIdentifier at
                         ]
            $ schemaTypeToXML s bt
instance Extension Amount MonetaryAmount where
    supertype (Amount s _) = s
 
newtype MonetaryAmount = MonetaryAmount Xs.Token deriving (Eq,Show)
instance Restricts MonetaryAmount Xs.Token where
    restricts (MonetaryAmount x) = x
instance SchemaType MonetaryAmount where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (MonetaryAmount x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType MonetaryAmount where
    acceptingParser = fmap MonetaryAmount acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern -?[0-9]{1,15}(,[0-9]{2,5})?)
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (MonetaryAmount x) = simpleTypeText x
 
data EpiAmount = EpiAmount EpiMonetaryAmount EpiAmountAttributes deriving (Eq,Show)
data EpiAmountAttributes = EpiAmountAttributes
    { epiAmountAttributes_amountCurrencyIdentifier :: Xsd.XsdString
    }
    deriving (Eq,Show)
instance SchemaType EpiAmount where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- getAttribute "AmountCurrencyIdentifier" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ EpiAmount v (EpiAmountAttributes a0)
    schemaTypeToXML s (EpiAmount bt at) =
        addXMLAttributes [ toXMLAttribute "AmountCurrencyIdentifier" $ epiAmountAttributes_amountCurrencyIdentifier at
                         ]
            $ schemaTypeToXML s bt
instance Extension EpiAmount EpiMonetaryAmount where
    supertype (EpiAmount s _) = s
 
newtype EpiMonetaryAmount = EpiMonetaryAmount Xs.Token deriving (Eq,Show)
instance Restricts EpiMonetaryAmount Xs.Token where
    restricts (EpiMonetaryAmount x) = x
instance SchemaType EpiMonetaryAmount where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EpiMonetaryAmount x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EpiMonetaryAmount where
    acceptingParser = fmap EpiMonetaryAmount acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern -?[0-9]{1,15},[0-9]{2})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (EpiMonetaryAmount x) = simpleTypeText x
 
data UnitAmount = UnitAmount UnitAmountType UnitAmountAttributes deriving (Eq,Show)
data UnitAmountAttributes = UnitAmountAttributes
    { unitAmountAttributes_amountCurrencyIdentifier :: Xsd.XsdString
    , unitAmountAttributes_unitPriceUnitCode :: Maybe GenericStringType0_14
    }
    deriving (Eq,Show)
instance SchemaType UnitAmount where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- getAttribute "AmountCurrencyIdentifier" e pos
          a1 <- optional $ getAttribute "UnitPriceUnitCode" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ UnitAmount v (UnitAmountAttributes a0 a1)
    schemaTypeToXML s (UnitAmount bt at) =
        addXMLAttributes [ toXMLAttribute "AmountCurrencyIdentifier" $ unitAmountAttributes_amountCurrencyIdentifier at
                         , maybe [] (toXMLAttribute "UnitPriceUnitCode") $ unitAmountAttributes_unitPriceUnitCode at
                         ]
            $ schemaTypeToXML s bt
instance Extension UnitAmount UnitAmountType where
    supertype (UnitAmount s _) = s
 
data UnitAmountUN = UnitAmountUN UnitAmount UnitAmountUNAttributes deriving (Eq,Show)
data UnitAmountUNAttributes = UnitAmountUNAttributes
    { unitAmountUNAttributes_quantityUnitCodeUN :: Maybe UnitCodeUN
    }
    deriving (Eq,Show)
instance SchemaType UnitAmountUN where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- optional $ getAttribute "QuantityUnitCodeUN" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ UnitAmountUN v (UnitAmountUNAttributes a0)
    schemaTypeToXML s (UnitAmountUN bt at) =
        addXMLAttributes [ maybe [] (toXMLAttribute "QuantityUnitCodeUN") $ unitAmountUNAttributes_quantityUnitCodeUN at
                         ]
            $ schemaTypeToXML s bt
instance Extension UnitAmountUN UnitAmount where
    supertype (UnitAmountUN s _) = s
 
newtype UnitAmountType = UnitAmountType Xs.Token deriving (Eq,Show)
instance Restricts UnitAmountType Xs.Token where
    restricts (UnitAmountType x) = x
instance SchemaType UnitAmountType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (UnitAmountType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType UnitAmountType where
    acceptingParser = fmap UnitAmountType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern -?[0-9]{1,15}(,[0-9]{2,5})?)
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (UnitAmountType x) = simpleTypeText x
 
newtype CountryCodeType = CountryCodeType Xs.NMTOKEN deriving (Eq,Show)
instance Restricts CountryCodeType Xs.NMTOKEN where
    restricts (CountryCodeType x) = x
instance SchemaType CountryCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (CountryCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CountryCodeType where
    acceptingParser = fmap CountryCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (CountryCodeType x) = simpleTypeText x
 
newtype LanguageCodeType = LanguageCodeType Xs.NMTOKEN deriving (Eq,Show)
instance Restricts LanguageCodeType Xs.NMTOKEN where
    restricts (LanguageCodeType x) = x
instance SchemaType LanguageCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (LanguageCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType LanguageCodeType where
    acceptingParser = fmap LanguageCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (LanguageCodeType x) = simpleTypeText x
 
newtype Iso6523cid = Iso6523cid Xs.NMTOKEN deriving (Eq,Show)
instance Restricts Iso6523cid Xs.NMTOKEN where
    restricts (Iso6523cid x) = x
instance SchemaType Iso6523cid where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (Iso6523cid x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Iso6523cid where
    acceptingParser = fmap Iso6523cid acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [0-9]{4})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (Iso6523cid x) = simpleTypeText x
 
data EanCodeType = EanCodeType GenericStringType0_35 EanCodeTypeAttributes deriving (Eq,Show)
data EanCodeTypeAttributes = EanCodeTypeAttributes
    { eanCodeTypeAttributes_schemeID :: Maybe Iso6523cid
    }
    deriving (Eq,Show)
instance SchemaType EanCodeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- optional $ getAttribute "SchemeID" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ EanCodeType v (EanCodeTypeAttributes a0)
    schemaTypeToXML s (EanCodeType bt at) =
        addXMLAttributes [ maybe [] (toXMLAttribute "SchemeID") $ eanCodeTypeAttributes_schemeID at
                         ]
            $ schemaTypeToXML s bt
instance Extension EanCodeType GenericStringType0_35 where
    supertype (EanCodeType s _) = s
 
data DestinationNameType = DestinationNameType GenericStringType0_35 DestinationNameTypeAttributes deriving (Eq,Show)
data DestinationNameTypeAttributes = DestinationNameTypeAttributes
    { destinationNameTypeAttributes_schemeID :: Maybe Iso6523cid
    }
    deriving (Eq,Show)
instance SchemaType DestinationNameType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- optional $ getAttribute "SchemeID" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ DestinationNameType v (DestinationNameTypeAttributes a0)
    schemaTypeToXML s (DestinationNameType bt at) =
        addXMLAttributes [ maybe [] (toXMLAttribute "SchemeID") $ destinationNameTypeAttributes_schemeID at
                         ]
            $ schemaTypeToXML s bt
instance Extension DestinationNameType GenericStringType0_35 where
    supertype (DestinationNameType s _) = s
 
data ArticleGroupIdentifierType = ArticleGroupIdentifierType GenericStringType0_70 ArticleGroupIdentifierTypeAttributes deriving (Eq,Show)
data ArticleGroupIdentifierTypeAttributes = ArticleGroupIdentifierTypeAttributes
    { articleGroupIdentifierTypeAttributes_schemeID :: Maybe Untdid7143
    , articleGroupIdentifierTypeAttributes_schemeVersion :: Maybe GenericStringType1_35
    }
    deriving (Eq,Show)
instance SchemaType ArticleGroupIdentifierType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- optional $ getAttribute "SchemeID" e pos
          a1 <- optional $ getAttribute "SchemeVersion" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ ArticleGroupIdentifierType v (ArticleGroupIdentifierTypeAttributes a0 a1)
    schemaTypeToXML s (ArticleGroupIdentifierType bt at) =
        addXMLAttributes [ maybe [] (toXMLAttribute "SchemeID") $ articleGroupIdentifierTypeAttributes_schemeID at
                         , maybe [] (toXMLAttribute "SchemeVersion") $ articleGroupIdentifierTypeAttributes_schemeVersion at
                         ]
            $ schemaTypeToXML s bt
instance Extension ArticleGroupIdentifierType GenericStringType0_70 where
    supertype (ArticleGroupIdentifierType s _) = s
 
data InvoicedObjectIDType = InvoicedObjectIDType GenericStringType1_70 InvoicedObjectIDTypeAttributes deriving (Eq,Show)
data InvoicedObjectIDTypeAttributes = InvoicedObjectIDTypeAttributes
    { invoicedObjectIDTypeAttributes_schemeID :: Maybe Untdid1153
    }
    deriving (Eq,Show)
instance SchemaType InvoicedObjectIDType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- optional $ getAttribute "SchemeID" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ InvoicedObjectIDType v (InvoicedObjectIDTypeAttributes a0)
    schemaTypeToXML s (InvoicedObjectIDType bt at) =
        addXMLAttributes [ maybe [] (toXMLAttribute "SchemeID") $ invoicedObjectIDTypeAttributes_schemeID at
                         ]
            $ schemaTypeToXML s bt
instance Extension InvoicedObjectIDType GenericStringType1_70 where
    supertype (InvoicedObjectIDType s _) = s
 
newtype CapAZ09 = CapAZ09 Xs.NMTOKEN deriving (Eq,Show)
instance Restricts CapAZ09 Xs.NMTOKEN where
    restricts (CapAZ09 x) = x
instance SchemaType CapAZ09 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (CapAZ09 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CapAZ09 where
    acceptingParser = fmap CapAZ09 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Z0-9]*)
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (CapAZ09 x) = simpleTypeText x
 
newtype Untdid1_3_AZ09 = Untdid1_3_AZ09 CapAZ09 deriving (Eq,Show)
instance Restricts Untdid1_3_AZ09 CapAZ09 where
    restricts (Untdid1_3_AZ09 x) = x
instance SchemaType Untdid1_3_AZ09 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (Untdid1_3_AZ09 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Untdid1_3_AZ09 where
    acceptingParser = fmap Untdid1_3_AZ09 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (Untdid1_3_AZ09 x) = simpleTypeText x
 
newtype Untdid1001 = Untdid1001 Untdid1_3_AZ09 deriving (Eq,Show)
instance Restricts Untdid1001 Untdid1_3_AZ09 where
    restricts (Untdid1001 x) = x
instance SchemaType Untdid1001 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (Untdid1001 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Untdid1001 where
    acceptingParser = fmap Untdid1001 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (Untdid1001 x) = simpleTypeText x
 
newtype Untdid1153 = Untdid1153 Untdid1_3_AZ09 deriving (Eq,Show)
instance Restricts Untdid1153 Untdid1_3_AZ09 where
    restricts (Untdid1153 x) = x
instance SchemaType Untdid1153 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (Untdid1153 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Untdid1153 where
    acceptingParser = fmap Untdid1153 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (Untdid1153 x) = simpleTypeText x
 
newtype Untdid2005 = Untdid2005 Untdid1_3_AZ09 deriving (Eq,Show)
instance Restricts Untdid2005 Untdid1_3_AZ09 where
    restricts (Untdid2005 x) = x
instance SchemaType Untdid2005 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (Untdid2005 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Untdid2005 where
    acceptingParser = fmap Untdid2005 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (Untdid2005 x) = simpleTypeText x
 
newtype Untdid4461 = Untdid4461 Untdid1_3_AZ09 deriving (Eq,Show)
instance Restricts Untdid4461 Untdid1_3_AZ09 where
    restricts (Untdid4461 x) = x
instance SchemaType Untdid4461 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (Untdid4461 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Untdid4461 where
    acceptingParser = fmap Untdid4461 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (Untdid4461 x) = simpleTypeText x
 
newtype Untdid5189 = Untdid5189 Untdid1_3_AZ09 deriving (Eq,Show)
instance Restricts Untdid5189 Untdid1_3_AZ09 where
    restricts (Untdid5189 x) = x
instance SchemaType Untdid5189 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (Untdid5189 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Untdid5189 where
    acceptingParser = fmap Untdid5189 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (Untdid5189 x) = simpleTypeText x
 
newtype Untdid5305 = Untdid5305 Untdid1_3_AZ09 deriving (Eq,Show)
instance Restricts Untdid5305 Untdid1_3_AZ09 where
    restricts (Untdid5305 x) = x
instance SchemaType Untdid5305 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (Untdid5305 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Untdid5305 where
    acceptingParser = fmap Untdid5305 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (Untdid5305 x) = simpleTypeText x
 
newtype Untdid7143 = Untdid7143 Untdid1_3_AZ09 deriving (Eq,Show)
instance Restricts Untdid7143 Untdid1_3_AZ09 where
    restricts (Untdid7143 x) = x
instance SchemaType Untdid7143 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (Untdid7143 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Untdid7143 where
    acceptingParser = fmap Untdid7143 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (Untdid7143 x) = simpleTypeText x
 
newtype Untdid7161 = Untdid7161 Untdid1_3_AZ09 deriving (Eq,Show)
instance Restricts Untdid7161 Untdid1_3_AZ09 where
    restricts (Untdid7161 x) = x
instance SchemaType Untdid7161 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (Untdid7161 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Untdid7161 where
    acceptingParser = fmap Untdid7161 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (Untdid7161 x) = simpleTypeText x
 
newtype ElectronicAddrSchemeIdType = ElectronicAddrSchemeIdType Xs.NMTOKEN deriving (Eq,Show)
instance Restricts ElectronicAddrSchemeIdType Xs.NMTOKEN where
    restricts (ElectronicAddrSchemeIdType x) = x
instance SchemaType ElectronicAddrSchemeIdType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (ElectronicAddrSchemeIdType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ElectronicAddrSchemeIdType where
    acceptingParser = fmap ElectronicAddrSchemeIdType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ElectronicAddrSchemeIdType x) = simpleTypeText x
 
data ElectronicAddrIdType = ElectronicAddrIdType GenericStringType2_35 ElectronicAddrIdTypeAttributes deriving (Eq,Show)
data ElectronicAddrIdTypeAttributes = ElectronicAddrIdTypeAttributes
    { electronicAddrIdTypeAttributes_schemeID :: Maybe ElectronicAddrSchemeIdType
    }
    deriving (Eq,Show)
instance SchemaType ElectronicAddrIdType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- optional $ getAttribute "SchemeID" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ ElectronicAddrIdType v (ElectronicAddrIdTypeAttributes a0)
    schemaTypeToXML s (ElectronicAddrIdType bt at) =
        addXMLAttributes [ maybe [] (toXMLAttribute "SchemeID") $ electronicAddrIdTypeAttributes_schemeID at
                         ]
            $ schemaTypeToXML s bt
instance Extension ElectronicAddrIdType GenericStringType2_35 where
    supertype (ElectronicAddrIdType s _) = s
 
newtype UnitCodeUN = UnitCodeUN Xs.NMTOKEN deriving (Eq,Show)
instance Restricts UnitCodeUN Xs.NMTOKEN where
    restricts (UnitCodeUN x) = x
instance SchemaType UnitCodeUN where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (UnitCodeUN x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType UnitCodeUN where
    acceptingParser = fmap UnitCodeUN acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Z0-9]{2,3})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (UnitCodeUN x) = simpleTypeText x
