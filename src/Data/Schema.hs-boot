{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.Xmldsig'core'schema
  ( module Data.Xmldsig'core'schema
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
 
newtype CryptoBinary = CryptoBinary Base64Binary
instance Eq CryptoBinary
instance Show CryptoBinary
instance Restricts CryptoBinary Base64Binary
instance SchemaType CryptoBinary
instance SimpleType CryptoBinary
 
elementSignature :: XMLParser SignatureType
elementToXMLSignature :: SignatureType -> [Content ()]
 
data SignatureType
instance Eq SignatureType
instance Show SignatureType
instance SchemaType SignatureType
 
elementSignatureValue :: XMLParser SignatureValueType
elementToXMLSignatureValue :: SignatureValueType -> [Content ()]
 
data SignatureValueType
data SignatureValueTypeAttributes
instance Eq SignatureValueType
instance Eq SignatureValueTypeAttributes
instance Show SignatureValueType
instance Show SignatureValueTypeAttributes
instance SchemaType SignatureValueType
instance Extension SignatureValueType Base64Binary
 
elementSignedInfo :: XMLParser SignedInfoType
elementToXMLSignedInfo :: SignedInfoType -> [Content ()]
 
data SignedInfoType
instance Eq SignedInfoType
instance Show SignedInfoType
instance SchemaType SignedInfoType
 
elementCanonicalizationMethod :: XMLParser CanonicalizationMethodType
elementToXMLCanonicalizationMethod :: CanonicalizationMethodType -> [Content ()]
 
data CanonicalizationMethodType
instance Eq CanonicalizationMethodType
instance Show CanonicalizationMethodType
instance SchemaType CanonicalizationMethodType
 
elementSignatureMethod :: XMLParser SignatureMethodType
elementToXMLSignatureMethod :: SignatureMethodType -> [Content ()]
 
data SignatureMethodType
instance Eq SignatureMethodType
instance Show SignatureMethodType
instance SchemaType SignatureMethodType
 
elementReference :: XMLParser ReferenceType
elementToXMLReference :: ReferenceType -> [Content ()]
 
data ReferenceType
instance Eq ReferenceType
instance Show ReferenceType
instance SchemaType ReferenceType
 
elementTransforms :: XMLParser TransformsType
elementToXMLTransforms :: TransformsType -> [Content ()]
 
data TransformsType
instance Eq TransformsType
instance Show TransformsType
instance SchemaType TransformsType
 
elementTransform :: XMLParser TransformType
elementToXMLTransform :: TransformType -> [Content ()]
 
data TransformType
instance Eq TransformType
instance Show TransformType
instance SchemaType TransformType
 
elementDigestMethod :: XMLParser DigestMethodType
elementToXMLDigestMethod :: DigestMethodType -> [Content ()]
 
data DigestMethodType
instance Eq DigestMethodType
instance Show DigestMethodType
instance SchemaType DigestMethodType
 
elementDigestValue :: XMLParser DigestValueType
elementToXMLDigestValue :: DigestValueType -> [Content ()]
 
newtype DigestValueType = DigestValueType Base64Binary
instance Eq DigestValueType
instance Show DigestValueType
instance Restricts DigestValueType Base64Binary
instance SchemaType DigestValueType
instance SimpleType DigestValueType
 
elementKeyInfo :: XMLParser KeyInfoType
elementToXMLKeyInfo :: KeyInfoType -> [Content ()]
 
data KeyInfoType
instance Eq KeyInfoType
instance Show KeyInfoType
instance SchemaType KeyInfoType
 
elementKeyName :: XMLParser Xsd.XsdString
elementToXMLKeyName :: Xsd.XsdString -> [Content ()]
 
elementMgmtData :: XMLParser Xsd.XsdString
elementToXMLMgmtData :: Xsd.XsdString -> [Content ()]
 
elementKeyValue :: XMLParser KeyValueType
elementToXMLKeyValue :: KeyValueType -> [Content ()]
 
data KeyValueType
instance Eq KeyValueType
instance Show KeyValueType
instance SchemaType KeyValueType
 
elementRetrievalMethod :: XMLParser RetrievalMethodType
elementToXMLRetrievalMethod :: RetrievalMethodType -> [Content ()]
 
data RetrievalMethodType
instance Eq RetrievalMethodType
instance Show RetrievalMethodType
instance SchemaType RetrievalMethodType
 
elementX509Data :: XMLParser X509DataType
elementToXMLX509Data :: X509DataType -> [Content ()]
 
data X509DataType
instance Eq X509DataType
instance Show X509DataType
instance SchemaType X509DataType
 
data X509IssuerSerialType
instance Eq X509IssuerSerialType
instance Show X509IssuerSerialType
instance SchemaType X509IssuerSerialType
 
elementPGPData :: XMLParser PGPDataType
elementToXMLPGPData :: PGPDataType -> [Content ()]
 
data PGPDataType
instance Eq PGPDataType
instance Show PGPDataType
instance SchemaType PGPDataType
 
elementSPKIData :: XMLParser SPKIDataType
elementToXMLSPKIData :: SPKIDataType -> [Content ()]
 
data SPKIDataType
instance Eq SPKIDataType
instance Show SPKIDataType
instance SchemaType SPKIDataType
 
elementObject :: XMLParser ObjectType
elementToXMLObject :: ObjectType -> [Content ()]
 
data ObjectType
instance Eq ObjectType
instance Show ObjectType
instance SchemaType ObjectType
 
elementManifest :: XMLParser ManifestType
elementToXMLManifest :: ManifestType -> [Content ()]
 
data ManifestType
instance Eq ManifestType
instance Show ManifestType
instance SchemaType ManifestType
 
elementSignatureProperties :: XMLParser SignaturePropertiesType
elementToXMLSignatureProperties :: SignaturePropertiesType -> [Content ()]
 
data SignaturePropertiesType
instance Eq SignaturePropertiesType
instance Show SignaturePropertiesType
instance SchemaType SignaturePropertiesType
 
elementSignatureProperty :: XMLParser SignaturePropertyType
elementToXMLSignatureProperty :: SignaturePropertyType -> [Content ()]
 
data SignaturePropertyType
instance Eq SignaturePropertyType
instance Show SignaturePropertyType
instance SchemaType SignaturePropertyType
 
newtype HMACOutputLengthType = HMACOutputLengthType Integer
instance Eq HMACOutputLengthType
instance Show HMACOutputLengthType
instance Restricts HMACOutputLengthType Integer
instance SchemaType HMACOutputLengthType
instance SimpleType HMACOutputLengthType
 
elementDSAKeyValue :: XMLParser DSAKeyValueType
elementToXMLDSAKeyValue :: DSAKeyValueType -> [Content ()]
 
data DSAKeyValueType
instance Eq DSAKeyValueType
instance Show DSAKeyValueType
instance SchemaType DSAKeyValueType
 
elementRSAKeyValue :: XMLParser RSAKeyValueType
elementToXMLRSAKeyValue :: RSAKeyValueType -> [Content ()]
 
data RSAKeyValueType
instance Eq RSAKeyValueType
instance Show RSAKeyValueType
instance SchemaType RSAKeyValueType
