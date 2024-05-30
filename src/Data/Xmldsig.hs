{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.Xmldsig
  ( module Data.Xmldsig
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
newtype CryptoBinary = CryptoBinary Base64Binary deriving (Eq,Show)
instance Restricts CryptoBinary Base64Binary where
    restricts (CryptoBinary x) = x
instance SchemaType CryptoBinary where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (CryptoBinary x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CryptoBinary where
    acceptingParser = fmap CryptoBinary acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (CryptoBinary x) = simpleTypeText x
 
elementSignature :: XMLParser SignatureType
elementSignature = parseSchemaType "Signature"
elementToXMLSignature :: SignatureType -> [Content ()]
elementToXMLSignature = schemaTypeToXML "Signature"
 
data SignatureType = SignatureType
        { signatType_id :: Maybe ID
        , signatType_signedInfo :: SignedInfoType
        , signatType_signatureValue :: SignatureValueType
        , signatType_keyInfo :: Maybe KeyInfoType
        , signatType_object :: [ObjectType]
        }
        deriving (Eq,Show)
instance SchemaType SignatureType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "Id" e pos
        commit $ interior e $ return (SignatureType a0)
            `apply` elementSignedInfo
            `apply` elementSignatureValue
            `apply` optional (elementKeyInfo)
            `apply` many (elementObject)
    schemaTypeToXML s x@SignatureType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "Id") $ signatType_id x
                       ]
            [ elementToXMLSignedInfo $ signatType_signedInfo x
            , elementToXMLSignatureValue $ signatType_signatureValue x
            , maybe [] (elementToXMLKeyInfo) $ signatType_keyInfo x
            , concatMap (elementToXMLObject) $ signatType_object x
            ]
 
elementSignatureValue :: XMLParser SignatureValueType
elementSignatureValue = parseSchemaType "SignatureValue"
elementToXMLSignatureValue :: SignatureValueType -> [Content ()]
elementToXMLSignatureValue = schemaTypeToXML "SignatureValue"
 
data SignatureValueType = SignatureValueType Base64Binary SignatureValueTypeAttributes deriving (Eq,Show)
data SignatureValueTypeAttributes = SignatureValueTypeAttributes
    { signatValueTypeAttrib_id :: Maybe ID
    }
    deriving (Eq,Show)
instance SchemaType SignatureValueType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- optional $ getAttribute "Id" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ SignatureValueType v (SignatureValueTypeAttributes a0)
    schemaTypeToXML s (SignatureValueType bt at) =
        addXMLAttributes [ maybe [] (toXMLAttribute "Id") $ signatValueTypeAttrib_id at
                         ]
            $ schemaTypeToXML s bt
instance Extension SignatureValueType Base64Binary where
    supertype (SignatureValueType s _) = s
 
elementSignedInfo :: XMLParser SignedInfoType
elementSignedInfo = parseSchemaType "SignedInfo"
elementToXMLSignedInfo :: SignedInfoType -> [Content ()]
elementToXMLSignedInfo = schemaTypeToXML "SignedInfo"
 
data SignedInfoType = SignedInfoType
        { signedInfoType_id :: Maybe ID
        , signedInfoType_canonicalizationMethod :: CanonicalizationMethodType
        , signedInfoType_signatureMethod :: SignatureMethodType
        , signedInfoType_reference :: [ReferenceType]
        }
        deriving (Eq,Show)
instance SchemaType SignedInfoType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "Id" e pos
        commit $ interior e $ return (SignedInfoType a0)
            `apply` elementCanonicalizationMethod
            `apply` elementSignatureMethod
            `apply` many1 (elementReference)
    schemaTypeToXML s x@SignedInfoType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "Id") $ signedInfoType_id x
                       ]
            [ elementToXMLCanonicalizationMethod $ signedInfoType_canonicalizationMethod x
            , elementToXMLSignatureMethod $ signedInfoType_signatureMethod x
            , concatMap (elementToXMLReference) $ signedInfoType_reference x
            ]
 
elementCanonicalizationMethod :: XMLParser CanonicalizationMethodType
elementCanonicalizationMethod = parseSchemaType "CanonicalizationMethod"
elementToXMLCanonicalizationMethod :: CanonicalizationMethodType -> [Content ()]
elementToXMLCanonicalizationMethod = schemaTypeToXML "CanonicalizationMethod"
 
data CanonicalizationMethodType = CanonicalizationMethodType
        { canonMethodType_algorithm :: AnyURI
        , canonMethodType_text0 :: String
        , canonMethodType_any1 :: [AnyElement]
        , canonMethodType_text2 :: String
        }
        deriving (Eq,Show)
instance SchemaType CanonicalizationMethodType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "Algorithm" e pos
        commit $ interior e $ return (CanonicalizationMethodType a0)
            `apply` parseText
            `apply` many (parseAnyElement)
            `apply` parseText
    schemaTypeToXML s x@CanonicalizationMethodType{} =
        toXMLElement s [ toXMLAttribute "Algorithm" $ canonMethodType_algorithm x
                       ]
            [ toXMLText $ canonMethodType_text0 x
            , concatMap (toXMLAnyElement) $ canonMethodType_any1 x
            , toXMLText $ canonMethodType_text2 x
            ]
 
elementSignatureMethod :: XMLParser SignatureMethodType
elementSignatureMethod = parseSchemaType "SignatureMethod"
elementToXMLSignatureMethod :: SignatureMethodType -> [Content ()]
elementToXMLSignatureMethod = schemaTypeToXML "SignatureMethod"
 
data SignatureMethodType = SignatureMethodType
        { signatMethodType_algorithm :: AnyURI
        , signatMethodType_text0 :: String
        , signatMethodType_hMACOutputLength :: Maybe HMACOutputLengthType
        , signatMethodType_text2 :: String
        , signatMethodType_any3 :: [AnyElement]
        , signatMethodType_text4 :: String
        }
        deriving (Eq,Show)
instance SchemaType SignatureMethodType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "Algorithm" e pos
        commit $ interior e $ return (SignatureMethodType a0)
            `apply` parseText
            `apply` optional (parseSchemaType "HMACOutputLength")
            `apply` parseText
            `apply` many (parseAnyElement)
            `apply` parseText
    schemaTypeToXML s x@SignatureMethodType{} =
        toXMLElement s [ toXMLAttribute "Algorithm" $ signatMethodType_algorithm x
                       ]
            [ toXMLText $ signatMethodType_text0 x
            , maybe [] (schemaTypeToXML "HMACOutputLength") $ signatMethodType_hMACOutputLength x
            , toXMLText $ signatMethodType_text2 x
            , concatMap (toXMLAnyElement) $ signatMethodType_any3 x
            , toXMLText $ signatMethodType_text4 x
            ]
 
elementReference :: XMLParser ReferenceType
elementReference = parseSchemaType "Reference"
elementToXMLReference :: ReferenceType -> [Content ()]
elementToXMLReference = schemaTypeToXML "Reference"
 
data ReferenceType = ReferenceType
        { refType_id :: Maybe ID
        , refType_uRI :: Maybe AnyURI
        , refType_type :: Maybe AnyURI
        , refType_transforms :: Maybe TransformsType
        , refType_digestMethod :: DigestMethodType
        , refType_digestValue :: DigestValueType
        }
        deriving (Eq,Show)
instance SchemaType ReferenceType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "Id" e pos
        a1 <- optional $ getAttribute "URI" e pos
        a2 <- optional $ getAttribute "Type" e pos
        commit $ interior e $ return (ReferenceType a0 a1 a2)
            `apply` optional (elementTransforms)
            `apply` elementDigestMethod
            `apply` elementDigestValue
    schemaTypeToXML s x@ReferenceType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "Id") $ refType_id x
                       , maybe [] (toXMLAttribute "URI") $ refType_uRI x
                       , maybe [] (toXMLAttribute "Type") $ refType_type x
                       ]
            [ maybe [] (elementToXMLTransforms) $ refType_transforms x
            , elementToXMLDigestMethod $ refType_digestMethod x
            , elementToXMLDigestValue $ refType_digestValue x
            ]
 
elementTransforms :: XMLParser TransformsType
elementTransforms = parseSchemaType "Transforms"
elementToXMLTransforms :: TransformsType -> [Content ()]
elementToXMLTransforms = schemaTypeToXML "Transforms"
 
data TransformsType = TransformsType
        { transfType_transform :: [TransformType]
        }
        deriving (Eq,Show)
instance SchemaType TransformsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return TransformsType
            `apply` many1 (elementTransform)
    schemaTypeToXML s x@TransformsType{} =
        toXMLElement s []
            [ concatMap (elementToXMLTransform) $ transfType_transform x
            ]
 
elementTransform :: XMLParser TransformType
elementTransform = parseSchemaType "Transform"
elementToXMLTransform :: TransformType -> [Content ()]
elementToXMLTransform = schemaTypeToXML "Transform"
 
data TransformType = TransformType
        { transfType_algorithm :: AnyURI
        , transfType_choice0 :: [OneOf3 String Xsd.XsdString (AnyElement)]
          -- ^ Choice between:
          --   
          --   (1) mixed text
          --   
          --   (2) XPath
          --   
          --   (3) unknown
        }
        deriving (Eq,Show)
instance SchemaType TransformType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "Algorithm" e pos
        commit $ interior e $ return (TransformType a0)
            `apply` many (oneOf' [ ("String", fmap OneOf3 (parseText))
                                 , ("Xsd.XsdString", fmap TwoOf3 (parseSchemaType "XPath"))
                                 , ("AnyElement", fmap ThreeOf3 (parseAnyElement))
                                 ])
    schemaTypeToXML s x@TransformType{} =
        toXMLElement s [ toXMLAttribute "Algorithm" $ transfType_algorithm x
                       ]
            [ concatMap (foldOneOf3  (toXMLText)
                                     (schemaTypeToXML "XPath")
                                     (toXMLAnyElement)
                                    ) $ transfType_choice0 x
            ]
 
elementDigestMethod :: XMLParser DigestMethodType
elementDigestMethod = parseSchemaType "DigestMethod"
elementToXMLDigestMethod :: DigestMethodType -> [Content ()]
elementToXMLDigestMethod = schemaTypeToXML "DigestMethod"
 
data DigestMethodType = DigestMethodType
        { digestMethodType_algorithm :: AnyURI
        , digestMethodType_text0 :: String
        , digestMethodType_any1 :: [AnyElement]
        , digestMethodType_text2 :: String
        }
        deriving (Eq,Show)
instance SchemaType DigestMethodType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "Algorithm" e pos
        commit $ interior e $ return (DigestMethodType a0)
            `apply` parseText
            `apply` many (parseAnyElement)
            `apply` parseText
    schemaTypeToXML s x@DigestMethodType{} =
        toXMLElement s [ toXMLAttribute "Algorithm" $ digestMethodType_algorithm x
                       ]
            [ toXMLText $ digestMethodType_text0 x
            , concatMap (toXMLAnyElement) $ digestMethodType_any1 x
            , toXMLText $ digestMethodType_text2 x
            ]
 
elementDigestValue :: XMLParser DigestValueType
elementDigestValue = parseSchemaType "DigestValue"
elementToXMLDigestValue :: DigestValueType -> [Content ()]
elementToXMLDigestValue = schemaTypeToXML "DigestValue"
 
newtype DigestValueType = DigestValueType Base64Binary deriving (Eq,Show)
instance Restricts DigestValueType Base64Binary where
    restricts (DigestValueType x) = x
instance SchemaType DigestValueType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (DigestValueType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DigestValueType where
    acceptingParser = fmap DigestValueType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (DigestValueType x) = simpleTypeText x
 
elementKeyInfo :: XMLParser KeyInfoType
elementKeyInfo = parseSchemaType "KeyInfo"
elementToXMLKeyInfo :: KeyInfoType -> [Content ()]
elementToXMLKeyInfo = schemaTypeToXML "KeyInfo"
 
data KeyInfoType = KeyInfoType
        { keyInfoType_id :: Maybe ID
        , keyInfoType_choice0 :: [OneOf9 String Xsd.XsdString KeyValueType RetrievalMethodType X509DataType PGPDataType SPKIDataType Xsd.XsdString (AnyElement)]
          -- ^ Choice between:
          --   
          --   (1) mixed text
          --   
          --   (2) KeyName
          --   
          --   (3) KeyValue
          --   
          --   (4) RetrievalMethod
          --   
          --   (5) X509Data
          --   
          --   (6) PGPData
          --   
          --   (7) SPKIData
          --   
          --   (8) MgmtData
          --   
          --   (9) unknown
        }
        deriving (Eq,Show)
instance SchemaType KeyInfoType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "Id" e pos
        commit $ interior e $ return (KeyInfoType a0)
            `apply` many1 (oneOf' [ ("String", fmap OneOf9 (parseText))
                                  , ("Xsd.XsdString", fmap TwoOf9 (elementKeyName))
                                  , ("KeyValueType", fmap ThreeOf9 (elementKeyValue))
                                  , ("RetrievalMethodType", fmap FourOf9 (elementRetrievalMethod))
                                  , ("X509DataType", fmap FiveOf9 (elementX509Data))
                                  , ("PGPDataType", fmap SixOf9 (elementPGPData))
                                  , ("SPKIDataType", fmap SevenOf9 (elementSPKIData))
                                  , ("Xsd.XsdString", fmap EightOf9 (elementMgmtData))
                                  , ("AnyElement", fmap NineOf9 (parseAnyElement))
                                  ])
    schemaTypeToXML s x@KeyInfoType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "Id") $ keyInfoType_id x
                       ]
            [ concatMap (foldOneOf9  (toXMLText)
                                     (elementToXMLKeyName)
                                     (elementToXMLKeyValue)
                                     (elementToXMLRetrievalMethod)
                                     (elementToXMLX509Data)
                                     (elementToXMLPGPData)
                                     (elementToXMLSPKIData)
                                     (elementToXMLMgmtData)
                                     (toXMLAnyElement)
                                    ) $ keyInfoType_choice0 x
            ]
 
elementKeyName :: XMLParser Xsd.XsdString
elementKeyName = parseSchemaType "KeyName"
elementToXMLKeyName :: Xsd.XsdString -> [Content ()]
elementToXMLKeyName = schemaTypeToXML "KeyName"
 
elementMgmtData :: XMLParser Xsd.XsdString
elementMgmtData = parseSchemaType "MgmtData"
elementToXMLMgmtData :: Xsd.XsdString -> [Content ()]
elementToXMLMgmtData = schemaTypeToXML "MgmtData"
 
elementKeyValue :: XMLParser KeyValueType
elementKeyValue = parseSchemaType "KeyValue"
elementToXMLKeyValue :: KeyValueType -> [Content ()]
elementToXMLKeyValue = schemaTypeToXML "KeyValue"
 
data KeyValueType = KeyValueType
        { keyValueType_choice0 :: OneOf4 String DSAKeyValueType RSAKeyValueType (AnyElement)
          -- ^ Choice between:
          --   
          --   (1) mixed text
          --   
          --   (2) DSAKeyValue
          --   
          --   (3) RSAKeyValue
          --   
          --   (4) unknown
        }
        deriving (Eq,Show)
instance SchemaType KeyValueType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return KeyValueType
            `apply` oneOf' [ ("String", fmap OneOf4 (parseText))
                           , ("DSAKeyValueType", fmap TwoOf4 (elementDSAKeyValue))
                           , ("RSAKeyValueType", fmap ThreeOf4 (elementRSAKeyValue))
                           , ("AnyElement", fmap FourOf4 (parseAnyElement))
                           ]
    schemaTypeToXML s x@KeyValueType{} =
        toXMLElement s []
            [ foldOneOf4  (toXMLText)
                          (elementToXMLDSAKeyValue)
                          (elementToXMLRSAKeyValue)
                          (toXMLAnyElement)
                          $ keyValueType_choice0 x
            ]
 
elementRetrievalMethod :: XMLParser RetrievalMethodType
elementRetrievalMethod = parseSchemaType "RetrievalMethod"
elementToXMLRetrievalMethod :: RetrievalMethodType -> [Content ()]
elementToXMLRetrievalMethod = schemaTypeToXML "RetrievalMethod"
 
data RetrievalMethodType = RetrievalMethodType
        { retriMethodType_uRI :: Maybe AnyURI
        , retriMethodType_type :: Maybe AnyURI
        , retriMethodType_transforms :: Maybe TransformsType
        }
        deriving (Eq,Show)
instance SchemaType RetrievalMethodType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "URI" e pos
        a1 <- optional $ getAttribute "Type" e pos
        commit $ interior e $ return (RetrievalMethodType a0 a1)
            `apply` optional (elementTransforms)
    schemaTypeToXML s x@RetrievalMethodType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "URI") $ retriMethodType_uRI x
                       , maybe [] (toXMLAttribute "Type") $ retriMethodType_type x
                       ]
            [ maybe [] (elementToXMLTransforms) $ retriMethodType_transforms x
            ]
 
elementX509Data :: XMLParser X509DataType
elementX509Data = parseSchemaType "X509Data"
elementToXMLX509Data :: X509DataType -> [Content ()]
elementToXMLX509Data = schemaTypeToXML "X509Data"
 
data X509DataType = X509DataType
        { x509DataType_choice0 :: OneOf6 X509IssuerSerialType Base64Binary Xsd.XsdString Base64Binary Base64Binary (AnyElement)
          -- ^ Choice between:
          --   
          --   (1) X509IssuerSerial
          --   
          --   (2) X509SKI
          --   
          --   (3) X509SubjectName
          --   
          --   (4) X509Certificate
          --   
          --   (5) X509CRL
          --   
          --   (6) unknown
        }
        deriving (Eq,Show)
instance SchemaType X509DataType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return X509DataType
            `apply` oneOf' [ ("X509IssuerSerialType", fmap OneOf6 (parseSchemaType "X509IssuerSerial"))
                           , ("Base64Binary", fmap TwoOf6 (parseSchemaType "X509SKI"))
                           , ("Xsd.XsdString", fmap ThreeOf6 (parseSchemaType "X509SubjectName"))
                           , ("Base64Binary", fmap FourOf6 (parseSchemaType "X509Certificate"))
                           , ("Base64Binary", fmap FiveOf6 (parseSchemaType "X509CRL"))
                           , ("AnyElement", fmap SixOf6 (parseAnyElement))
                           ]
    schemaTypeToXML s x@X509DataType{} =
        toXMLElement s []
            [ foldOneOf6  (schemaTypeToXML "X509IssuerSerial")
                          (schemaTypeToXML "X509SKI")
                          (schemaTypeToXML "X509SubjectName")
                          (schemaTypeToXML "X509Certificate")
                          (schemaTypeToXML "X509CRL")
                          (toXMLAnyElement)
                          $ x509DataType_choice0 x
            ]
 
data X509IssuerSerialType = X509IssuerSerialType
        { x509IssuerSerialType_x509IssuerName :: Xsd.XsdString
        , x509IssuerSerialType_x509SerialNumber :: Integer
        }
        deriving (Eq,Show)
instance SchemaType X509IssuerSerialType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return X509IssuerSerialType
            `apply` parseSchemaType "X509IssuerName"
            `apply` parseSchemaType "X509SerialNumber"
    schemaTypeToXML s x@X509IssuerSerialType{} =
        toXMLElement s []
            [ schemaTypeToXML "X509IssuerName" $ x509IssuerSerialType_x509IssuerName x
            , schemaTypeToXML "X509SerialNumber" $ x509IssuerSerialType_x509SerialNumber x
            ]
 
elementPGPData :: XMLParser PGPDataType
elementPGPData = parseSchemaType "PGPData"
elementToXMLPGPData :: PGPDataType -> [Content ()]
elementToXMLPGPData = schemaTypeToXML "PGPData"
 
data PGPDataType = PGPDataType
        { pGPDataType_choice0 :: OneOf2 (Base64Binary,(Maybe (Base64Binary)),([AnyElement])) (Base64Binary,([AnyElement]))
          -- ^ Choice between:
          --   
          --   (1) Sequence of:
          --   
          --     * PGPKeyID
          --   
          --     * PGPKeyPacket
          --   
          --     * unknown
          --   
          --   (2) Sequence of:
          --   
          --     * PGPKeyPacket
          --   
          --     * unknown
        }
        deriving (Eq,Show)
instance SchemaType PGPDataType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return PGPDataType
            `apply` oneOf' [ ("Base64Binary Maybe Base64Binary [AnyElement]", fmap OneOf2 (return (,,) `apply` parseSchemaType "PGPKeyID"
                                                                                                       `apply` optional (parseSchemaType "PGPKeyPacket")
                                                                                                       `apply` many (parseAnyElement)))
                           , ("Base64Binary [AnyElement]", fmap TwoOf2 (return (,) `apply` parseSchemaType "PGPKeyPacket"
                                                                                   `apply` many (parseAnyElement)))
                           ]
    schemaTypeToXML s x@PGPDataType{} =
        toXMLElement s []
            [ foldOneOf2  (\ (a,b,c) -> concat [ schemaTypeToXML "PGPKeyID" a
                                               , maybe [] (schemaTypeToXML "PGPKeyPacket") b
                                               , concatMap (toXMLAnyElement) c
                                               ])
                          (\ (a,b) -> concat [ schemaTypeToXML "PGPKeyPacket" a
                                             , concatMap (toXMLAnyElement) b
                                             ])
                          $ pGPDataType_choice0 x
            ]
 
elementSPKIData :: XMLParser SPKIDataType
elementSPKIData = parseSchemaType "SPKIData"
elementToXMLSPKIData :: SPKIDataType -> [Content ()]
elementToXMLSPKIData = schemaTypeToXML "SPKIData"
 
data SPKIDataType = SPKIDataType
        { sPKIDataType_sPKISexp :: Base64Binary
        , sPKIDataType_any1 :: Maybe AnyElement
        }
        deriving (Eq,Show)
instance SchemaType SPKIDataType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return SPKIDataType
            `apply` parseSchemaType "SPKISexp"
            `apply` optional (parseAnyElement)
    schemaTypeToXML s x@SPKIDataType{} =
        toXMLElement s []
            [ schemaTypeToXML "SPKISexp" $ sPKIDataType_sPKISexp x
            , maybe [] (toXMLAnyElement) $ sPKIDataType_any1 x
            ]
 
elementObject :: XMLParser ObjectType
elementObject = parseSchemaType "Object"
elementToXMLObject :: ObjectType -> [Content ()]
elementToXMLObject = schemaTypeToXML "Object"
 
data ObjectType = ObjectType
        { objectType_id :: Maybe ID
        , objectType_mimeType :: Maybe Xsd.XsdString
        , objectType_encoding :: Maybe AnyURI
        , objectType_text0 :: String
        , objectType_any1 :: AnyElement
        , objectType_text2 :: String
        }
        deriving (Eq,Show)
instance SchemaType ObjectType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "Id" e pos
        a1 <- optional $ getAttribute "MimeType" e pos
        a2 <- optional $ getAttribute "Encoding" e pos
        commit $ interior e $ return (ObjectType a0 a1 a2)
            `apply` parseText
            `apply` parseAnyElement
            `apply` parseText
    schemaTypeToXML s x@ObjectType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "Id") $ objectType_id x
                       , maybe [] (toXMLAttribute "MimeType") $ objectType_mimeType x
                       , maybe [] (toXMLAttribute "Encoding") $ objectType_encoding x
                       ]
            [ toXMLText $ objectType_text0 x
            , toXMLAnyElement $ objectType_any1 x
            , toXMLText $ objectType_text2 x
            ]
 
elementManifest :: XMLParser ManifestType
elementManifest = parseSchemaType "Manifest"
elementToXMLManifest :: ManifestType -> [Content ()]
elementToXMLManifest = schemaTypeToXML "Manifest"
 
data ManifestType = ManifestType
        { manifestType_id :: Maybe ID
        , manifestType_reference :: [ReferenceType]
        }
        deriving (Eq,Show)
instance SchemaType ManifestType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "Id" e pos
        commit $ interior e $ return (ManifestType a0)
            `apply` many1 (elementReference)
    schemaTypeToXML s x@ManifestType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "Id") $ manifestType_id x
                       ]
            [ concatMap (elementToXMLReference) $ manifestType_reference x
            ]
 
elementSignatureProperties :: XMLParser SignaturePropertiesType
elementSignatureProperties = parseSchemaType "SignatureProperties"
elementToXMLSignatureProperties :: SignaturePropertiesType -> [Content ()]
elementToXMLSignatureProperties = schemaTypeToXML "SignatureProperties"
 
data SignaturePropertiesType = SignaturePropertiesType
        { signatPropsType_id :: Maybe ID
        , signatPropsType_signatureProperty :: [SignaturePropertyType]
        }
        deriving (Eq,Show)
instance SchemaType SignaturePropertiesType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "Id" e pos
        commit $ interior e $ return (SignaturePropertiesType a0)
            `apply` many1 (elementSignatureProperty)
    schemaTypeToXML s x@SignaturePropertiesType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "Id") $ signatPropsType_id x
                       ]
            [ concatMap (elementToXMLSignatureProperty) $ signatPropsType_signatureProperty x
            ]
 
elementSignatureProperty :: XMLParser SignaturePropertyType
elementSignatureProperty = parseSchemaType "SignatureProperty"
elementToXMLSignatureProperty :: SignaturePropertyType -> [Content ()]
elementToXMLSignatureProperty = schemaTypeToXML "SignatureProperty"
 
data SignaturePropertyType = SignaturePropertyType
        { signatPropType_target :: AnyURI
        , signatPropType_id :: Maybe ID
        , signatPropType_choice0 :: [OneOf2 String (AnyElement)]
          -- ^ Choice between:
          --   
          --   (1) mixed text
          --   
          --   (2) unknown
        }
        deriving (Eq,Show)
instance SchemaType SignaturePropertyType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "Target" e pos
        a1 <- optional $ getAttribute "Id" e pos
        commit $ interior e $ return (SignaturePropertyType a0 a1)
            `apply` many1 (oneOf' [ ("String", fmap OneOf2 (parseText))
                                  , ("AnyElement", fmap TwoOf2 (parseAnyElement))
                                  ])
    schemaTypeToXML s x@SignaturePropertyType{} =
        toXMLElement s [ toXMLAttribute "Target" $ signatPropType_target x
                       , maybe [] (toXMLAttribute "Id") $ signatPropType_id x
                       ]
            [ concatMap (foldOneOf2  (toXMLText)
                                     (toXMLAnyElement)
                                    ) $ signatPropType_choice0 x
            ]
 
newtype HMACOutputLengthType = HMACOutputLengthType Integer deriving (Eq,Show)
instance Restricts HMACOutputLengthType Integer where
    restricts (HMACOutputLengthType x) = x
instance SchemaType HMACOutputLengthType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (HMACOutputLengthType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType HMACOutputLengthType where
    acceptingParser = fmap HMACOutputLengthType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (HMACOutputLengthType x) = simpleTypeText x
 
elementDSAKeyValue :: XMLParser DSAKeyValueType
elementDSAKeyValue = parseSchemaType "DSAKeyValue"
elementToXMLDSAKeyValue :: DSAKeyValueType -> [Content ()]
elementToXMLDSAKeyValue = schemaTypeToXML "DSAKeyValue"
 
data DSAKeyValueType = DSAKeyValueType
        { dSAKeyValueType_p :: CryptoBinary
        , dSAKeyValueType_q :: CryptoBinary
        , dSAKeyValueType_g :: Maybe CryptoBinary
        , dSAKeyValueType_y :: CryptoBinary
        , dSAKeyValueType_j :: Maybe CryptoBinary
        , dSAKeyValueType_seed :: CryptoBinary
        , dSAKeyValueType_pgenCounter :: CryptoBinary
        }
        deriving (Eq,Show)
instance SchemaType DSAKeyValueType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DSAKeyValueType
            `apply` parseSchemaType "P"
            `apply` parseSchemaType "Q"
            `apply` optional (parseSchemaType "G")
            `apply` parseSchemaType "Y"
            `apply` optional (parseSchemaType "J")
            `apply` parseSchemaType "Seed"
            `apply` parseSchemaType "PgenCounter"
    schemaTypeToXML s x@DSAKeyValueType{} =
        toXMLElement s []
            [ schemaTypeToXML "P" $ dSAKeyValueType_p x
            , schemaTypeToXML "Q" $ dSAKeyValueType_q x
            , maybe [] (schemaTypeToXML "G") $ dSAKeyValueType_g x
            , schemaTypeToXML "Y" $ dSAKeyValueType_y x
            , maybe [] (schemaTypeToXML "J") $ dSAKeyValueType_j x
            , schemaTypeToXML "Seed" $ dSAKeyValueType_seed x
            , schemaTypeToXML "PgenCounter" $ dSAKeyValueType_pgenCounter x
            ]
 
elementRSAKeyValue :: XMLParser RSAKeyValueType
elementRSAKeyValue = parseSchemaType "RSAKeyValue"
elementToXMLRSAKeyValue :: RSAKeyValueType -> [Content ()]
elementToXMLRSAKeyValue = schemaTypeToXML "RSAKeyValue"
 
data RSAKeyValueType = RSAKeyValueType
        { rSAKeyValueType_modulus :: CryptoBinary
        , rSAKeyValueType_exponent :: CryptoBinary
        }
        deriving (Eq,Show)
instance SchemaType RSAKeyValueType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return RSAKeyValueType
            `apply` parseSchemaType "Modulus"
            `apply` parseSchemaType "Exponent"
    schemaTypeToXML s x@RSAKeyValueType{} =
        toXMLElement s []
            [ schemaTypeToXML "Modulus" $ rSAKeyValueType_modulus x
            , schemaTypeToXML "Exponent" $ rSAKeyValueType_exponent x
            ]
