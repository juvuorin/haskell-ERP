{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.CertificateServices
  ( module Data.CertificateServices
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema hiding (Result)
import Text.XML.HaXml.OneOfN ()
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.Xmldsig as Ds
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
data SignNewCertificateRequest = SignNewCertificateRequest
        { signNewCertificateRequest_environment :: EnvironmentTypes
        , signNewCertificateRequest_customerId :: String30
        , signNewCertificateRequest_customerName :: Maybe String100
        , signNewCertificateRequest_transferId :: String32
        , signNewCertificateRequest_transferPassword :: String16
        , signNewCertificateRequest_certificateRequest :: CertificateRequestType
        }
        deriving (Eq,Show)
instance SchemaType SignNewCertificateRequest where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return SignNewCertificateRequest
            `apply` parseSchemaType "Environment"
            `apply` parseSchemaType "CustomerId"
            `apply` optional (parseSchemaType "CustomerName")
            `apply` parseSchemaType "TransferId"
            `apply` parseSchemaType "TransferPassword"
            `apply` parseSchemaType "CertificateRequest"
    schemaTypeToXML s x@SignNewCertificateRequest{} =
        toXMLElement s []
            [ schemaTypeToXML "Environment" $ signNewCertificateRequest_environment x
            , schemaTypeToXML "CustomerId" $ signNewCertificateRequest_customerId x
            , maybe [] (schemaTypeToXML "CustomerName") $ signNewCertificateRequest_customerName x
            , schemaTypeToXML "TransferId" $ signNewCertificateRequest_transferId x
            , schemaTypeToXML "TransferPassword" $ signNewCertificateRequest_transferPassword x
            , schemaTypeToXML "CertificateRequest" $ signNewCertificateRequest_certificateRequest x
            ]
 
data RenewCertificateRequest = RenewCertificateRequest
        { renewCertificateRequest_environment :: EnvironmentTypes
        , renewCertificateRequest_customerId :: String30
        , renewCertificateRequest_customerName :: Maybe String100
        , renewCertificateRequest_certificateRequest :: CertificateRequestType
        , renewCertificateRequest_signature :: Maybe Ds.SignatureType
        }
        deriving (Eq,Show)
instance SchemaType RenewCertificateRequest where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return RenewCertificateRequest
            `apply` parseSchemaType "Environment"
            `apply` parseSchemaType "CustomerId"
            `apply` optional (parseSchemaType "CustomerName")
            `apply` parseSchemaType "CertificateRequest"
            `apply` optional elementSignature
    schemaTypeToXML s x@RenewCertificateRequest{} =
        toXMLElement s []
            [ schemaTypeToXML "Environment" $ renewCertificateRequest_environment x
            , schemaTypeToXML "CustomerId" $ renewCertificateRequest_customerId x
            , maybe [] (schemaTypeToXML "CustomerName") $ renewCertificateRequest_customerName x
            , schemaTypeToXML "CertificateRequest" $ renewCertificateRequest_certificateRequest x
            , maybe [] (elementToXMLSignature) $ renewCertificateRequest_signature x
            ]
 
data SignNewCertificateResponse = SignNewCertificateResponse
        { signNewCertificateResponse_retrievalId :: Maybe String32
        , signNewCertificateResponse_result :: Result
        , signNewCertificateResponse_signature :: Ds.SignatureType
        }
        deriving (Eq,Show)
instance SchemaType SignNewCertificateResponse where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return SignNewCertificateResponse
            `apply` optional (parseSchemaType "RetrievalId")
            `apply` parseSchemaType "Result"
            `apply` elementSignature
    schemaTypeToXML s x@SignNewCertificateResponse{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "RetrievalId") $ signNewCertificateResponse_retrievalId x
            , schemaTypeToXML "Result" $ signNewCertificateResponse_result x
            , elementToXMLSignature $ signNewCertificateResponse_signature x
            ]
 
data RenewCertificateResponse = RenewCertificateResponse
        { renewCertificateResponse_retrievalId :: Maybe String32
        , renewCertificateResponse_result :: Result
        , renewCertificateResponse_signature :: Ds.SignatureType
        }
        deriving (Eq,Show)
instance SchemaType RenewCertificateResponse where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return RenewCertificateResponse
            `apply` optional (parseSchemaType "RetrievalId")
            `apply` parseSchemaType "Result"
            `apply` elementSignature
    schemaTypeToXML s x@RenewCertificateResponse{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "RetrievalId") $ renewCertificateResponse_retrievalId x
            , schemaTypeToXML "Result" $ renewCertificateResponse_result x
            , elementToXMLSignature $ renewCertificateResponse_signature x
            ]
 
data GetCertificateRequest = GetCertificateRequest
        { getCertificateRequest_environment :: EnvironmentTypes
        , getCertificateRequest_customerId :: String30
        , getCertificateRequest_customerName :: Maybe String100
        , getCertificateRequest_retrievalId :: String32
        }
        deriving (Eq,Show)
instance SchemaType GetCertificateRequest where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return GetCertificateRequest
            `apply` parseSchemaType "Environment"
            `apply` parseSchemaType "CustomerId"
            `apply` optional (parseSchemaType "CustomerName")
            `apply` parseSchemaType "RetrievalId"
    schemaTypeToXML s x@GetCertificateRequest{} =
        toXMLElement s []
            [ schemaTypeToXML "Environment" $ getCertificateRequest_environment x
            , schemaTypeToXML "CustomerId" $ getCertificateRequest_customerId x
            , maybe [] (schemaTypeToXML "CustomerName") $ getCertificateRequest_customerName x
            , schemaTypeToXML "RetrievalId" $ getCertificateRequest_retrievalId x
            ]
 
data GetCertificateResponse = GetCertificateResponse
        { getCertificateResponse_certificate :: Maybe CertificateType
        , getCertificateResponse_result :: Result
        , getCertificateResponse_signature :: Ds.SignatureType
        }
        deriving (Eq,Show)
instance SchemaType GetCertificateResponse where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return GetCertificateResponse
            `apply` optional (parseSchemaType "Certificate")
            `apply` parseSchemaType "Result"
            `apply` elementSignature
    schemaTypeToXML s x@GetCertificateResponse{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "Certificate") $ getCertificateResponse_certificate x
            , schemaTypeToXML "Result" $ getCertificateResponse_result x
            , elementToXMLSignature $ getCertificateResponse_signature x
            ]
 
newtype CertificateRequestType = CertificateRequestType Xs.Base64Binary deriving (Eq,Show)
instance Restricts CertificateRequestType Xs.Base64Binary where
    restricts (CertificateRequestType x) = x
instance SchemaType CertificateRequestType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (CertificateRequestType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CertificateRequestType where
    acceptingParser = fmap CertificateRequestType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (CertificateRequestType x) = simpleTypeText x
 
newtype CertificateType = CertificateType Xs.Base64Binary deriving (Eq,Show)
instance Restricts CertificateType Xs.Base64Binary where
    restricts (CertificateType x) = x
instance SchemaType CertificateType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (CertificateType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CertificateType where
    acceptingParser = fmap CertificateType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (CertificateType x) = simpleTypeText x
 
elementSignNewCertificateRequest :: XMLParser SignNewCertificateRequest
elementSignNewCertificateRequest = parseSchemaType "SignNewCertificateRequest"
elementToXMLSignNewCertificateRequest :: SignNewCertificateRequest -> [Content ()]
elementToXMLSignNewCertificateRequest = schemaTypeToXML "SignNewCertificateRequest"
 
elementRenewCertificateRequest :: XMLParser RenewCertificateRequest
elementRenewCertificateRequest = parseSchemaType "RenewCertificateRequest"
elementToXMLRenewCertificateRequest :: RenewCertificateRequest -> [Content ()]
elementToXMLRenewCertificateRequest = schemaTypeToXML "RenewCertificateRequest"
 
elementGetCertificateRequest :: XMLParser GetCertificateRequest
elementGetCertificateRequest = parseSchemaType "GetCertificateRequest"
elementToXMLGetCertificateRequest :: GetCertificateRequest -> [Content ()]
elementToXMLGetCertificateRequest = schemaTypeToXML "GetCertificateRequest"
 
elementSignNewCertificateResponse :: XMLParser SignNewCertificateResponse
elementSignNewCertificateResponse = parseSchemaType "SignNewCertificateResponse"
elementToXMLSignNewCertificateResponse :: SignNewCertificateResponse -> [Content ()]
elementToXMLSignNewCertificateResponse = schemaTypeToXML "SignNewCertificateResponse"
 
elementRenewCertificateResponse :: XMLParser RenewCertificateResponse
elementRenewCertificateResponse = parseSchemaType "RenewCertificateResponse"
elementToXMLRenewCertificateResponse :: RenewCertificateResponse -> [Content ()]
elementToXMLRenewCertificateResponse = schemaTypeToXML "RenewCertificateResponse"
 
elementGetCertificateResponse :: XMLParser GetCertificateResponse
elementGetCertificateResponse = parseSchemaType "GetCertificateResponse"
elementToXMLGetCertificateResponse :: GetCertificateResponse -> [Content ()]
elementToXMLGetCertificateResponse = schemaTypeToXML "GetCertificateResponse"
 
data ErrorType = ErrorType
        { errorType_errorCode :: String16
        , errorType_errorMessage :: String255
        }
        deriving (Eq,Show)
instance SchemaType ErrorType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return ErrorType
            `apply` parseSchemaType "ErrorCode"
            `apply` parseSchemaType "ErrorMessage"
    schemaTypeToXML s x@ErrorType{} =
        toXMLElement s []
            [ schemaTypeToXML "ErrorCode" $ errorType_errorCode x
            , schemaTypeToXML "ErrorMessage" $ errorType_errorMessage x
            ]
 
data Result = Result
        { result_status :: ResultTypes
        , result_errorInfo :: [ErrorType]
        }
        deriving (Eq,Show)
instance SchemaType Result where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Result
            `apply` parseSchemaType "Status"
            `apply` many (parseSchemaType "ErrorInfo")
    schemaTypeToXML s x@Result{} =
        toXMLElement s []
            [ schemaTypeToXML "Status" $ result_status x
            , concatMap (schemaTypeToXML "ErrorInfo") $ result_errorInfo x
            ]
 
newtype String100 = String100 Xs.NormalizedString deriving (Eq,Show)
instance Restricts String100 Xs.NormalizedString where
    restricts (String100 x) = x
instance SchemaType String100 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (String100 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType String100 where
    acceptingParser = fmap String100 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (String100 x) = simpleTypeText x
 
newtype String16 = String16 Xs.NormalizedString deriving (Eq,Show)
instance Restricts String16 Xs.NormalizedString where
    restricts (String16 x) = x
instance SchemaType String16 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (String16 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType String16 where
    acceptingParser = fmap String16 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (String16 x) = simpleTypeText x
 
newtype String255 = String255 Xs.NormalizedString deriving (Eq,Show)
instance Restricts String255 Xs.NormalizedString where
    restricts (String255 x) = x
instance SchemaType String255 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (String255 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType String255 where
    acceptingParser = fmap String255 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (String255 x) = simpleTypeText x
 
newtype String30 = String30 Xs.NormalizedString deriving (Eq,Show)
instance Restricts String30 Xs.NormalizedString where
    restricts (String30 x) = x
instance SchemaType String30 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (String30 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType String30 where
    acceptingParser = fmap String30 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (String30 x) = simpleTypeText x
 
newtype String32 = String32 Xs.NormalizedString deriving (Eq,Show)
instance Restricts String32 Xs.NormalizedString where
    restricts (String32 x) = x
instance SchemaType String32 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (String32 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType String32 where
    acceptingParser = fmap String32 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (String32 x) = simpleTypeText x
 
data EnvironmentTypes
    = EnvironmentTypes_PRODUCTION
    | EnvironmentTypes_TEST
    deriving (Eq,Show,Enum)
instance SchemaType EnvironmentTypes where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EnvironmentTypes where
    acceptingParser =  do literal "PRODUCTION"; return EnvironmentTypes_PRODUCTION
                      `onFail` do literal "TEST"; return EnvironmentTypes_TEST
                      
    simpleTypeText EnvironmentTypes_PRODUCTION = "PRODUCTION"
    simpleTypeText EnvironmentTypes_TEST = "TEST"
 
data ResultTypes
    = ResultTypes_FAIL
    | ResultTypes_OK
    deriving (Eq,Show,Enum)
instance SchemaType ResultTypes where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ResultTypes where
    acceptingParser =  do literal "FAIL"; return ResultTypes_FAIL
                      `onFail` do literal "OK"; return ResultTypes_OK
                      
    simpleTypeText ResultTypes_FAIL = "FAIL"
    simpleTypeText ResultTypes_OK = "OK"
