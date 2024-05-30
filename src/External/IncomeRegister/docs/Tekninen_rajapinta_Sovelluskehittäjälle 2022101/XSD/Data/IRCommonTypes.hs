{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.IRCommonTypes'xsd
  ( module Data.IRCommonTypes'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
newtype TrueOrFalse = TrueOrFalse Xsd.Boolean deriving (Eq,Show)
instance Restricts TrueOrFalse Xsd.Boolean where
    restricts (TrueOrFalse x) = x
instance SchemaType TrueOrFalse where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (TrueOrFalse x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType TrueOrFalse where
    acceptingParser = fmap TrueOrFalse acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern true)
    --      (Pattern false)
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (TrueOrFalse x) = simpleTypeText x
 
newtype Decimal2 = Decimal2 Xsd.Decimal deriving (Eq,Show)
instance Restricts Decimal2 Xsd.Decimal where
    restricts (Decimal2 x) = x
instance SchemaType Decimal2 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (Decimal2 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Decimal2 where
    acceptingParser = fmap Decimal2 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [+-]?\d{1,13}(\.\d{1,2})?)
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (Decimal2 x) = simpleTypeText x
 
newtype Guid = Guid Xs.NormalizedString deriving (Eq,Show)
instance Restricts Guid Xs.NormalizedString where
    restricts (Guid x) = x
instance SchemaType Guid where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (Guid x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Guid where
    acceptingParser = fmap Guid acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [0-9a-fA-F]{32})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (Guid x) = simpleTypeText x
 
data MonthsType
    = MonthsType_V1
    | MonthsType_V2
    | MonthsType_V3
    | MonthsType_V4
    | MonthsType_V5
    | MonthsType_V6
    | MonthsType_V7
    | MonthsType_V8
    | MonthsType_V9
    | MonthsType_V10
    | MonthsType_V11
    | MonthsType_V12
    deriving (Eq,Show,Enum)
instance SchemaType MonthsType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType MonthsType where
    acceptingParser =  do literal "1"; return MonthsType_V1
                      `onFail` do literal "2"; return MonthsType_V2
                      `onFail` do literal "3"; return MonthsType_V3
                      `onFail` do literal "4"; return MonthsType_V4
                      `onFail` do literal "5"; return MonthsType_V5
                      `onFail` do literal "6"; return MonthsType_V6
                      `onFail` do literal "7"; return MonthsType_V7
                      `onFail` do literal "8"; return MonthsType_V8
                      `onFail` do literal "9"; return MonthsType_V9
                      `onFail` do literal "10"; return MonthsType_V10
                      `onFail` do literal "11"; return MonthsType_V11
                      `onFail` do literal "12"; return MonthsType_V12
                      
    simpleTypeText MonthsType_V1 = "1"
    simpleTypeText MonthsType_V2 = "2"
    simpleTypeText MonthsType_V3 = "3"
    simpleTypeText MonthsType_V4 = "4"
    simpleTypeText MonthsType_V5 = "5"
    simpleTypeText MonthsType_V6 = "6"
    simpleTypeText MonthsType_V7 = "7"
    simpleTypeText MonthsType_V8 = "8"
    simpleTypeText MonthsType_V9 = "9"
    simpleTypeText MonthsType_V10 = "10"
    simpleTypeText MonthsType_V11 = "11"
    simpleTypeText MonthsType_V12 = "12"
 
newtype PensionPolicyNo = PensionPolicyNo Xs.NormalizedString deriving (Eq,Show)
instance Restricts PensionPolicyNo Xs.NormalizedString where
    restricts (PensionPolicyNo x) = x
instance SchemaType PensionPolicyNo where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (PensionPolicyNo x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PensionPolicyNo where
    acceptingParser = fmap PensionPolicyNo acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (PensionPolicyNo x) = simpleTypeText x
 
newtype String10 = String10 Xs.NormalizedString deriving (Eq,Show)
instance Restricts String10 Xs.NormalizedString where
    restricts (String10 x) = x
instance SchemaType String10 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (String10 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType String10 where
    acceptingParser = fmap String10 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (String10 x) = simpleTypeText x
 
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
 
newtype String15 = String15 Xs.NormalizedString deriving (Eq,Show)
instance Restricts String15 Xs.NormalizedString where
    restricts (String15 x) = x
instance SchemaType String15 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (String15 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType String15 where
    acceptingParser = fmap String15 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (String15 x) = simpleTypeText x
 
newtype String2 = String2 Xsd.XsdString deriving (Eq,Show)
instance Restricts String2 Xsd.XsdString where
    restricts (String2 x) = x
instance SchemaType String2 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (String2 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType String2 where
    acceptingParser = fmap String2 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (String2 x) = simpleTypeText x
 
newtype String20 = String20 Xs.NormalizedString deriving (Eq,Show)
instance Restricts String20 Xs.NormalizedString where
    restricts (String20 x) = x
instance SchemaType String20 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (String20 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType String20 where
    acceptingParser = fmap String20 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (String20 x) = simpleTypeText x
 
newtype String200 = String200 Xs.NormalizedString deriving (Eq,Show)
instance Restricts String200 Xs.NormalizedString where
    restricts (String200 x) = x
instance SchemaType String200 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (String200 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType String200 where
    acceptingParser = fmap String200 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (String200 x) = simpleTypeText x
 
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
 
newtype String40 = String40 Xs.NormalizedString deriving (Eq,Show)
instance Restricts String40 Xs.NormalizedString where
    restricts (String40 x) = x
instance SchemaType String40 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (String40 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType String40 where
    acceptingParser = fmap String40 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (String40 x) = simpleTypeText x
 
newtype String4000 = String4000 Xsd.XsdString deriving (Eq,Show)
instance Restricts String4000 Xsd.XsdString where
    restricts (String4000 x) = x
instance SchemaType String4000 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (String4000 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType String4000 where
    acceptingParser = fmap String4000 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (String4000 x) = simpleTypeText x
 
newtype String5 = String5 Xs.NormalizedString deriving (Eq,Show)
instance Restricts String5 Xs.NormalizedString where
    restricts (String5 x) = x
instance SchemaType String5 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (String5 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType String5 where
    acceptingParser = fmap String5 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (String5 x) = simpleTypeText x
 
newtype String50 = String50 Xs.NormalizedString deriving (Eq,Show)
instance Restricts String50 Xs.NormalizedString where
    restricts (String50 x) = x
instance SchemaType String50 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (String50 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType String50 where
    acceptingParser = fmap String50 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (String50 x) = simpleTypeText x
 
newtype String500 = String500 Xs.NormalizedString deriving (Eq,Show)
instance Restricts String500 Xs.NormalizedString where
    restricts (String500 x) = x
instance SchemaType String500 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (String500 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType String500 where
    acceptingParser = fmap String500 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (String500 x) = simpleTypeText x
 
newtype String70 = String70 Xs.NormalizedString deriving (Eq,Show)
instance Restricts String70 Xs.NormalizedString where
    restricts (String70 x) = x
instance SchemaType String70 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (String70 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType String70 where
    acceptingParser = fmap String70 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (String70 x) = simpleTypeText x
 
newtype String850 = String850 Xs.NormalizedString deriving (Eq,Show)
instance Restricts String850 Xs.NormalizedString where
    restricts (String850 x) = x
instance SchemaType String850 where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (String850 x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType String850 where
    acceptingParser = fmap String850 acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (String850 x) = simpleTypeText x
 
data True
    = True_True
    deriving (Eq,Show,Enum)
instance SchemaType True where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType True where
    acceptingParser =  do literal "true"; return True_True
                      
    simpleTypeText True_True = "true"
