{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.IRCommonTypes
  ( module Data.IRCommonTypes
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
newtype TrueOrFalse = TrueOrFalse Xs.Boolean
instance Eq TrueOrFalse
instance Show TrueOrFalse
instance Restricts TrueOrFalse Xs.Boolean
instance SchemaType TrueOrFalse
instance SimpleType TrueOrFalse
 
newtype Decimal2 = Decimal2 Xs.Decimal
instance Eq Decimal2
instance Show Decimal2
instance Restricts Decimal2 Xs.Decimal
instance SchemaType Decimal2
instance SimpleType Decimal2
 
newtype Guid = Guid Xs.NormalizedString
instance Eq Guid
instance Show Guid
instance Restricts Guid Xs.NormalizedString
instance SchemaType Guid
instance SimpleType Guid
 
data MonthsType
instance Eq MonthsType
instance Show MonthsType
instance Enum MonthsType
instance SchemaType MonthsType
instance SimpleType MonthsType
 
newtype PensionPolicyNo = PensionPolicyNo Xs.NormalizedString
instance Eq PensionPolicyNo
instance Show PensionPolicyNo
instance Restricts PensionPolicyNo Xs.NormalizedString
instance SchemaType PensionPolicyNo
instance SimpleType PensionPolicyNo
 
newtype String10 = String10 Xs.NormalizedString
instance Eq String10
instance Show String10
instance Restricts String10 Xs.NormalizedString
instance SchemaType String10
instance SimpleType String10
 
newtype String100 = String100 Xs.NormalizedString
instance Eq String100
instance Show String100
instance Restricts String100 Xs.NormalizedString
instance SchemaType String100
instance SimpleType String100
 
newtype String15 = String15 Xs.NormalizedString
instance Eq String15
instance Show String15
instance Restricts String15 Xs.NormalizedString
instance SchemaType String15
instance SimpleType String15
 
newtype String2 = String2 Xs.XsdString
instance Eq String2
instance Show String2
instance Restricts String2 Xs.XsdString
instance SchemaType String2
instance SimpleType String2
 
newtype String20 = String20 Xs.NormalizedString
instance Eq String20
instance Show String20
instance Restricts String20 Xs.NormalizedString
instance SchemaType String20
instance SimpleType String20
 
newtype String200 = String200 Xs.NormalizedString
instance Eq String200
instance Show String200
instance Restricts String200 Xs.NormalizedString
instance SchemaType String200
instance SimpleType String200
 
newtype String30 = String30 Xs.NormalizedString
instance Eq String30
instance Show String30
instance Restricts String30 Xs.NormalizedString
instance SchemaType String30
instance SimpleType String30
 
newtype String40 = String40 Xs.NormalizedString
instance Eq String40
instance Show String40
instance Restricts String40 Xs.NormalizedString
instance SchemaType String40
instance SimpleType String40
 
newtype String4000 = String4000 Xs.XsdString
instance Eq String4000
instance Show String4000
instance Restricts String4000 Xs.XsdString
instance SchemaType String4000
instance SimpleType String4000
 
newtype String5 = String5 Xs.NormalizedString
instance Eq String5
instance Show String5
instance Restricts String5 Xs.NormalizedString
instance SchemaType String5
instance SimpleType String5
 
newtype String50 = String50 Xs.NormalizedString
instance Eq String50
instance Show String50
instance Restricts String50 Xs.NormalizedString
instance SchemaType String50
instance SimpleType String50
 
newtype String500 = String500 Xs.NormalizedString
instance Eq String500
instance Show String500
instance Restricts String500 Xs.NormalizedString
instance SchemaType String500
instance SimpleType String500
 
newtype String70 = String70 Xs.NormalizedString
instance Eq String70
instance Show String70
instance Restricts String70 Xs.NormalizedString
instance SchemaType String70
instance SimpleType String70
 
newtype String850 = String850 Xs.NormalizedString
instance Eq String850
instance Show String850
instance Restricts String850 Xs.NormalizedString
instance SchemaType String850
instance SimpleType String850
 
data True
instance Eq True
instance Show True
instance Enum True
instance SchemaType True
instance SimpleType True
