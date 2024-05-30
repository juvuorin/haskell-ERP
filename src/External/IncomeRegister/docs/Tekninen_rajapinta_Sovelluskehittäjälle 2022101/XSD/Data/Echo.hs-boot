{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.Echo'xsd
  ( module Data.Echo'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
elementEcho :: XMLParser EchoMessage
elementToXMLEcho :: EchoMessage -> [Content ()]
 
newtype Data = Data Xsd.XsdString
instance Eq Data
instance Show Data
instance Restricts Data Xsd.XsdString
instance SchemaType Data
instance SimpleType Data
 
data EchoMessage
instance Eq EchoMessage
instance Show EchoMessage
instance SchemaType EchoMessage
