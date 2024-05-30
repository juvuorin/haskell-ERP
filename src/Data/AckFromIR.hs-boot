{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.AckFromIR
  ( module Data.AckFromIR
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.StatusMessageTypes as Smt 
elementAckFromIR :: XMLParser Smt.AckFromIR
elementToXMLAckFromIR :: Smt.AckFromIR -> [Content ()]
