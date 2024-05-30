{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.InvalidationsToIR'xsd
  ( module Data.InvalidationsToIR'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
elementInvalidationRequestToIR :: XMLParser Itirt.InvalidationsToIR
elementToXMLInvalidationRequestToIR :: Itirt.InvalidationsToIR -> [Content ()]
 
elementInvalidationsRequestToIR :: XMLParser Itirt.InvalidationsToIR
elementToXMLInvalidationsRequestToIR :: Itirt.InvalidationsToIR -> [Content ()]
