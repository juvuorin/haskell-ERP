{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Data.Echo'xsd
  ( module Data.Echo'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Data.Xmldsig'core'schema'xsd as Ds
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementEcho :: XMLParser EchoMessage
elementEcho = parseSchemaType "Echo"
elementToXMLEcho :: EchoMessage -> [Content ()]
elementToXMLEcho = schemaTypeToXML "Echo"
 
newtype Data = Data Xsd.XsdString deriving (Eq,Show)
instance Restricts Data Xsd.XsdString where
    restricts (Data x) = x
instance SchemaType Data where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (Data x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Data where
    acceptingParser = fmap Data acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (Data x) = simpleTypeText x
 
data EchoMessage = EchoMessage
        { echoMessage_data :: Data
        , echoMessage_signature :: SignatureType
        }
        deriving (Eq,Show)
instance SchemaType EchoMessage where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return EchoMessage
            `apply` parseSchemaType "Data"
            `apply` elementSignature
    schemaTypeToXML s x@EchoMessage{} =
        toXMLElement s []
            [ schemaTypeToXML "Data" $ echoMessage_data x
            , elementToXMLSignature $ echoMessage_signature x
            ]
