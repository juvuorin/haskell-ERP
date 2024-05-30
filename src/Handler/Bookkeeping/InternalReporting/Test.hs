{-# LANGUAGE OverloadedStrings #-}


module Handler.Bookkeeping.InternalReporting.Test where

import Data.Aeson
    ( decode,
      encode,
      (.:),
      withObject,
      object,
      FromJSON(parseJSON),
      KeyValue((.=)),
      ToJSON(toJSON) )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as BS
import ClassyPrelude ((<|>))

-- Define a simple Haskell tree datatype
data Tree a = Leaf a | Node [Tree a]
  deriving (Show)

-- Define instances for converting our Tree datatype to and from JSON
instance ToJSON a => ToJSON (Tree a) where
  toJSON (Leaf x) = object ["leaf" .= x]
  toJSON (Node children) = object ["node" .= toJSON children]

instance FromJSON a => FromJSON (Tree a) where
  parseJSON = withObject "Tree" $ \o -> 
    Leaf <$> o .: "leaf" <|> Node <$> o .: "node"

-- Example Haskell tree
haskellTree :: Tree Int
haskellTree = Node [Leaf 1, Node [Leaf 2, Leaf 3], Leaf 4]

h = "{\"node\":[{\"leaf\":1},{\"node\":[{\"leaf\":2},{\"leaf\":3}]},{\"leaf\":4}]}"


-- Convert Haskell tree to JSON
jsonTree :: Tree Int -> BS.ByteString
jsonTree = encode

main :: IO ()
main = do
  let json = jsonTree haskellTree
  TIO.putStrLn $ "JSON Tree: " <> (T.pack $ BS.unpack json)
  let y = decode h :: Maybe (Tree Int)
  case y of 
    Nothing -> return ()
    Just tr -> putStrLn (show tr)
  