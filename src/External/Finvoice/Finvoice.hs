{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts, DuplicateRecordFields, CPP #-}
{-# LANGUAGE BangPatterns #-}

module External.Finvoice.Finvoice where
import Import
    ( fst,
      ($),
      Show(show),
      Either(..),
      String,
      ManagerSettings,
      pack, unpack )
import Text.XML.HaXml.Verbatim (verbatim)
import Text.XML.HaXml.Combinators as C
import Text.Pretty.Simple (pPrint)
import External.IncomeRegister.Types
import Text.XML.HaXml (xmlParse)
import Text.XML.HaXml.Types (Document (Document))
import Text.XML.HaXml
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Schema.Schema ( runParser,  parseSchemaType, XMLParser)
import Text.XML.HaXml.Parse (xmlParse')
import Data.Finvoice30
import Types
import Import.NoFoundation (Text)
import System.Directory

parseFile :: IO ()
parseFile = do 
    print "moikka"
    current<-getCurrentDirectory

    let files = ["/home/azureuser/hookkeeperHaskell/src/External/Finvoice/xsd/laskuesimerkit/Verkkoyhtiö_yrityslaskuesimerkki_20200921.xml",
            "/home/azureuser/hookkeeperHaskell/src/External/Finvoice/xsd/laskuesimerkit/Verkkoyhtiö_yrityslaskuesimerkki_siirtyvä_veloitus_20200921.xml",
            "/home/azureuser/hookkeeperHaskell/src/External/Finvoice/xsd/laskuesimerkit/Verkkoyhtiön_kulutuslaskuesimerkki_20200921.xml",
            "/home/azureuser/hookkeeperHaskell/src/External/Finvoice/xsd/laskuesimerkit/Verkkoyhtion_kulutuslaskuesimerkki_Ennakko_PaidAmount_20200921.xml",
            "/home/azureuser/hookkeeperHaskell/src/External/Finvoice/xsd/laskuesimerkit/Verkkoyhtion_kulutuslaskuesimerkki_Vakiokorvaus_20200921.xml",
            "/home/azureuser/hookkeeperHaskell/src/External/Finvoice/xsd/laskuesimerkit/Verkkoyhtiön_läpilaskutuksen_erillislaskuesimerkki_20200921.xml",
            "/home/azureuser/hookkeeperHaskell/src/External/Finvoice/xsd/laskuesimerkit/Verkkoyhtiön_loppulaskuesimerkki_yritys_20200921.xml"]
  

    putStrLn current
    results <- mapM (\file -> do 
          x' <- readFile file
          let doc = xmlParse' "" x'           
          case doc of 
              Right x -> do
                  let (Text.XML.HaXml.Types.Document _ _ root _)  =  x  
                  let !cnt' = [CElem root noPos]
                  print "OK"
                  let (result,pos) =  runParser elementFinvoice [CElem root noPos]

                  case result of 
                      Right invoice -> do
                     
                          pPrint $ verbatim $ elementToXMLFinvoice invoice
            
                          return $ Right "OK"
                      Left s -> return $ Left s

              Left _ -> return $ Left "failed"
              ) files
    print results
    return ()          
