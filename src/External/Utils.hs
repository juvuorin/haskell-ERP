{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns, FlexibleContexts, DuplicateRecordFields,CPP #-}
--{-# OPTIONS_GHC -w #-}
{- module External.Utils (

    mkMngr,mkMngrDefaultTls,findStringBetween, storePem,readAndSign,External.Utils.sign

) where

 -}
module External.Utils
where

import OpenSSL.PEM
import OpenSSL.EVP.PKey
import OpenSSL.RSA
    ( RSAKey(rsaE, rsaN, rsaSize), RSAKeyPair, rsaD )
import Data.Maybe

--import Crypto.PubKey.RSA.Types
import Data.Text.Encoding as TLS ( encodeUtf8 )
import Import(readFile)
--import           Crypto.PubKey.RSA.Types
import           Data.Text.Encoding ( decodeUtf8,encodeUtf8 )
import           Text.XML.C14N
import           System.Random
-- #if MIN_VERSION_conduit(1,1,0)
-- #endif
import           Data.Text.Internal.Search ( indices )
import qualified Data.Text as T
import           Data.List.Split ()
import qualified Data.ByteString as B
import           Network.Connection ( TLSSettings(TLSSettings) )
import           Network.TLS
    ( defaultParamsClient,
      credentialLoadX509,
      Supported(supportedCiphers),
      ClientParams(clientSupported, clientHooks),
      ClientHooks(onServerCertificate, onCertificateRequest), credentialLoadX509FromMemory )
import           Network.TLS.Extra.Cipher (ciphersuite_default)
import           Data.Default ( Default(def) )
import           Network.HTTP.Client (RequestBody (RequestBodyLBS), ManagerSettings,Request,defaultManagerSettings,newManager,  requestHeaders, requestBody, method)
import           Network.HTTP.Client.TLS (mkManagerSettings,tlsManagerSettings)
import           Data.ByteString.Lazy ()
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import           Text.XML ()
import           Text.XML.Cursor as Cur ()
import qualified Data.Text as Text
import           Data.ByteString.Lazy.UTF8 as U ()
{- import OpenSSL.PEM
import           OpenSSL.EVP.PKey
import           OpenSSL.RSA
 -}

import           Codec.Crypto.RSA (sign)
--import           Crypto.PubKey.OpenSsh (decodePrivate, OpenSshPrivateKey(..),OpenSshPublicKey(..))
import           Crypto.Types.PubKey.RSA (PrivateKey(..), PublicKey(..))
import Crypto.PubKey.RSA as RSA

import           Types
import Import
import Prelude hiding ((.), map, (++), writeFile, print)
import qualified Data.Text.Encoding as TT
getRandom :: IO Int
getRandom = do
    num <- randomRIO (0,59)
    return num


findString :: Text.Text -> Text.Text -> [Int]
findString = indices

findStringMaybe :: Text.Text -> Text.Text -> Maybe Int
findStringMaybe x str = case indices x str of
                         (idx:_) -> Just idx
                         _ -> Nothing
findStringBetween::Text.Text->Text.Text->Text.Text->Maybe Text.Text
findStringBetween startString endString str=do
    let s = findStringMaybe startString str
    case s of 
        Just x ->  do
            let firstPart = Text.drop x str
            let e = findStringMaybe endString firstPart
            case e of 
                Just x->do
                    let endIndex = x+(Text.length endString) 
                    Just (Text.take endIndex firstPart)
                Nothing -> Nothing                                                
        Nothing -> Nothing


findStringBetween'::Text.Text->Text.Text->Text.Text->Text.Text
findStringBetween' startString endString str=do
    let s = findStringMaybe startString str
    case s of 
        Just x ->  do
            let firstPart = Text.drop x str
            let e = findStringMaybe endString firstPart
            case e of 
                Just x->do
                    let endIndex = x+(Text.length endString) 
                    (Text.take endIndex firstPart)
                Nothing -> ""                                                
        Nothing -> ""
        
        
        
canonicalize::Text.Text->IO Text.Text
canonicalize xmlToBeSigned = do
    canonicalizedXml <- c14n [] c14n_exclusive_1_0 [] False Nothing (Data.Text.Encoding.encodeUtf8 xmlToBeSigned)
    return $ Data.Text.Encoding.decodeUtf8 canonicalizedXml
  
canonicalizeAndHash :: Text.Text -> IO Text.Text
canonicalizeAndHash xmlToBeSigned  = do
    canonicalizedXml <- c14n [] c14n_exclusive_1_0 [] False Nothing (Data.Text.Encoding.encodeUtf8 xmlToBeSigned)
    return $ Data.Text.Encoding.decodeUtf8 canonicalizedXml 
 
storePem:: Text.Text->Text.Text->IO ()
storePem cert savePath= do

    let rows'' = ["-----BEGIN CERTIFICATE-----"] ++ T.chunksOf 64 cert
    let rows''' = mconcat $ mappend (map (<>"\n") rows'')  ["-----END CERTIFICATE-----"]
 --   writeFile ((Text.unpack savePath)++"Certificate.pem") (T.unpack rows''')
    writeFile (T.unpack savePath++"Certificate.pem") (fromString (T.unpack rows'''))
    print "File written ok"
    return ()

createPemWithBeginAndEnd:: Text.Text->Text.Text
createPemWithBeginAndEnd cert = do

    let rows'' = ["-----BEGIN CERTIFICATE-----"] ++ T.chunksOf 64 cert
    let rows''' = mconcat $ mappend (map (<>"\n") rows'')  ["-----END CERTIFICATE-----"]
 --   writeFile ((Text.unpack savePath)++"Certificate.pem") (T.unpack rows''')
--    writeFile (T.unpack savePath++"Certificate.pem") (T.unpack rows''')
--    print "File written ok"
    rows'''


mkMngr :: String -> FilePath -> FilePath -> IO ManagerSettings
mkMngr hostName crtFile keyFile = do

    creds <- either error Just `fmap` credentialLoadX509 crtFile keyFile
    let hooks = def 
            { onCertificateRequest = \_ -> print("Certificate requested") >> return creds
            , onServerCertificate = \_ _ _ _ -> print("Server certificate requested") >>return []
            }

        clientParams = (defaultParamsClient "apitesti.ilmoitin.fi" B.empty)
            { clientHooks = hooks
            , clientSupported = def { supportedCiphers = ciphersuite_default }
            }
        tlsSettings = TLSSettings clientParams
    return $ mkManagerSettings tlsSettings Nothing

getKeyAndCert :: Key Company -> Handler (Maybe (ByteString, ByteString))
getKeyAndCert companyId' = do
    security <- runDB $ selectList [SecurityInfoCompanyId ==. companyId'][] :: Handler [Entity SecurityInfo] 
    securityEntity <- case security of 
      (x:xs) -> return x 
      _ -> sendResponseStatus status404 ("Security info was not found"::Text.Text)

    let key' = securityInfoCurrentprivatekey (entityVal securityEntity)     

    case key' of
      Just key -> do
        let certificate' = securityInfoCertificate (entityVal securityEntity)     
        case certificate' of
          Just certificate ->return $ Just(key,certificate)
          Nothing -> return Nothing

      Nothing -> return Nothing

mkMngr' :: String -> CompanyId -> Handler ManagerSettings
mkMngr' hostName companyId = do

    maybeKeyCertPair <- getKeyAndCert companyId 
    (key, certificate) <- case maybeKeyCertPair of 
      Just x -> return x 
      Nothing -> sendResponseStatus status404 ("Cannot get certificate and key while trying to create TLS socket"::Text.Text)

    let creds' = credentialLoadX509FromMemory certificate key
    creds<-case creds' of
      Right creds -> return (Just creds)
      Left _ -> sendResponseStatus status404 ("Cannot find credentials!"::Text)

    let hooks = def 
            { onCertificateRequest = \_ -> print("Certificate requested") >> return creds
            , onServerCertificate = \_ _ _ _ -> print("Server certificate requested") >> return []
            }

        clientParams = (defaultParamsClient "" B.empty)
            { clientHooks = hooks
            , clientSupported = def { supportedCiphers = ciphersuite_default }
            }
        tlsSettings = TLSSettings clientParams
    return $ mkManagerSettings tlsSettings Nothing


mkMngrDefaultTls :: IO ManagerSettings
mkMngrDefaultTls  = do
    return $  tlsManagerSettings
    
{- throwLeft :: Either String OpenSshPrivateKey -> Crypto.Types.PubKey.RSA.PrivateKey
throwLeft (Right (OpenSshPrivateKeyRsa k)) = k
throwLeft (Right _) = error "Wrong key type"
throwLeft (Left s)  = error $ "Error reading keys: " ++ s
 -}
{- throwLeft' :: Either String OpenSshPublicKey -> Crypto.Types.PubKey.RSA.PublicKey
throwLeft' (Right (OpenSshPublicKeyRsa k desc)) = k
throwLeft' (Right _) = error "Wrong key type"
throwLeft' (Left s)  = error $ "Error reading keys: " ++ s
 -}

privateKeyFromFile :: FilePath -> IO (Either String RSA.PrivateKey) 
privateKeyFromFile  file  = do
    keyString <- Prelude.readFile file
    key_ <- readPrivateKey keyString PwNone
    let maybeRsaKey = (toKeyPair key_) :: Maybe RSAKeyPair

    case maybeRsaKey of 
      Just key -> do
            let size = rsaSize key
            let n = rsaN key
            let e = rsaE key
            
            let pubK = RSA.PublicKey {public_size=size, public_n=n,public_e=e}
          
            let pkey = RSA.PrivateKey { private_pub  = pubK
                                          , private_d    = rsaD key 
                                          , private_p    = 0
                                          , private_q    = 0
                                          , private_qinv = 0
                                          , private_dP   = 0
                                          , private_dQ   = 0 }
            return $ Right pkey
      Nothing -> return $ Left $ "Wrong key type"      




-- Muutettu 16.8.2022 voi olla, että ei toimi!!! 
readAndSign :: FilePath -> LBS.ByteString -> IO LBS.ByteString
readAndSign file msg = do
    keyString <- Prelude.readFile file
    key_ <- readPrivateKey keyString PwNone
    let maybeRsaKey = (toKeyPair key_) :: Maybe RSAKeyPair

    case maybeRsaKey of 
      Just key -> do
            let size = rsaSize key
            let n = rsaN key
            let e = rsaE key
            
            let pubK = Crypto.Types.PubKey.RSA.PublicKey {public_size=size, public_n=n,public_e=e}
          
            let pkey = Crypto.Types.PubKey.RSA.PrivateKey { private_pub  = pubK
                                          , private_d    = rsaD key 
                                          , private_p    = 0
                                          , private_q    = 0
                                          , private_qinv = 0
                                          , private_dP   = 0
                                          , private_dQ   = 0 }
            return $ Codec.Crypto.RSA.sign pkey msg
      Nothing -> error "Wrong key type"      

sign' :: BS.ByteString -> LBS.ByteString -> IO LBS.ByteString
sign' key msg = do
    let decoded = TT.decodeUtf8 key
    let keyS = (T.unpack decoded)
    key_ <- readPrivateKey keyS PwNone
    let maybeRsaKey = (toKeyPair key_) :: Maybe RSAKeyPair

    case maybeRsaKey of 
      Just key -> do
            let size = rsaSize key
            let n = rsaN key
            let e = rsaE key
            
            let pubK = Crypto.Types.PubKey.RSA.PublicKey {public_size=size, public_n=n,public_e=e}
          
            let pkey = Crypto.Types.PubKey.RSA.PrivateKey { private_pub  = pubK
                                          , private_d    = rsaD key 
                                          , private_p    = 0
                                          , private_q    = 0
                                          , private_qinv = 0
                                          , private_dP   = 0
                                          , private_dQ   = 0 }
            return $ Codec.Crypto.RSA.sign pkey msg
      Nothing -> error "Wrong key type"      



      --key <- BS.readFile file
{-       case decodePrivate  key of 
          Right (OpenSshPrivateKeyRsa key) ->  Codec.Crypto.RSA.sign key msg
          Right _ -> error "Wrong key type"
          Left _ -> error $ "Error reading keys: " 

 -}
{- read :: FilePath -> IO ByteString
read file = do
    key <- BS.readFile file
    case decodePrivate key of 
        Right (OpenSshPrivateKeyRsa key) -> return $ key 
        Right _ -> error "Wrong key type"
        Left _ -> error $ "Error reading keys: " ++ file

 -}
    --(flip Codec.Crypto.RSA.sign msg . throwLeft . decodePrivate) `fmap` BS.readFile file

{- sign :: Types.PrivateKey -> LBS.ByteString -> LBS.ByteString

sign pvtKey msg = do
    let pvtKey'=TLS.encodeUtf8 (unPrivateKey pvtKey) 
--    let msg'=LBS.fromStrict (TLS.encodeUtf8 pvtKey) 

    (flip Codec.Crypto.RSA.sign msg . throwLeft . decodePrivate)  pvtKey'

 -}




