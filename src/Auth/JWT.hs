{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth.JWT
  ( lookupToken
  , jsonToToken
  , tokenToJson
  )
  where
import           Data.Text as T 
import           Data.List as L 
import           Import.NoFoundation

--import           ClassyPrelude.Yesod
import           Data.Char           (isSpace)
import           Data.Map             as Map (fromList, (!?))
--import           Web.JWT               {-     as JWT (encodeSigned, hmacSecret,unregisteredClaims,ClaimsMap(..))

 
import Web.JWT         
import Data.Time.Clock.POSIX ( POSIXTime )

-- | Try to lookup token from the Authorization header
lookupToken :: MonadHandler m => m (Maybe Text)
lookupToken = do
  mAuth <- lookupHeader "Authorization"
  return $ extractToken . decodeUtf8 =<< mAuth
--  addUTCTime :: NominalDiffTime -> UTCTime -> UTCTimeSource 

  {- posixSecondsToUTCTime :: POSIXTime -> UTCTimeSource

  utcTimeToPOSIXSeconds :: UTCTime -> POSIXTimeSource

  getPOSIXTime
 -}

-- numericDate :: NominalDiffTime -> Maybe NumericDate
-- | Create a token out of a given JSON 'Value'
jsonToToken :: Text -> Value -> POSIXTime ->Text
jsonToToken jwtSecret userId exp' = 
    let
      cs = mempty 
        { -- mempty returns a default JWTClaimsSet
           iss = stringOrURI . T.pack $ "Ideal"
         , unregisteredClaims = ClaimsMap $ Map.fromList [(jwtKey, userId)]
         , Web.JWT.exp = numericDate exp'
        }
      key = hmacSecret jwtSecret
      in encodeSigned key mempty cs

{-   key = hmacSecret . T.pack $ "hassukissa"  
  Web.JWT.encodeSigned (Web.JWT.hmacSecret jwtSecret) mempty {unregisteredClaims = ClaimsMap $ Map.fromList [(jwtKey, userId)]}
 -}
-- | Extract a JSON 'Value' out of a token
tokenToJson :: Text -> Text -> Maybe Value
tokenToJson jwtSecret token = do
    jwt <- Web.JWT.decodeAndVerifySignature (Web.JWT.hmacSecret jwtSecret) token
    unClaimsMap (Web.JWT.unregisteredClaims (Web.JWT.claims jwt)) !? jwtKey

jwtKey :: Text
jwtKey = "jwt"

extractToken :: Text -> Maybe Text
extractToken auth
  | T.toLower (T.pack x) == "bearer" = Just $ T.pack (L.dropWhile isSpace y)
  | otherwise            = Nothing
  where (x, y) = L.break isSpace (T.unpack auth)

