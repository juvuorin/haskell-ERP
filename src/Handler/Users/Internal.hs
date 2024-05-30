{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Users.Internal
  ( 
    mkPassword
  , verifyPassword
  ) where

import           ClassyPrelude.Yesod
import qualified Yesod.Auth.Util.PasswordStore as PS

-- | Instantiate a 'Password' from 'Text'.
mkPassword :: MonadIO m => Text ->m Text
mkPassword text =
    decodeUtf8 <$> liftIO (PS.makePassword (encodeUtf8 text) 14)

-- | Check a raw password against DB password.
verifyPassword :: Text -> Text -> Bool
verifyPassword rawPassword password =
  PS.verifyPassword (encodeUtf8 rawPassword) $ encodeUtf8 password