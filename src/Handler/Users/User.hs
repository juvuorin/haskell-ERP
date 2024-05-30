{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Users.User
  ( postUsersLoginR,
    postUsersRegisterR,
  )
where

import Data.Aeson (object)
import Data.Maybe (maybe)
import Data.Text
import Handler.Users.Internal
import Import hiding (FormResult)
import qualified Yesod.Auth.Util.PasswordStore as PS

type LoginFields = '["user", "email", "password"]

data Login = Login
  { email :: Text,
    password :: Text
  }
  deriving (Show, Generic)

instance FromJSON Login

instance ToJSON Login

postUsersLoginR :: Handler Value
postUsersLoginR = do
  loginUser <- requireCheckJsonBody :: Handler Login
  mUser <- runDB $ getBy $ UniqueUser (email loginUser)
  let loginPassword = password loginUser

  case mUser of
    Just (Entity userId user@User {..}) -> do
      case userPassword of
        Just p -> if verifyPassword loginPassword p then encodeUser userId user else sendResponseStatus status404 (Data.Text.pack "Not found")
        Nothing -> sendResponseStatus status404 (Data.Text.pack "Not found")
    _ -> notAuthenticated


type RegisterFields = '["user", "username", "email", "password"]

data Register = Register
  { registerUsername :: Text,
    registerEmail :: Text,
    registerPassword :: Text
  }
  deriving (Show)

postUsersRegisterR :: Handler Value
postUsersRegisterR = do
  registeringUser <- requireCheckJsonBody :: Handler Login
  pwdHash <- mkPassword (password registeringUser)
  let user = User (email registeringUser) (Just pwdHash)
  userId <- runDB $ insert user
  encodeUser userId user

postUsersDeleteR :: UserId -> Handler ()
postUsersDeleteR userId = do
  runDB $ delete userId
  sendResponseStatus status200 ("DELETED" :: Text)

getUserR :: Handler Value
getUserR = do
  mUserId <- maybeAuthId
  case mUserId of
    Nothing -> notAuthenticated
    Just userId -> do
      mUser <- runDB $ get userId
      case mUser of
        Nothing -> notAuthenticated
        Just user -> encodeUser userId user

type UpdateFields = '["user", "username", "email", "password", "image", "bio"]

data Update' = Update'
  { updateEmail :: Maybe Text,
    updatePassword :: Maybe Text
  }
  deriving (Show)

encodeUser :: UserId -> User -> Handler Value
encodeUser userId User {..} = do
  token <- userIdToToken userId
  setSession "auth_token" token
  return $ object ["usertoken" .= token]
