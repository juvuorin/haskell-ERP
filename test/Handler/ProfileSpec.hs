{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ProfileSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
      describe "Basic navigation and assertions" $ do
        it "Gets a page that has a form, with auto generated fields and token" $ do
          get ("url/to/page/with/form" :: Text) -- Load a page.
          statusIs 200 -- Assert the status was success.

          bodyContains "Hello Person" -- Assert any part of the document contains some text.
          
          -- Perform CSS queries and assertions.
          htmlCount "form .main" 1 -- It matches 1 element.
          htmlAllContain "h1#mainTitle" "Sign Up Now!" -- All matches have some text.


{- spec :: Spec
spec = withApp $ do
 
    describe "Profile page" $ do
        it "asserts no access to my-account for anonymous users" $ do
            get ProfileR
            statusIs 403

        it "asserts access to my-account for authenticated users" $ do
            userEntity <- createUser "foo"
            authenticateAs userEntity

            get ProfileR
            statusIs 200

        it "asserts user's information is shown" $ do
            userEntity <- createUser "bar"
            authenticateAs userEntity

            get ProfileR
            let (Entity _ user) = userEntity
            htmlAnyContain ".username" . unpack $ userIdent user
  -}