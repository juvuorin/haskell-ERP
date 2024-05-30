{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.BankStatement.BankStatement where

import ClassyPrelude.Yesod (neverExpires)
import Data.Text (Text)
import Import

getBankStatementItemsR :: CompanyId -> BankStatementId -> Handler Value
getBankStatementItemsR companyId bankStatementId = do
  bankStatementItems <- runDB $ selectList [BankStatementItemBankStatementId ==. bankStatementId] [] :: Handler [Entity BankStatementItem]
  returnJson bankStatementItems

data TransactionPackageForUI = TransactionPackageForUI
  { transactionPackageForUITransaction :: Entity Transaction,
    transactionPackageForUIEntry :: Entity Entry
  }
  deriving (Generic, Show)

instance ToJSON TransactionPackageForUI

data StatementItemWithPostingSuggestionsForUI = StatementItemWithPostingSuggestionsForUI
  { statementItemWithPostingSuggestionsStatementItem :: BankStatementItem,
    statementItemWithPostingSuggestionsTransactionPackage :: [TransactionPackageForUI]
  }
  deriving (Generic)

instance ToJSON StatementItemWithPostingSuggestionsForUI

getBankStatementItemsWithPostingSuggestionsR :: CompanyId -> BankStatementId -> Handler Value
getBankStatementItemsWithPostingSuggestionsR companyId bankStatementId = do
  bankStatementItems <- runDB $ selectList [BankStatementItemBankStatementId ==. bankStatementId] [] :: Handler [Entity BankStatementItem]

  itemsWithSuggestions <-
    mapM
      ( \item -> do
          suggestions <- runDB $ selectList [BankStatementItemPostingSuggestionBankStatementItemId ==. (entityKey item)] [] :: Handler [Entity BankStatementItemPostingSuggestion]
          packages <-
            mapM
              ( \(Entity _ suggestion) -> do
                  let entryId = bankStatementItemPostingSuggestionEntryId suggestion

                  entry' <- runDB $ getEntity entryId
                  entry <- case entry' of
                    Just x -> return x
                    Nothing -> sendResponseStatus status404 ("Cannot find entry while generating bank statement posting suggestions" :: Text)
                  let transactionId = entryTransactionId (entityVal entry)

                  transaction' <- runDB $ getEntity transactionId
                  transaction <- case transaction' of
                    Just x -> return x
                    Nothing -> sendResponseStatus status404 ("Cannot find transaction while generating bank statement posting suggestions" :: Text)

                  return $ TransactionPackageForUI {transactionPackageForUITransaction = transaction, transactionPackageForUIEntry = entry}
              )
              suggestions

          return
            StatementItemWithPostingSuggestionsForUI
              { statementItemWithPostingSuggestionsStatementItem = (entityVal item),
                statementItemWithPostingSuggestionsTransactionPackage = packages
              }
      )
      bankStatementItems

  returnJson itemsWithSuggestions

deleteBankStatementR :: CompanyId -> BankStatementId -> Handler Value
deleteBankStatementR companyId bankStatementId = do
  putStrLn ("Deleting bankStatement")
  runDB $ delete bankStatementId
  sendResponseStatus status200 ("DELETED" :: Text)
