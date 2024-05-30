{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use mapM_" #-}

module Handler.Files.Files where

--import Types
--import Types

import Codec.Picture
import qualified Control.Monad
import Data.Foldable (foldl)
import qualified Data.List
import Data.Time (LocalTime (localDay), ZonedTime (zonedTimeToLocalTime), getZonedTime, gregorianMonthLength)
import Data.Time.Calendar (addGregorianMonthsClip)
import Data.UUID.V4 (nextRandom)
import Database.Persist.Postgresql (toSqlKey)
import Database.Persist.Sql (fromSqlKey)
import Graphics.Thumbnail
--import Handler.Bookkeeping.CrudEndpoints.Transaction (TransactionWithNewAndExistingEntries (..), HeterogenousListEntry (WithoutId), updateBookkeepingTransactionAndInsertOrUpdateEntries)
import Import hiding (id)
import Parsers.BankStatement (extractBankStatementInformation)
import System.Directory (createDirectoryIfMissing, doesFileExist, getFileSize, listDirectory, removeFile)
import Text.Read (read)
import Text.Regex.Posix
import Yesod.Core.Types
--import Common
import Handler.Bookkeeping.CrudEndpoints.Transaction

data StatusType = OK | ERROR | EXISTS deriving (Show, Generic)

instance ToJSON StatusType

data TransferStatus = TransferStatus {status :: StatusType} deriving (Generic, Show)

data Message = Message {message :: TransferStatus} deriving (Generic, Show)

instance ToJSON Message

instance ToJSON TransferStatus

sOk = toJSON (Message {message = TransferStatus OK})

sError = toJSON (Message {message = TransferStatus ERROR})

sExists = toJSON (Message {message = TransferStatus EXISTS})

data FetchInfo = FetchInfo {companyId :: CompanyId, entityId :: Int64, filename :: String, entityType::String} deriving (Show, Generic)

instance FromJSON FetchInfo

postFilesFetchR :: CompanyId -> EntityType -> EntityId -> Handler ()
postFilesFetchR companyId' _ entityId = do
  entries <- requireCheckJsonBody :: Handler FetchInfo
  let filename' = filename entries
  let entityId' = Handler.Files.Files.entityId entries
  let entityType' = entityType entries
  $(logInfo) $ pack $ "companies" </> show (fromSqlKey companyId') </> unpack entityType' </> show entityId' </> filename'
  sendFile typeOctet ("companies" </> show (fromSqlKey companyId') </> unpack entityType' </> show entityId' </> filename')
  return ()

data TransactionPackage = TransactionPackage {transactionPackageTransaction :: Transaction, transactionPackageEntry :: Entry} deriving (Generic, Show)

instance ToJSON TransactionPackage

data StatementItemWithMatches = StatementItemWithMatches
  { statementItemWithMatchesStatementItem :: BankStatementItem,
    statementItemWithMatchesEntries :: [Entity Entry]
  }
  deriving (Generic)

instance ToJSON StatementItemWithMatches



findPotentialMatchesForStatementItems :: [BankStatementItem] -> CompanyId -> Handler [StatementItemWithMatches]
findPotentialMatchesForStatementItems items companyId = do
  transactionIds <- runDB $ selectKeysList [TransactionCompanyId ==. companyId] []
  transactions <- runDB $ selectList [TransactionCompanyId ==. companyId] []
  entries <- runDB $ selectList [EntryTransactionId <-. transactionIds] [] :: Handler [Entity Entry]

  let result =
        map
          ( \statementItem -> do
              let matches = filter (\entry -> entryAmount (entityVal entry) == bankStatementItemAmount statementItem) entries
              StatementItemWithMatches
                { statementItemWithMatchesStatementItem = statementItem,
                  statementItemWithMatchesEntries = matches
                }
          )
          items

  return result




data OneToMany a b = OneToMany {document::a, details::[b]} deriving Show

type BankStatementPipeline = (OneToMany BankStatement BankStatementItem,[OneToMany Transaction Entry])
                        -> (OneToMany BankStatement BankStatementItem,[OneToMany Transaction Entry])

processNonBankFeeItems' :: BankStatementPipeline
processNonBankFeeItems' (statement, transactions) = do
    let companyId = bankStatementCompanyId (document statement)
    let date = bankStatementDate (document statement)
    let items = details statement
    if (length items > 0)
      then do
        let doc = defTransaction {transactionMemo = (Just "Tiliote"), transactionValid= Just True, transactionDate = date, transactionCompanyId = companyId}
        let nonBankFeeEntries =
             foldl
              (\acc item -> do
                let toBeDecidedAccount = toSqlKey 1528
                let bankAccount = toSqlKey 1701
                let description =
                      ( (bankStatementItemPayerOrReceiver item) ++ " "
                          ++ (bankStatementItemArchiveid item)
                          ++ " "
                          ++ maybe "" ((++) " ") (bankStatementItemMessage item)
                          ++ maybe "" ((++) " ") (bankStatementItemReferencenumber item)
                          ++ maybe "" ((++) " ") (bankStatementItemTransactionType item)
                      ) ::
                        Text

                let matchItems =
                      [ (Or ["ålandsbanken", "telia", "sähkölaitos"], toSqlKey 2330),
                        (Or ["keva ", "tahkola "], toSqlKey 6101),
                        (Or ["verohallinto"], toSqlKey 2364),
                        (And ["juuso vuorinen", "laina"], toSqlKey 2381),
                        (And ["juuso vuorinen", "velka"], toSqlKey 2381),
                        (And ["juuso vuorinen", "palkka"], toSqlKey 2350)
                      ]

                let account =
                      foldl
                        ( \acc item -> do
                            case item of
                              (Or x, n) -> do
                                let result =
                                      foldl
                                        ( \acc item -> do
                                            let found = isInfixOf item (toLower description)
                                            if (found) then True else acc
                                        )
                                        False
                                        x
                                if (result) then n else acc

                              (And x, n) -> do
                                let result =
                                      foldl
                                        ( \acc item -> do
                                            let found = isInfixOf item (toLower description)
                                            if (found) then acc ++ [True] else acc ++ [False]
                                        )
                                        []
                                        x
                                if (all (== True) result) then n else acc
                        )
                        toBeDecidedAccount
                        matchItems

                let entry1 = defEntry { entryMemo = (Just description), entryAccountId = bankAccount, entryAmount = (bankStatementItemAmount item)}
                let entry2 = defEntry { entryMemo = (Just description), entryAccountId = account, entryAmount = (- bankStatementItemAmount item)}

                acc ++ [entry1, entry2]
              )
              []
              items

        let entries' =
              map
                ( \item -> do
                    WithoutId item
                )
                nonBankFeeEntries

        (statement, OneToMany doc nonBankFeeEntries:transactions)
    
      else (statement,transactions)













data SearchType = Or [Text] | And [Text]

processNonBankFeeItems :: BankStatement -> [BankStatementItem] -> DB (Maybe (Key Transaction))
processNonBankFeeItems statement items = do
    let companyId = bankStatementCompanyId statement

    if (length items > 0)
      then do
        let doc = defTransaction {transactionMemo = (Just "Tiliote"), transactionValid= Just True, transactionDate = bankStatementDate statement, transactionCompanyId = companyId}
        --allAccounts <- selectList ([AccountCompanyId ==. companyId]) []
        allAccounts <- liftHandler $ getAccountEntities companyId

        statementAsTransactionId <- insert doc

        nonBankFeeEntries <-
          foldM
            ( \acc item -> do
                let toBeDecidedAccount = 1528
                bankAccount <- liftHandler $ toId 1701 companyId {- 1528 -} --allAccounts
                let description =
                      ( (bankStatementItemPayerOrReceiver item) ++ " "
                          ++ (bankStatementItemArchiveid item)
                          ++ " "
                          ++ maybe "" ((++) " ") (bankStatementItemMessage item)
                          ++ maybe "" ((++) " ") (bankStatementItemReferencenumber item)
                          ++ maybe "" ((++) " ") (bankStatementItemTransactionType item)
                      ) ::
                        Text

                let matchItems =
                      [ (Or ["ålandsbanken", "telia", "sähkölaitos"], 2330),
                        (Or ["keva ", "tahkola "], 6101),
                        (Or ["verohallinto"], 2364),
                        (And ["juuso vuorinen", "laina"], 2381),
                        (And ["juuso vuorinen", "velka"], 2381),
                        (And ["juuso vuorinen", "palkka"], 2350)
                      ]

                let account =
                      foldl
                        ( \acc item -> do
                            case item of
                              (Or x, n) -> do
                                let result =
                                      foldl
                                        ( \acc item -> do
                                            let found = isInfixOf item (toLower description)
                                            if (found) then True else acc
                                        )
                                        False
                                        x
                                if (result) then n else acc

                              (And x, n) -> do
                                let result =
                                      foldl
                                        ( \acc item -> do
                                            let found = isInfixOf item (toLower description)
                                            if (found) then acc ++ [True] else acc ++ [False]
                                        )
                                        []
                                        x
                                if (all (== True) result) then n else acc
                        )
                        toBeDecidedAccount
                        matchItems

                account' <- liftHandler $ toId account companyId --allAccounts
                let entry1 = defEntry {entryTransactionId = statementAsTransactionId, entryMemo = (Just description), entryAccountId = bankAccount, entryAmount = (bankStatementItemAmount item)}
                let entry2 = defEntry {entryTransactionId = statementAsTransactionId, entryMemo = (Just description), entryAccountId = account', entryAmount = (- bankStatementItemAmount item)}

                return $ acc ++ [entry1, entry2]
            )
            []
            items

        let entries' = map (New) nonBankFeeEntries

        let package = DocumentWithNewAndExistingItems {documentFromClient = doc, documentDetailsFromClient =entries'}


        _<-updateParentAndInsertOrUpdateChildren statementAsTransactionId package
        return (Just statementAsTransactionId)
      else return Nothing


processBankFeeItems' :: BankStatementPipeline
processBankFeeItems' (statement, transactions) = do
    let items' = details statement
    let companyId = bankStatementCompanyId (document statement)
    let itemsWithBankingFees = filter (\item -> bankStatementItemTransactionType item == Just "PANKKIMAKSUT") items'

    if (length itemsWithBankingFees > 0) then do
        let newDate = (addGregorianMonthsClip (-1)) (bankStatementItemDate (Data.List.head itemsWithBankingFees))
        let doc = defTransaction {transactionMemo = (Just "PANKKIMAKSUT"), transactionValid = Just True,transactionDate = newDate, transactionCompanyId = companyId}

        let bankFeeEntries =
             foldl
              ( \acc item -> do
                  let bankingFeesAccount = toSqlKey 6940  {- 1528 -} --allAccounts
                  let payablesAccount = toSqlKey 2330

                  let entry1 = defEntry {entryMemo = (Just "PANKKIMAKSUT"), entryAccountId = bankingFeesAccount, entryAmount = (- bankStatementItemAmount item)}
                  let entry2 = defEntry {entryMemo = (Just "PANKKIMAKSUT"), entryAccountId = payablesAccount, entryAmount = (bankStatementItemAmount item)}

                  acc ++ [entry1, entry2]
               )
               []
               itemsWithBankingFees

        let entries' =
              map
                ( \item -> do
                    WithoutId item
                )
                bankFeeEntries
        (statement, OneToMany doc bankFeeEntries:transactions)
      else (statement,transactions)

 




processBankFeeItems :: BankStatement -> [BankStatementItem] -> DB (Maybe (Key Transaction))
processBankFeeItems statement items = do
  let companyId = bankStatementCompanyId statement
  let itemsWithBankingFees = filter (\item -> bankStatementItemTransactionType item == Just "PANKKIMAKSUT") items

  if (length itemsWithBankingFees > 0) then do
      let newDate = (addGregorianMonthsClip (-1)) (bankStatementItemDate (Data.List.head itemsWithBankingFees))
      let doc = defTransaction {transactionMemo = (Just "PANKKIMAKSUT"), transactionValid = Just True,transactionDate = newDate, transactionCompanyId = companyId}
      allAccounts <- liftHandler $ getAccountEntities companyId
      bankFeeDocId <- insert doc

      bankFeeEntries <-
        foldM
          ( \acc item -> do
              bankingFeesAccount <- liftHandler $ toId 6940 companyId {- 1528 -} --allAccounts
              payablesAccount <- liftHandler $ toId 2330 companyId {- 1528 -} --allAccounts
              let entry1 = defEntry {entryTransactionId = bankFeeDocId, entryMemo = (Just "PANKKIMAKSUT"), entryAccountId = bankingFeesAccount, entryAmount = (- bankStatementItemAmount item)}
              let entry2 = defEntry {entryTransactionId = bankFeeDocId, entryMemo = (Just "PANKKIMAKSUT"), entryAccountId = payablesAccount, entryAmount = (bankStatementItemAmount item)}

              return $ acc ++ [entry1, entry2]
          )
          []
          itemsWithBankingFees

      let entries' =
            map
              ( \item -> do
                  WithoutId item
              )
              bankFeeEntries
      let package = Handler.Bookkeeping.CrudEndpoints.Transaction.TransactionWithHeterogenousEntries {transaction = doc, entries = entries'}

      updateBookkeepingTransactionAndInsertOrUpdateEntries companyId bankFeeDocId package
      return (Just bankFeeDocId)
  else return Nothing




postBankStatementFilesR :: CompanyId -> Handler ()
postBankStatementFilesR companyId = do
  x' <- runInputPost $ iopt fileField "file"
  let entityType = "bankStatement"
  _ <- case x' of
    (Just fileInfo) -> do
      random <- liftIO nextRandom
      let filenameWithHash = (show random) ++ (unpack $ fileName fileInfo)
      let newFileInfo = fileInfo {fileName = (pack filenameWithHash)}
      let path = "companies" </> (show $ fromSqlKey companyId) </> entityType </> "tmp" </> filenameWithHash
      savedToTmp <- Handler.Files.Files.saveFile newFileInfo ("companies" </> (show $ fromSqlKey companyId) </> unpack entityType </> "tmp")
      if savedToTmp
        then do
          (statement, items) <- extractBankStatementInformation path
          let statement' = statement {bankStatementCompanyId = companyId}
          docIdBankFee <- runDB $ processBankFeeItems statement' items --companyId
          docIdBankStatement <- runDB $ processNonBankFeeItems statement' items --companyId

          case docIdBankStatement of
            Just x -> do
              let finalFileInfo = fileInfo {fileName = (bankStatementAccount statement) ++ pack (show (bankStatementDate statement) ++ ".pdf")}
              savedToDocuments <- Handler.Files.Files.saveFile finalFileInfo ("companies" </> (show $ fromSqlKey companyId) </> "documents" </> (show $ fromSqlKey x))
              liftIO $ removeFile path

              if (savedToDocuments == True)
                then sendResponseStatus status200 ("All set, bank statement handled and saved::Text" :: Text)
                else sendResponseStatus status404 ("Could not store files! Maybe out of memory on disk" :: Text)
            Nothing -> do
              let finalFileInfo = fileInfo {fileName = (bankStatementAccount statement) ++ pack (show (bankStatementDate statement) ++ ".pdf")}
              liftIO $ removeFile path
        else -- This adds journal entries for the corresponding bank fee entries
          sendResponseStatus status200 ("Nothing here! " :: Text)
    _ -> sendResponseStatus status200 ("Nothing here! " :: Text)
  sendResponseStatus status200 ("Nothing here! " :: Text)

saveOne :: CompanyId -> OneToMany Transaction Entry -> DB TransactionId
saveOne companyId x = do
          x' <- transformAccountId (transactionCompanyId (document x)) [x]
          docId <- insert (document (Data.List.head x'))
          keys <- mapM (\detail-> insertEntries companyId [(detail {entryTransactionId=docId})] (transactionDate (document x))) (details (Data.List.head x'))
     
          return docId
         
saveMany :: CompanyId -> [OneToMany Transaction Entry] -> DB ()
saveMany companyId x = do
        mapM_ (\transaction-> do
              docId <- insert (document transaction)
              transformedTransaction <- transformAccountId (transactionCompanyId (document transaction)) [transaction]
              keys <- mapM (\detail-> insertEntries companyId [(detail {entryTransactionId=docId})] (transactionDate (document transaction))) (details (Data.List.head transformedTransaction))
              return ()
              ) x
        return ()

transformAccountId :: CompanyId 
                      -> [OneToMany Transaction Entry] 
                      -> DB [OneToMany Transaction Entry]  
transformAccountId companyId bankStatementProcessingResult = do

          bankStatementProcessingResult' <- mapM(\item->do
                newEntries <- mapM (\detail-> do
                    let accountNumber = fromIntegral(fromSqlKey(entryAccountId detail))
                    z <- toId'' accountNumber companyId
                    return detail {entryAccountId = z} 
                    ) (details item)
                return $ OneToMany (document item) newEntries
                ) bankStatementProcessingResult
          return bankStatementProcessingResult'

postBankStatementFilesR'' :: CompanyId -> Handler ()
postBankStatementFilesR'' companyId = do

  files <- lookupFiles "files"
  putStrLn ("Files to be processed")
  putStrLn ("---------------------")
  mapM (\file->putStrLn (fileName file)) files
  putStrLn ("---------------------")

  let entityType = "bankStatement"
  results <- case files of
    (x:xs) -> do
      mapM(\fileInfo-> do
        random <- liftIO nextRandom
        let filenameWithHash = (show random) ++ (unpack $ fileName fileInfo)
        let newFileInfo = fileInfo {fileName = (pack filenameWithHash)}
        let path = "companies" </> (show $ fromSqlKey companyId) </> entityType </> "tmp" </> filenameWithHash

        savedToTmp <- Handler.Files.Files.saveFile newFileInfo ("companies" </> (show $ fromSqlKey companyId) </> unpack entityType </> "tmp")
  
        if savedToTmp
          then do
            -- The following will fail if extraction filas
            (statement, items) <- extractBankStatementInformation path

            -- Attach companyId to the statement 
            let statement' = statement {bankStatementCompanyId = companyId}
            -- Define processing steps
            let processingPipeline = processBankFeeItems' 


            let (_,bankStatementProcessingResult) = processNonBankFeeItems' (OneToMany statement' items,[])
            case bankStatementProcessingResult of
              (x:xs)-> do
                  let ( _, bankFeeProcessingResult) = processBankFeeItems' (OneToMany statement' items,[])

                  transactionIdOfBankStatement <- runDB $ do
                    saveMany companyId bankFeeProcessingResult
                    saveOne companyId (Data.List.head bankStatementProcessingResult)
   

                  putStrLn ("---------------OK.............")

                  let finalFileInfo = fileInfo {fileName = (bankStatementAccount statement) ++ pack (show (bankStatementDate statement) ++ ".pdf")}
   
                  savedToDocuments <- Handler.Files.Files.saveFile finalFileInfo ("companies" </> (show $ fromSqlKey companyId) </> "documents" </> (show $ fromSqlKey transactionIdOfBankStatement))
                  liftIO $ removeFile path

                  if (savedToDocuments == True)
                    then do
                      putStrLn ("File was successfully processed and saved: "++fileName fileInfo)
                      return ("File was successfully processed and saved: "++fileName fileInfo)
                    else do
                      putStrLn ("Could not store files! Maybe out of memory on disk: " ++fileName fileInfo)
                      sendResponseStatus status404 ("Could not store files! Maybe out of memory on disk" :: Text)

              
              [] -> sendResponseStatus status200 ("Could not find any entries in bank statement" :: Text)

        else 
            sendResponseStatus status200 ("Cannot save to /tmp directory!" :: Text)
        ) files

    [] -> sendResponseStatus status200 ("No files were posted!" :: Text)

  putStrLn "Parsing results for bank staments:"
  putStrLn "----------------------------------"
  mapM_ (\info->do
    putStrLn info) results
  sendResponseStatus status200 $ toJSON results

postBankStatementFilesR' :: CompanyId -> Handler ()
postBankStatementFilesR' companyId = do
  files <- lookupFiles "files"
  putStrLn ("Files to be processed")
  putStrLn ("---------------------")
  mapM_ (putStrLn . fileName) files

  let entityType = "bankStatement"
  results <- case files of
    (x:xs) -> do
      mapM(\fileInfo-> do
        random <- liftIO nextRandom
        let filenameWithHash = (show random) ++ (unpack $ fileName fileInfo)
        let newFileInfo = fileInfo {fileName = (pack filenameWithHash)}
        let path = "companies" </> (show $ fromSqlKey companyId) </> entityType </> "tmp" </> filenameWithHash
        savedToTmp <- Handler.Files.Files.saveFile newFileInfo ("companies" </> (show $ fromSqlKey companyId) </> unpack entityType </> "tmp")
        if savedToTmp
          then do
            (statement, items) <- extractBankStatementInformation path
            let statement' = statement {bankStatementCompanyId = companyId}
            docIdBankFee <- runDB $ processBankFeeItems statement' items --companyId
            docIdBankStatement <- runDB $ processNonBankFeeItems statement' items --companyId

            case docIdBankStatement of

              -- Document was created
              Just x -> do
                let finalFileInfo = fileInfo {fileName = (bankStatementAccount statement) ++ pack (show (bankStatementDate statement) ++ ".pdf")}
                savedToDocuments <- Handler.Files.Files.saveFile finalFileInfo ("companies" </> (show $ fromSqlKey companyId) </> "documents" </> (show $ fromSqlKey x))
                liftIO $ removeFile path

                if (savedToDocuments == True)
                  then do
                    putStrLn ("File was successfully processed and saved: "++fileName fileInfo)
                    return ("File was successfully processed and saved: "++fileName fileInfo)

                  else do
                    putStrLn ("Could not store files! Maybe out of memory on disk: " ++fileName fileInfo)

                    sendResponseStatus status404 ("Could not store files! Maybe out of memory on disk" :: Text)

              -- No document was not created - bank statement has no items 
              Nothing -> do
                let finalFileInfo = fileInfo {fileName = (bankStatementAccount statement) ++ pack (show (bankStatementDate statement) ++ ".pdf")}
                liftIO $ removeFile path
                return ("File was successfully processed but had no items: "++fileName fileInfo)

          else 
            sendResponseStatus status200 ("Cannot save to /tmp directory!" :: Text)
        ) files

    [] -> sendResponseStatus status200 ("No files were posted!" :: Text)

  putStrLn "Parsing results for bank staments:"
  putStrLn "----------------------------------"
  mapM (\info->do
    putStrLn info) results
  sendResponseStatus status200 $ toJSON results


postFilesR :: CompanyId -> Handler Value
postFilesR companyId' = do
  x' <- do
    fileInfo <- runInputPost $ iopt fileField "file"
    companyId <- runInputPost $ iopt textField "companyId"
    entityId <- runInputPost $ iopt textField "entityId"
    entityType <- runInputPost $ iopt textField "entityType"
    return (fileInfo, companyId, entityId, entityType)
  case x' of
    (Just fileInfo, Just companyId, Just entityId, Just entityType) -> do
      saved <- Handler.Files.Files.saveFile fileInfo ("companies" </> unpack companyId </> unpack entityType </> unpack entityId)
      if saved then return sOk else return sExists
    _ -> return sError

postFilesMonthlyR :: CompanyId->Handler Value
postFilesMonthlyR companyId = do
  x' <- do
    fileInfo <- runInputPost $ iopt fileField "file"
    companyId <- runInputPost $ iopt textField "companyId"
    return (fileInfo, companyId)
  case x' of
    (Just fileInfo, Just companyId) ->
      do
        putStrLn companyId
        putStrLn (fileName fileInfo)

        let companyIdInt64 = read (unpack companyId) :: Int64
        let cid = toSqlKey companyIdInt64
        time <- liftIO getZonedTime

        let (year, monthOfYear, dayOfMonth) = toGregorian $ localDay $ zonedTimeToLocalTime time
        let end = gregorianMonthLength year monthOfYear
        let firstDay = fromGregorian year monthOfYear 1
        let lastDay = fromGregorian year monthOfYear end

        x <- runDB $ selectFirst [TransactionType ==. TypeGeneralExpenseMonthly, TransactionCompanyId ==. cid, TransactionDate >=. firstDay, TransactionDate <=. lastDay] [] :: Handler (Maybe (Entity Transaction))
        case x of
          Just doc -> do
            let key = entityKey doc
            let docIdKey = fromSqlKey key
            let filePath = "companies" </> unpack companyId </> "documents" </> show docIdKey
            Handler.Files.Files.saveFile fileInfo filePath
            return ()

          Nothing -> do
            let doc = defTransaction {transactionCompanyId = cid, transactionType = TypeGeneralExpenseMonthly, transactionMemo = Just "Saapuneet tositteet", transactionDate = lastDay}
            id <- runDB $ insert doc
            let docIdKey = fromSqlKey id

            let filePath = "companies" </> unpack companyId </> "transactions" </> show docIdKey
            Handler.Files.Files.saveFile fileInfo filePath

            return ()

    _ -> return ()
  sendResponseStatus status200 ("OK" :: Text)

deleteFilesR2 :: CompanyId -> EntityType -> EntityId -> FileName -> Handler ()
deleteFilesR2 companyId entityType entityId fileName = do
  let filePath = "companies" </> show (fromSqlKey companyId) </> unpack entityType </> show entityId </> unpack fileName
  liftIO $ removeFile filePath
  sendResponseStatus status200 ("DELETED" :: Text)

listHandler :: IOError -> IO [FilePath]
listHandler e
  | isDoesNotExistError e = putStrLn "The directory does not exist" >> return []
  | otherwise = putStrLn "Something went wrong" >> return []

getFileListR2 :: CompanyId -> EntityType -> EntityId -> Handler Value
getFileListR2 companyId entityType entityId = do
  let filePath = "companies" </> show (fromSqlKey companyId) </> unpack entityType </> show entityId
  fileList <- liftIO $ listDirectory filePath `catch` listHandler
  returnJson fileList

helperGetFileList :: CompanyId -> EntityType -> EntityId -> IO [FilePath]
helperGetFileList companyId entityType entityId = do
  listDirectory ("companies" </> (show $ fromSqlKey companyId) </> (unpack entityType) </> (show entityId))

saveFile :: FileInfo -> FilePath -> Handler Bool
saveFile file directory = do
  $(logInfo) "Saving file...."
  let filename = unpack $ fileName file
  print filename

  let dest = directory </> filename
  print dest
  exists <- liftIO $ doesFileExist dest
  if exists
    then return False
    else do
      liftIO $ createDirectoryIfMissing True $ directory
      liftIO $ fileMove file dest
      return True

--mkThumbnail :: ByteString -> IO (Either String Thumbnail)
--      f <- readFile dest
--      thumbnail <- liftIO $ mkThumbnail (fromStrict f)
--      case thumbnail of
--        Right x -> do
--          liftIO $ Graphics.Thumbnail.saveFile x (directory</>(unpack("thumb_"++filename)))
--          putStrLn("Thumbnali created ok"){- writeFile (directory</>(unpack("thumb_"++filename))) x -}
--        Left _-> putStrLn("Thumbnail creating failed")
--      fileLengthInBytes <- liftIO $ getFileSize dest
--      putStrLn (pack(show fileLengthInBytes))
{- Control.Monad.when (fileLengthInBytes > 512000) $ do
         fileData <- liftIO $ readImage dest

         case fileData of
           Left s -> putStrLn ("Image type not recognized")
           Right (ImageYCbCr8 image) -> do
             putStrLn ("Started processing 1")
             let lessQuality = toStrict $ encodeJpegAtQuality 80 image
             putStrLn "Finished processing 1"

             putStrLn ("Started processing 2")

             let lessQuality' = toStrict $ encodeJpegAtQuality 50 image
             putStrLn ("Finished processing 2")

             putStrLn ("Started storing 1")

             writeFile (unpack (directory </>"80_"++unpack(fileName file))) lessQuality
             putStrLn ("Stored 1")

             putStrLn ("Started storing 2")

             writeFile (unpack (directory </>"50_"++unpack(fileName file))) lessQuality'
             putStrLn ("Stored 2")

             return ()
           Right (_) ->return ()
     return True)
-}
