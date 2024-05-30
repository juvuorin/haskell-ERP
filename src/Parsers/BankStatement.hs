{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Parsers.BankStatement where

import ClassyPrelude
import Control.Monad (
  MonadFail (fail),
 )
import Data.Array ((!))
import Data.List (foldl, tail, (!!))
import Data.Text (breakOnEnd, replace, splitOn, strip, takeEnd)
import Database.Persist.Postgresql (toSqlKey)
import Import hiding (document, getStartDate, getEndDate)
import Pdf.Document (
  catalogPageNode,
  defaultUserPassword,
  document,
  documentCatalog,
  isEncrypted,
  pageExtractText,
  pageNodeNKids,
  pageNodePageByNum,
  setUserPassword,
  withPdfFile,
 )
import Text.Regex.Posix
import Prelude (read, uncurry, head)

{- | This module parses Finnish bank statements for Ålandsbanken bank. Nothice that statements in Swedish do
 | need a separate algorithm
-}
regAccountNumber = "FI[0-9]{2} [0-9]{4} [0-9]{4} [0-9]{4} [0-9]{2}" :: String
regSaldo = "SALDO [0-9]{2}[.][0-9]{2}[.][0-9]{2} [0-9]+[.][0-9]+[,][0-9]{2}[+-]" :: String
regArchiveIdWithoutLetter = "[0-9]{18} [0-9]{4}" :: String
regArchiveId = "[0-9]{18} [A-Z]|[0-9]{18} [0-9]{4}" :: String
regRefNumber = "[0-9]{20}|RF[0-9]{4,22}" :: String
regTransactionDate = " [0-9]{4} " :: String
regEndOfLastItem = "YHTEENVETOTIEDOT" :: String
getAmountFromBlock x = getAmount $ snd $ breakOnEnd " " (Prelude.head x)

-- "220125602900062743 A 2501 xxx xxxx 8 5.000,00+"
getPayerOrReceiverFromBlock :: [Text] -> Text
getPayerOrReceiverFromBlock text = do
  let archiveId = (pack $ (unpack (Prelude.head text) :: String) =~ regArchiveId) :: Text
  if length archiveId == 20
    then do
      let lastPart = fst $ breakOnEnd " " (fst $ breakOnEnd " " (Prelude.head text))
      strip $ drop 26 $ take (length lastPart -3) lastPart
    else do
      let lastPart = fst $ breakOnEnd " " (fst $ breakOnEnd " " (Prelude.head text))
      strip $ drop 24 $ take (length lastPart -3) lastPart

-- " 1701 E-LASKU"
getTransactionTypeFromBlock :: [Text] -> Maybe Text
getTransactionTypeFromBlock text =
  if length text > 1
    then do
      let reg = " [0-9]{4} " :: String
      let typeInfoPosition = matchOnce (makeRegex reg :: Regex) (unpack $ text !! 1)

      case typeInfoPosition of
        Nothing -> Nothing
        Just x ->
          Just $ drop (fst (x ! 0) + snd (x ! 0)) (text !! 1)
    else Nothing

getArchiveId :: [Text] -> Text
getArchiveId text = do
  let archiveId = (pack $ (unpack (Prelude.head text) :: String) =~ regArchiveId) :: Text
  if length archiveId == 20
    then archiveId
    else take 18 archiveId

-- " 1701 E-LASKU"
getDayMonth :: [Text] -> (Int, Int)
getDayMonth text = do
  let date = strip $ pack $ (unpack (Prelude.head text) :: String) =~ (" [0-9]{4} " :: String)
  let day = read (take 2 (unpack date)) :: Int
  let month = read (drop 2 (unpack date)) :: Int
  (day, month)

-- " 1701 E-LASKU"
getMessageFromBlock :: [Text] -> Maybe Text
getMessageFromBlock text =
  if length text > 1
    then do
      let typeInfoExists = (unpack (text !! 1) :: String) =~ (" [0-9]{4} " :: String) :: Bool
      if typeInfoExists
        then do
          case getRefNumberFromBlock text of
            Just x -> Nothing
            Nothing -> Just $ strip $ text !! 2
        else Just $ strip $ text !! 1
    else Nothing

-- " 00000000000071346670" | " RF00000000000071346670"
getRefNumberFromBlock :: [Text] -> Maybe Text
getRefNumberFromBlock text =
  if length text > 2
    then do
      let refNumber = (unpack (text !! 2) :: String) =~ regRefNumber :: String
      if refNumber == "" then Nothing else Just (pack refNumber)
    else Nothing

getAmount x =
  read
    ( ( unpack
          . removePlusSign
          . moveMinusSignToFront
          . changeDecimalCommaToDecimalPoint
          . removeThousandSeparator
          . findAmount
      )
        x
    ) ::
    Double

findAmount x = snd $ breakOnEnd " " x
removeThousandSeparator :: Text -> Text
removeThousandSeparator = Data.Text.replace "." ""
changeDecimalCommaToDecimalPoint = Data.Text.replace "," "."
moveMinusSignToFront :: Text -> Text
moveMinusSignToFront x =
  if elem '-' x
    then "-" ++ Data.Text.replace "-" "" x
    else x
removePlusSign :: Text -> Text
removePlusSign = Data.Text.replace "+" ""

getAccountNumber :: (MonoFoldable mono, Element mono ~ Char) => mono -> String
getAccountNumber txt = do
  let blocks = (unpack txt :: String) =~ regAccountNumber
  unpack $ Data.Text.replace " " "" (pack blocks)

getStartBalance :: (MonoFoldable mono, Element mono ~ Char) => mono -> Double
getStartBalance txt = do
  let blocks = getAllTextMatches $ (unpack txt :: String) =~ regSaldo :: [String]
  if length blocks /= 2 then error "Problems!" else getAmount (pack $ Prelude.head blocks)

getEndBalance :: (MonoFoldable mono, Element mono ~ Char) => mono -> Double
getEndBalance txt = do
  let blocks = getAllTextMatches $ (unpack txt :: String) =~ regSaldo :: [String]
  if length blocks /= 2 then error "Problems!" else getAmount (pack $ blocks !! 1)

getStartDate :: (MonoFoldable mono, Element mono ~ Char) => mono -> Day
getStartDate txt = do
  let blocks = getAllTextMatches $ (unpack txt :: String) =~ regSaldo :: [String]
  if length blocks /= 2
    then error "Problems!"
    else do
      let endDateString = ((unpack blocks !! 1) :: String) =~ ("[0-9]{2}[.][0-9]{2}[.][0-9]{2}" :: String)
      let dateList = splitOn "." (pack endDateString)
      let endDate = fromGregorian (read (unpack $ "20" ++ dateList !! 2) :: Integer) (read (unpack $ dateList !! 1) :: Int) (read (unpack $ Prelude.head dateList) :: Int)
      let startDate = fromGregorian (read (unpack $ "20" ++ dateList !! 2) :: Integer) (read (unpack $ dateList !! 1) :: Int) 1
      startDate

getEndDate txt = do
  let blocks = getAllTextMatches $ (unpack txt :: String) =~ regSaldo :: [String]
  if length blocks /= 2
    then error "Problems!"
    else do
      let endDateString = ((unpack blocks !! 1) :: String) =~ ("[0-9]{2}[.][0-9]{2}[.][0-9]{2}" :: String)
      let dateList = splitOn "." (pack endDateString)
      let endDate = fromGregorian (read (unpack $ "20" ++ dateList !! 2) :: Integer) (read (unpack $ dateList !! 1) :: Int) (read (unpack $ Prelude.head dateList) :: Int)
      endDate

getYear txt = do
  let blocks = getAllTextMatches $ (unpack txt :: String) =~ regSaldo :: [String]
  if length blocks /= 2
    then error "Problems!"
    else do
      let endDateString = ((unpack blocks !! 1) :: String) =~ ("[0-9]{2}[.][0-9]{2}[.][0-9]{2}" :: String)
      let dateList = splitOn "." (pack endDateString)
      (read (unpack $ "20" ++ dateList !! 2) :: Integer)

findStatementBlocksByArchiveIdAndCleanUpText :: Text -> Import.Handler [[Text]]
findStatementBlocksByArchiveIdAndCleanUpText text = do
  let blocks = getAllMatches ((unpack text :: String) =~ regArchiveId :: AllMatches [] (MatchOffset, MatchLength))
  let lastBlockEnd = matchOnce (makeRegex regEndOfLastItem :: Regex) (unpack text)
  case lastBlockEnd of
    Nothing -> sendResponseStatus status404 ("YHTEENVETOTIEDOT tekstiä ei löydy tiliotteelta!" :: Text)
    Just x -> do
      let getValue o l = takeEnd l $ take (o + l) text
      let blocksWithoutLast = take (length blocks -1) blocks
      let blocksWithoutFirst = Data.List.tail blocks
      let lastMarker = [(fst $ x ! 0, snd $ x ! 0)]
      let zipped' = zipWith (\start end -> (fst start, fst end - fst start)) blocksWithoutLast blocksWithoutFirst
      let zipped = zipped' ++ zipWith (\start end -> (fst start, fst end - fst start)) [blocks !! (length blocks -1)] lastMarker

      let blockValues = map (Prelude.uncurry getValue) zipped
      print "block values coming here:"
      print blockValues
      return $
        Data.List.foldl
          ( \acc item -> do
              let splitted = splitOn "\n" item
              let hasIds i = (unpack i :: String) =~ regArchiveId :: Bool
              let hasJatkuu i = (unpack i :: String) =~ ("^ \\* JATKUU" :: String) :: Bool
              let keepThese = filter (\item -> hasIds item || not (hasJatkuu item)) splitted
              acc ++ [keepThese]
          )
          []
          blockValues

getPdfInText :: FilePath -> IO Text
getPdfInText file =
  withPdfFile file $ \pdf -> do
  encrypted <- isEncrypted pdf
  when encrypted $ do
    ok <- setUserPassword pdf defaultUserPassword
    unless ok $
      fail "need password"
  doc <- document pdf
  catalog <- documentCatalog doc
  rootNode <- catalogPageNode catalog
  count <- pageNodeNKids rootNode
  textages <-
    mapM
      ( \pageNumber -> do
          page <- pageNodePageByNum rootNode pageNumber
          pageExtractText page
      )
      [0 .. (count -1)]
  return $ concat textages

extractBankStatementInformation :: String -> Import.Handler (BankStatement, [BankStatementItem])
extractBankStatementInformation file = do
  text <- liftIO $ getPdfInText file
  print text
  print $ "tili:" ++ Data.Text.replace " " "" (pack $ getAccountNumber text)
  print $ "alkusaldo:" ++ show (getStartBalance text)
  print $ "loppusaldo:" ++ show (getEndBalance text)
  print $ "jakson alku:" ++ show (getStartDate text)
  print $ "jakson loppu:" ++ show (getEndDate text)

  blocks <- findStatementBlocksByArchiveIdAndCleanUpText text
  print blocks

  items <-
    ClassyPrelude.mapM
      ( \block -> do
          let (day, month) = getDayMonth block

          print $ "Date:" ++ show day ++ "." ++ show month ++ "." ++ show (getYear text)
          print $ getArchiveId block
          print $ getAmountFromBlock block
          print $ getPayerOrReceiverFromBlock block
          print $ getTransactionTypeFromBlock block
          print $ getRefNumberFromBlock block
          print $ getMessageFromBlock block

          print "-------------------------------"

          return $
            BankStatementItem
              { bankStatementItemBankStatementId = toSqlKey 0
              , bankStatementItemAmount = getAmountFromBlock block
              , bankStatementItemPayerOrReceiver = getPayerOrReceiverFromBlock block
              , bankStatementItemArchiveid = getArchiveId block
              , bankStatementItemDate = fromGregorian (getYear text) month day
              , bankStatementItemReferencenumber = getRefNumberFromBlock block
              , bankStatementItemMessage = getMessageFromBlock block
              , bankStatementItemTransactionType = getTransactionTypeFromBlock block
              , bankStatementItemTransactionId = Nothing
              }
      )
      blocks
  return
    ( BankStatement
        { bankStatementDate = getEndDate text
        , bankStatementAccount = pack $ getAccountNumber text
        , bankStatementCompanyId = toSqlKey 0
        }
    , items
    )
