{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TypeApplications          #-}

module Euler.Product.OLTP.Card.Card where


import           EulerHS.Prelude hiding (id)

import           Euler.API.CardPS
import           Euler.Storage.Repository.EulerDB
import           Euler.Storage.Types.CardInfo
import           Euler.Storage.Types.EulerDB as EDB
import           EulerHS.Language

import           Data.Generics.Product.Fields
import qualified Data.List as L
import qualified Data.Text as T
import           Database.Beam ((==.))
import qualified Database.Beam as B
import           Text.RE.TDFA.Text


-- ----------------------------------------------------------------------------
-- function: encryptCardData
-- TODO port
-- ----------------------------------------------------------------------------

-- encryptCardData :: forall a b. CardData -> BackendFlow a b String
-- encryptCardData x = do
--  keyForEncryption <- doAffRR' "ecTempCardCred" ecTempCardCred
--  let encryptedData = encryptAESRaw "aes-256-ecb" (getCardDataString x) keyForEncryption Nothing
--  case encryptedData of
--   Right result -> pure (hexToBase64 result)
--   Left err -> throwErr $ "Error while Encrypting Card Data "<> show err

-- ----------------------------------------------------------------------------
-- function: decryptCardData
-- TODO port
-- ----------------------------------------------------------------------------

-- decryptCardData :: forall a b. String -> BackendFlow a b String
-- decryptCardData x = do
--   keyForDecryption <- doAffRR' "ecTempCardCred" ecTempCardCred
--   case (decryptAESRaw "aes-256-ecb" (base64ToHex x) keyForDecryption Nothing) of
--     Right result -> pure result
--     Left err -> throwErr $ "Error in decrypppppting card data "<>show err

-- ----------------------------------------------------------------------------
-- isin :: CardData -> String
-- done
-- ----------------------------------------------------------------------------

-- isin :: CardData -> String
-- isin cardData = take 6 (cardData ^. _cardNumber)

isin :: CardData -> Text
isin cardData = T.take 6 (getField @"cardNumber" cardData)

-- ----------------------------------------------------------------------------
-- function: getCardBrandFromIsin
-- done
-- ----------------------------------------------------------------------------

-- getCardBrandFromIsin ::forall st rt e. Newtype st (TState e) => String -> BackendFlow st _ (Maybe String)
-- getCardBrandFromIsin cardIsin = do
--   cardBrand <- pure $ getCardBrand cardIsin
--   case cardBrand of
--     "" -> do
--       cardInfo <- DB.findOne ecDB (where_ := WHERE ["card_isin" /\ String cardIsin] :: WHERE CardInfo)
--       cardSwitchProvider <- pure $ maybe "" ( _.cardSwitchProvider <<< unwrap) cardInfo
--       if (cardSwitchProvider == "") then (pure Nothing) else pure $ Just cardSwitchProvider
--     _ -> pure $ Just cardBrand

getCardBrandFromIsin :: Maybe Text -> Flow (Maybe Text)
getCardBrandFromIsin Nothing = pure Nothing
getCardBrandFromIsin (Just cardIsin') = do
  let mCardBrand = getCardBrand cardIsin'
  case mCardBrand of
    Nothing -> do
      cardInfo <- withEulerDB $ do
        let predicate CardInfo {cardIsin} = cardIsin ==. B.val_ cardIsin'
        findRow
          $ B.select
          $ B.limit_ 1
          $ B.filter_ predicate
          $ B.all_ (EDB.card_info eulerDBSchema)
      pure $ getField @"cardSwitchProvider" <$> cardInfo
    _ -> pure mCardBrand


-- ----------------------------------------------------------------------------
-- function: getCardDetailsFormat
-- TODO port
-- ----------------------------------------------------------------------------

-- getCardDetailsFormat :: TransactionCreateReq -> CardDetailsFormat
-- getCardDetailsFormat req =
--   let isNewCard   = isNotNull $ req ^. _card_number
--       hasToken    = isNotNull $ req ^. _card_token
--       isTokenized = fromMaybe false (startsWith cardTokenPrefix <$> (unNullOrUndefined $ req ^. _card_token))
--   in case [isNewCard, hasToken, isTokenized] of
--       [true, _, _]     -> NewCard
--       [_, true, true]  -> Tokenized
--       [_, true, false] -> SavedCard
--       _                -> NotSupported

-- ----------------------------------------------------------------------------
-- function: getRupayCardRange
-- done
-- ----------------------------------------------------------------------------

-- getRupayCardRange :: Array (Tuple Int Int)
-- getRupayCardRange = [
--   (Tuple 508227 508227), (Tuple 508500 508999), (Tuple 603741 603741), (Tuple 606985 607384),
--   (Tuple 607385 607484), (Tuple 607485 607984), (Tuple 608001 608100), (Tuple 608101 608200),
--   (Tuple 608201 608300), (Tuple 608301 608350), (Tuple 608351 608500), (Tuple 652150 652849),
--   (Tuple 652850 653049), (Tuple 653050 653149) ]

getRupayCardRange :: [(Int, Int)]
getRupayCardRange = [
  (508227, 508227), (508500, 508999), (603741, 603741), (606985, 607384),
  (607385, 607484), (607485, 607984), (608001, 608100), (608101, 608200),
  (608201, 608300), (608301, 608350), (608351, 608500), (652150, 652849),
  (652850, 653049), (653050, 653149) ]

-- ----------------------------------------------------------------------------
-- function: inRange
-- done
-- ----------------------------------------------------------------------------

-- inRange :: String -> (Tuple Int Int) -> Boolean
-- inRange numStr tup = (\num -> ((num >= (fst tup)) && (num <= (snd tup)))) $ (stringToIntDefaultZero numStr)

inRange :: Text -> (Int, Int) -> Bool
inRange numStr tup = (\num -> ((num >= (fst tup)) && (num <= (snd tup))))
  $ stringToIntDefaultZero numStr

-- ----------------------------------------------------------------------------
-- function: stringToIntDefaultZero
-- done
-- ----------------------------------------------------------------------------

-- From Utils.Utils
-- stringToIntDefaultZero :: String -> Int
-- stringToIntDefaultZero str = maybe 0 (\x -> x) $ Int.fromString str

stringToIntDefaultZero :: Text -> Int
stringToIntDefaultZero str = fromMaybe 0 ((readMaybe $ T.unpack str) :: Maybe Int)

-- ----------------------------------------------------------------------------
-- function: checkRupayCard
-- done
-- ----------------------------------------------------------------------------

-- checkRupayCard :: String -> Boolean
-- checkRupayCard isin = (length (filter (inRange isin) getRupayCardRange)) > 0

checkRupayCard :: Text -> Bool
checkRupayCard isin' = length (filter (inRange isin') getRupayCardRange) > 0

-- ----------------------------------------------------------------------------
-- function: checkMaestroCard
-- done
-- ----------------------------------------------------------------------------

-- checkMaestroCard :: String -> Boolean
-- checkMaestroCard isin = test "^(5018|5081|5044|504681|504993|5020|502260|5038|603845|603123|6304|6759|676[1-3]|6220|504834|504817|504645|504775)" isin

checkMaestroCard :: Text -> Bool
checkMaestroCard isin' = matched $
  isin' ?=~ [re|^(5018|5081|5044|504681|504993|5020|502260|5038|603845|603123|6304|6759|676[1-3]|6220|504834|504817|504645|504775)|]

-- ----------------------------------------------------------------------------
-- function: checkMasterCard
-- done
-- ----------------------------------------------------------------------------

-- checkMasterCard :: String -> Boolean
-- checkMasterCard isin = test "^(51|52|53|54|55)" isin

checkMasterCard :: Text -> Bool
checkMasterCard isin' = matched $ isin' ?=~ [re|^(51|52|53|54|55)|]

-- ----------------------------------------------------------------------------
-- function: checkVisaCard
-- done
-- ----------------------------------------------------------------------------

-- checkVisaCard :: String -> Boolean
-- checkVisaCard isin = test "^4" isin

checkVisaCard :: Text -> Bool
checkVisaCard isin' = matched $ isin' ?=~ [re|^4|]

-- ----------------------------------------------------------------------------
-- function: checkAmexCard
-- done
-- ----------------------------------------------------------------------------

-- checkAmexCard :: String -> Boolean
-- checkAmexCard isin = test "^(34|37)" isin

checkAmexCard :: Text -> Bool
checkAmexCard isin' = matched $ isin' ?=~ [re|^(34|37)|]

-- ----------------------------------------------------------------------------
-- function: checkDinersCard
-- done
-- ----------------------------------------------------------------------------

-- checkDinersCard :: String -> Boolean
-- checkDinersCard isin = test "^36|38|(30[0-5])" isin

checkDinersCard :: Text -> Bool
checkDinersCard isin' = matched $ isin' ?=~ [re|^36|38|(30[0-5])|]

-- ----------------------------------------------------------------------------
-- function: checkDiscoversCard
-- done
-- ----------------------------------------------------------------------------

-- checkDiscoversCard :: String -> Boolean
-- checkDiscoversCard isin = test "^6011|65|64[4-9]|622" isin

checkDiscoversCard :: Text -> Bool
checkDiscoversCard isin' = matched $ isin' ?=~ [re|^6011|65|64[4-9]|622|]

-- ----------------------------------------------------------------------------
-- function: checkJCBCard
-- done
-- ----------------------------------------------------------------------------

-- checkJCBCard :: String -> Boolean
-- checkJCBCard isin = test "^35" isin

checkJCBCard :: Text -> Bool
checkJCBCard isin' = matched $ isin' ?=~ [re|^35|]

-- ----------------------------------------------------------------------------
-- function: checkSodexoCard
-- done
-- ----------------------------------------------------------------------------

-- checkSodexoCard :: String -> Boolean
-- checkSodexoCard isin = test "^637513" isin

checkSodexoCard :: Text -> Bool
checkSodexoCard isin' = matched $ isin' ?=~ [re|^637513|]

-- ----------------------------------------------------------------------------
-- function: allCardsFn
-- done
-- ----------------------------------------------------------------------------

-- allCardsFn :: Array (Tuple (String -> Boolean) String)
-- allCardsFn = [Tuple checkMasterCard "MASTERCARD", Tuple checkVisaCard "VISA",
--   Tuple checkAmexCard "AMEX", Tuple checkMaestroCard "MAESTRO",
--   Tuple checkRupayCard "RUPAY", Tuple checkDinersCard "DINERS",
--   Tuple checkDiscoversCard "DISCOVER", Tuple checkJCBCard "JCB",
--   Tuple checkSodexoCard "SODEXO"]

allCardsFn :: [((Text -> Bool), Text)]
allCardsFn =
  [ (checkMasterCard, "MASTERCARD")
  , (checkVisaCard, "VISA")
  , (checkAmexCard, "AMEX")
  , (checkMaestroCard, "MAESTRO")
  , (checkRupayCard, "RUPAY")
  , (checkDinersCard, "DINERS")
  , (checkDiscoversCard, "DISCOVER")
  , (checkJCBCard, "JCB")
  , (checkSodexoCard, "SODEXO")
  ]

-- ----------------------------------------------------------------------------
-- function: evalCardISIN
-- done
-- ----------------------------------------------------------------------------

-- evalCardISIN :: String -> (Tuple (String -> Boolean) String)-> Boolean
-- evalCardISIN isin (Tuple fn val) = fn isin

evalCardISIN :: Text -> ((Text -> Bool), Text)-> Bool
evalCardISIN isin' (fn, _) = fn isin'

-- ----------------------------------------------------------------------------
-- function: getCardBrand
-- done
-- ----------------------------------------------------------------------------

-- getCardBrand :: String -> String
-- getCardBrand cardISIN = maybe "" (\idx -> (maybe "" snd (index allCardsFn idx))) (findIndex (evalCardISIN (trim cardISIN)) allCardsFn)

getCardBrand :: Text -> Maybe Text
getCardBrand cardISIN = snd <$> L.find (evalCardISIN $ T.strip cardISIN) allCardsFn
