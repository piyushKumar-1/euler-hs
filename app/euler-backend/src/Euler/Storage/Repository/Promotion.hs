{-# LANGUAGE NamedFieldPuns  #-}

module Euler.Storage.Repository.Promotion where

import           EulerHS.Prelude hiding (id, Key)
import           EulerHS.Prelude as P

import           EulerHS.Extra.Validation
import           EulerHS.Language

import qualified Euler.Encryption as E
import           Euler.Lens
import qualified Euler.Config.Config as Config

import qualified Euler.Common.Types as C
import           Euler.Common.Errors.PredefinedErrors
import           Euler.Common.Validators (amountValidators, notNegative, textNotEmpty)

import qualified Euler.Product.Domain as D

import           Euler.Storage.DBConfig
import qualified Euler.Storage.Types as DB

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson             as A
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy.Char8 as LC
-- import           Data.Generics.Product.Fields
import           Database.Beam ((&&.), (==.))
import qualified Database.Beam as B
import qualified Data.ByteString.Lazy   as BSL
import           Servant.Server (errBody, err500)



loadPromotions :: C.OrderPId -> Flow [D.Promotion]
loadPromotions orderPId = do
  proms  <- withDB eulerDB $ do
    let predicate DB.Promotion{orderReferenceId} =
          orderReferenceId ==. B.just_ (B.val_ orderPId)
    findRows
      $ B.select
      $ B.filter_ predicate
      $ B.all_ (DB.promotion DB.eulerDBSchema)
  case traverse transformPromotions proms of
    Success pr -> pure pr
    Failure e -> do
      logError "Incorrect Promotion(s) in DB"
        $  "orderPId: " <> show orderPId
        <> " error: " <> show e
      throwException internalError


decryptActivePromotion :: C.OrderId -> [D.Promotion] -> Flow (Maybe C.Promotion')
decryptActivePromotion _ [] = pure Nothing
decryptActivePromotion orderId promotions = do
  let mPromotion = find (\promotion -> (promotion ^. _status) == "ACTIVE" ) promotions
  traverse (decryptPromotionRules orderId) mPromotion


transformPromotions :: DB.Promotion -> V D.Promotion
transformPromotions r = D.Promotion
  <$> (D.PromotionPId <$> withField @"id" r notNegative)
  <*> withField @"dateCreated" r pure
  <*> (C.mkMoney <$> withField @"discountAmount" r amountValidators)
  <*> withField @"lastModified" r pure
  <*> withField @"orderId" r (insideJust notNegative)
  <*> withField @"rules" r textNotEmpty
  <*> withField @"status" r textNotEmpty
  <*> withField @"orderReferenceId" r (insideJust notNegative)



decryptPromotionRules :: Text -> D.Promotion -> Flow C.Promotion'
decryptPromotionRules ordId promotions = do

  -- ecTempCardCred partly implemented. See Euler.Config.Config for details.
  keyForDecryption <- Config.ecTempCardCred

  let rulesDecoded = B64.decode $ T.encodeUtf8 $ promotions ^. _rules
  rulesjson <- case rulesDecoded of
    Right result -> pure $ E.decryptEcb keyForDecryption result
    Left err -> throwException err500 {errBody = LC.pack err}

  let rules = getRulesFromString rulesjson
  let rValue = getMaskedAccNo (rules ^. _value)

  pure $ C.Promotion'
          { id = Just $ show $ D.promotionPId $ promotions ^. _id
          , order_id = Just ordId
          , rules = Just [rules & _value .~ rValue]
          , created = Just $ show (promotions ^. _dateCreated)
          , discount_amount = Just $ C.fromMoney $ promotions ^. _discountAmount
          , status = Just $ promotions ^. _status
          }

getRulesFromString :: Either E.EncryptionError ByteString -> C.Rules
getRulesFromString bs =
  case A.decode . BSL.fromStrict <$> bs of
    Right (Just (rules :: [C.Rules])) -> case find (\r -> r ^. _dimension == "card_number") rules of
      Just resp  -> resp
      Nothing -> C.Rules { dimension = "", value = ""}
    _ -> C.Rules { dimension = "", value = ""}

getMaskedAccNo :: Text -> Text
getMaskedAccNo txt = T.append (T.map (const 'X') $ T.dropEnd 4 txt) (T.takeEnd 4 txt)
