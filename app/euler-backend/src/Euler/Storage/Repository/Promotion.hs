{-# LANGUAGE NamedFieldPuns  #-}

module Euler.Storage.Repository.Promotion where

import           EulerHS.Prelude hiding (id, Key)
import           EulerHS.Prelude as P

import           EulerHS.Extra.Validation
import           EulerHS.Language

import qualified Euler.Encryption as E
import           Euler.Lens

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


{-PS
decryptPromotionRules :: forall st e r rt. Newtype st (TState e) => String -> Promotions -> BackendFlow st rt Promotion'
decryptPromotionRules ordId promotions = do
  let promotion = unwrap promotions
  keyForDecryption <- doAffRR' "ecTempCardCred" (ecTempCardCred)
  resp <- case (decryptAESRaw "aes-256-ecb" (base64ToHex (promotion.rules)) keyForDecryption Nothing) of
    Right result -> pure result
    Left err -> throwErr $ show err
  rules  <- pure $ getRulesFromString resp
  rValue <- pure $ getMaskedAccNo (rules ^. _value)
  pure $ Promotion'
          { id : just $ show (promotion.id)
          , order_id : just ordId
          , rules : just $ singleton (rules # _value .~ rValue)
          , created : just $ _dateToString (promotion.dateCreated)
          , discount_amount : just $ promotion.discountAmount
          , status :just $ promotion.status
          }
-}

-- EHS: port
decryptPromotionRules :: Text -> D.Promotion -> Flow C.Promotion'
-- decryptPromotionRules ordId promotions = pure C.defaultPromotion' -- EHS: TODO port
decryptPromotionRules ordId promotions = do
  keyForDecryption <- pure undefined :: Flow (E.Key E.AES256 ByteString) -- TODO: runIO ecTempCardCred

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



-- ecTempCardCred:: forall e. Aff e String
-- ecTempCardCred = case getEnv of
--   PROD -> decrypt getECTempCardEncryptedKey
--   UAT -> decrypt getECTempCardEncryptedKey
--   INTEG -> decrypt getECTempCardEncryptedKey
--   _ -> pure $ "bd3222130d110b9684dac9cc0903ce111b25e97ae93ddc2925f89c4ed6e41bab"

-- decrypt :: forall e. String -> Aff e String
-- decrypt = toAff <<< decodeKMS

-- exports.getECTempCardEncryptedKey = process.env.EC_TEMP_CARD_AES_KEY

-- exports.decodeKMS = function(val) {
--   AWS.config.update(config.aws);
--   var kms = new AWS.KMS();

--   var encryptedParams = {
--     CiphertextBlob: Buffer(val, "base64")
--   };

--   return kms
--     .decrypt(encryptedParams)
--     .promise()
--     .then(function(data) {
--       var decryptedString = data.Plaintext.toString("ascii");
--       return Promise.resolve(decryptedString);
--     })
--     .catch(function(err) {
--       return Promise.reject(new Error(err));
--     });
-- };

getRulesFromString :: Either E.EncryptionError ByteString -> C.Rules
getRulesFromString bs =
  case A.decode . BSL.fromStrict <$> bs of
    Right (Just (rules :: [C.Rules])) -> case find (\r -> r ^. _dimension == "card_number") rules of
      Just resp  -> resp
      Nothing -> C.Rules { dimension = "", value = ""}
    _ -> C.Rules { dimension = "", value = ""}

-- exports["getMaskedAccNo"] = function(str) {
--   return str.replace(/.(?=.{4})/g, 'X');
-- }

getMaskedAccNo :: Text -> Text
getMaskedAccNo txt = T.append (T.map (const 'X') $ T.dropEnd 4 txt) (T.takeEnd 4 txt)
