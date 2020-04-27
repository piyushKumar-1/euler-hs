{-# LANGUAGE DeriveAnyClass #-}

module Euler.API.Authentication where

import EulerHS.Prelude

import qualified EulerHS.Language                     as L
import           WebService.Language

import           Data.Generics.Product.Fields
import qualified Data.ByteString.Base64               as B64
import qualified Database.Beam                        as B
import           Database.Beam ((==.), (&&.))
import qualified Data.Aeson                           as A
import           Servant

import           Euler.Storage.Repository.EulerDB
import           Euler.Storage.Types.MerchantAccount
import           Euler.Storage.Types.MerchantKey
import           Euler.Storage.Types.EulerDB
import           Euler.Encryption


-- We can not use ByteStrings directly as we need To/FromJSON instances
data Signed a = Signed
  { signature_payload :: String -- ^ UTF8-String. JSON-encoded a. a should contain merchant_id field.
  , signature         :: String -- ^ UTF8-String. BASE64-encoded signature (PKCS1, AES256)
  , merchant_key_id   :: Int
  } deriving (Generic, ToJSON, FromJSON)

authenticateUsingRSAEncryption'
    :: (FromJSON a, HasField' "merchant_id" a Text, Generic a)
    => (MerchantAccount -> a -> L.Flow b)
    -> Signed a
    -> L.Flow b
authenticateUsingRSAEncryption' next Signed
  { signature_payload
  , signature = b64signature
  , merchant_key_id
  } = do

    decodedSignaturePayload <- case A.decodeStrict $ fromString signature_payload of
      Just ds  -> pure ds
      Nothing  -> L.throwException err401 {errBody = "Malformed signature_payload. Can not parse expected structure from JSON."}

    signature <- case B64.decode $ fromString b64signature of
      Right sig -> pure sig
      Left _    -> do
        logInfoT "authenticateUsingRSAEncryption" "Can not decode b64-encoded signature"
        L.throwException err401 {errBody = "Malformed signature. Expected Base64 encoded signature."}

    let merchantId' = getField @"merchant_id" decodedSignaturePayload

    merchantAccount <- do
      res <- withEulerDB $ do
        let predicate MerchantAccount {merchantId} =
              merchantId ==. (B.val_ $ Just merchantId')

        L.findRow
          $ B.select
          $ B.limit_ 1
          $ B.filter_ predicate
          $ B.all_ (merchant_account eulerDBSchema)

      case res of
        Nothing -> L.throwException err401
        Just ma -> pure ma

    merchantKey <- do
      res <- withEulerDB $ do
        let maid = Euler.Storage.Types.MerchantAccount.id
        let predicate MerchantKey {merchantAccountId, scope, status, id = mkid} =
              merchantAccountId ==. (B.val_ $ maid merchantAccount)     &&.
              scope             ==. (B.val_ $ Just "CLIENT_ENCRYPTION") &&.
              status            ==. (B.val_ $ Just "ACTIVE")            &&.
              mkid              ==. (B.val_ $ Just merchant_key_id)

        L.findRow
          $ B.select
          $ B.limit_ 1
          $ B.filter_ predicate
          $ B.all_ (merchant_key eulerDBSchema)

      case res of
        Nothing -> L.throwException err401
        Just mk -> pure mk

    -- Should apiKey be mandatory field?
    merchantPublicKey <- case (apiKey :: MerchantKey -> Maybe Text) merchantKey of
      Nothing -> do
        logInfoT "authenticateUsingRSAEncryption" "Merchant Key not found"
        L.throwException err401

      Just pubKey -> do
        logInfoT "authenticateUsingRSAEncryption" "Merchant Key found"
        pure pubKey

    merchantRSAPublicKey <- case parseRSAPubKey $ encodeUtf8 merchantPublicKey of
      Right key -> pure key
      Left _    -> do
        logInfoT "authenticateUsingRSAEncryption" "Can not parse merchant public key"
        L.throwException err401

    case verifyRSASignaturePKCS15 (fromString signature_payload) signature merchantRSAPublicKey of
      True  -> do
        logInfoT "authenticateUsingRSAEncryption" "RSA signature verfifed"
        next merchantAccount decodedSignaturePayload

      False -> do
        logInfoT "authenticateUsingRSAEncryption" "RSA signature not verfifed"
        L.throwException err401

authenticateUsingRSAEncryption
  :: (FromJSON a, HasField' "merchant_id" a Text, Generic a)
  => (MerchantAccount -> a -> L.Flow b)
  -> Signed a
  -> L.Flow b
authenticateUsingRSAEncryption = authenticateUsingRSAEncryption'
