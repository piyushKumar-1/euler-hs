{-# LANGUAGE DeriveAnyClass #-}

module Euler.Config.Creditails where

import           EulerHS.Language
import           EulerHS.Prelude hiding (getOption)
import           EulerHS.Types

import qualified Euler.Config.Config as C
import           Euler.Config.EnvVars
import qualified Euler.Encryption as E

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


data ECTempCardCred = ECTempCardCred
  deriving (Generic, Typeable, Show, Eq, ToJSON, FromJSON)

data ECMandateParamsCred = ECMandateParamsCred
  deriving (Generic, Typeable, Show, Eq, ToJSON, FromJSON)

data RazorpayClientSecret = RazorpayClientSecret
  deriving (Generic, Typeable, Show, Eq, ToJSON, FromJSON)

instance OptionEntity ECTempCardCred Text
instance OptionEntity ECMandateParamsCred Text
instance OptionEntity RazorpayClientSecret Text


ecTempCardCred :: Flow ByteString
ecTempCardCred = do
  case C.getEnv of
    C.DEV -> pure devKey
    _ -> do
      mKey <- getOption ECTempCardCred
      case mKey of
        Just key -> pure $ B64.decodeLenient $ T.encodeUtf8 key
        Nothing -> do
          key <- E.decryptKMS $ B64.decodeLenient $ BC.pack getECTempCardEncryptedKey
          setOption ECTempCardCred $ T.decodeUtf8 $ B64.encode key
          pure key
  where
    devKey = fst $ B16.decode "bd3222130d110b9684dac9cc0903ce111b25e97ae93ddc2925f89c4ed6e41bab"

ecMandateParamsCred :: Flow ByteString
ecMandateParamsCred = do
  case C.getEnv of
    C.DEV -> pure devKey
    _ -> do
      mbKey <- getOption ECMandateParamsCred
      case mbKey of
        Just key -> pure $ B64.decodeLenient $ T.encodeUtf8 key
        Nothing -> do
          key <- E.decryptKMS $ B64.decodeLenient $ BC.pack getECMandateParamsEncryptedKey
          setOption ECMandateParamsCred $ T.decodeUtf8 $ B64.encode key
          pure key
  where
    devKey = fst $ B16.decode "a231ccb856c125486f1891bc5646f30949efcd6d1c14b6acc439ef928b133c32"


razorpayClientSecret :: String -> Flow ByteString
razorpayClientSecret = E.decryptKMS . B64.decodeLenient . BC.pack