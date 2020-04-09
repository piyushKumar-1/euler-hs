{-# LANGUAGE DeriveAnyClass #-}

module Euler.Config.Creditails where

import           EulerHS.Language
import           EulerHS.Prelude
import           EulerHS.Types

import qualified Euler.Config.Config as C
import           Euler.Config.EnvVars
import qualified Euler.Encryption as E

import qualified Data.ByteString.Base16 as B16
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
  key <- E.decryptKMS $ T.encodeUtf8 $ T.pack getECTempCardEncryptedKey
  setOption ECTempCardCred $ T.decodeUtf8 key
  case C.getEnv of
    C.PROD  -> pure key
    C.UAT   -> pure key
    C.INTEG -> pure key
    _       -> pure devKey
  where
    devKey = fst $ B16.decode "bd3222130d110b9684dac9cc0903ce111b25e97ae93ddc2925f89c4ed6e41bab"


ecMandateParamsCred :: Flow ByteString
ecMandateParamsCred = do
  key <- E.decryptKMS $ T.encodeUtf8 $ T.pack getECMandateParamsEncryptedKey
  setOption ECMandateParamsCred $ T.decodeUtf8 key
  case C.getEnv of
    C.PROD  -> pure key
    C.UAT   -> pure key
    C.INTEG -> pure key
    _       -> pure devKey
  where
    devKey = fst $ B16.decode "a231ccb856c125486f1891bc5646f30949efcd6d1c14b6acc439ef928b133c32"


razorpayClientSecret :: String -> Flow ByteString
razorpayClientSecret = E.decryptKMS . T.encodeUtf8 . T.pack

getMySQLCfg :: Flow MySQLConfig
getMySQLCfg = case C.getEnv of
  C.DEV -> pure MySQLConfig
          { connectHost     = devMysqlConnectHost
          , connectPort     = devMysqlConnectPort
          , connectUser     = devMysqlConnectUser
          , connectPassword = devMysqlConnectPassword
          , connectDatabase = devMysqlConnectDatabase
          , connectOptions  = [CharsetName "utf8"]
          , connectPath     = devMysqlConnectPath
          , connectSSL      = Nothing
          }
  _ -> do
    decodedPassword <- E.decryptKMS $ T.encodeUtf8 $ T.pack getEcDbPass
    pure MySQLConfig
          { connectHost     = getEcDbHost
          , connectPort     = getEcDbPort
          , connectUser     = getEcDbUserName
          , connectPassword = T.unpack $ T.decodeUtf8 decodedPassword
          , connectDatabase = getEcDbName
          , connectOptions  = [CharsetName "utf8"]
          , connectPath     = ""
          , connectSSL      = Nothing
          }

mysqlDBC = do
  mySqlConfig <- getMySQLCfg
  case C.getEnv of
    C.DEV -> pure $ mkMySQLPoolConfig (T.pack devMysqlConnectionName) mySqlConfig C.mySqlPoolConfig
    _     -> pure $ mkMySQLPoolConfig (T.pack devMysqlConnectionName) mySqlConfig C.mySqlPoolConfig
