{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Euler.Common.Types.Gateway where

import EulerHS.Prelude

import qualified Data.Map as Map
import qualified Data.Text as T

import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Sqlite
import Database.Beam.MySQL

type GatewayId = Int

-- from src/Types/Storage/EC/TxnDetail/Types.purs
data Gateway =
    AXIS
  | HDFC
  | ICICI
  | CITI
  | AMEX
  | CYBERSOURCE
  | IPG
  | MIGS
  | KOTAK
  | EBS
  | PAYU
  | CCAVENUE
  | CITRUS
  | ATOM
  | CCAVENUE_V2
  | TPSL
  | PAYTM
  | PAYTM_V2
  | PAYPAL
  | HDFC_EBS_VAS
  | PAYLATER
  | RAZORPAY
  | FSS_ATM_PIN
  | EBS_V3
  | ZAAKPAY
  | BILLDESK
  | SODEXO
  | BLAZEPAY
  | FSS_ATM_PIN_V2
  | MOBIKWIK
  | OLAMONEY
  | FREECHARGE
  | MPESA
  | SBIBUDDY
  | JIOMONEY
  | AIRTELMONEY
  | AMAZONPAY
  | PHONEPE
  | STRIPE
  | DUMMY
  | HDFC_IVR
  | ZESTMONEY
  | EPAYLATER
  | AXISNB
  | ICICINB
  | TPSL_SI
  | AXIS_UPI
  | HDFC_UPI
  | INDUS_UPI
  | KOTAK_UPI
  | SBI_UPI
  | ICICI_UPI
  | VIJAYA_UPI
  | HSBC_UPI
  | YESBANK_UPI
  | PAYTM_UPI
  | LINEPAY
  | OLAPOSTPAID
  | SIMPL
  | GOOGLEPAY
  | GOCASHFREE
  | FSSPAY
  | CASH
  | DEFAULT
  deriving (Show, Read, Eq, Ord, Generic, Enum, Bounded, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Gateway where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres Gateway where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow Sqlite Gateway where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow MySQL Gateway where
  fromBackendRow = read . T.unpack <$> fromBackendRow

-- EHS: these functions and constants should not be here.
-- Preferably, this should be set up as a config.
gatewayList :: [(Gateway, GatewayId)]
gatewayList =
  [ (AXIS, 1)
  , (HDFC, 2)
  , (ICICI, 3)
  , (CITI, 4)
  , (AMEX, 5)
  , (CYBERSOURCE, 6)
  , (IPG, 7)
  , (MIGS, 8)
  , (KOTAK, 9)
  , (EBS, 11)
  , (PAYU, 12)
  , (CCAVENUE, 13)
  , (CITRUS, 14)
  , (ATOM, 15)
  , (CCAVENUE_V2, 16)
  , (TPSL, 17)
  , (PAYTM, 18)
  , (PAYTM_V2, 19)
  , (PAYPAL, 20)
  , (HDFC_EBS_VAS, 21)
  , (PAYLATER, 22)
  , (RAZORPAY, 23)
  , (FSS_ATM_PIN, 24)
  , (EBS_V3, 25)
  , (ZAAKPAY, 26)
  , (BILLDESK, 27)
  , (SODEXO, 28)
  , (BLAZEPAY, 29)
  , (FSS_ATM_PIN_V2, 30)
  , (MOBIKWIK, 31)
  , (OLAMONEY, 32)
  , (FREECHARGE, 33)
  , (MPESA, 34)
  , (SBIBUDDY, 35)
  , (JIOMONEY, 36)
  , (AIRTELMONEY, 37)
  , (AMAZONPAY, 38)
  , (PHONEPE, 39)
  , (OLAPOSTPAID, 40)
  , (SIMPL, 41)
  , (GOOGLEPAY, 42)
  , (STRIPE, 50)
  , (GOCASHFREE, 70)
  , (FSSPAY, 73)
  , (DUMMY, 100)
  , (HDFC_IVR, 201)
  , (ZESTMONEY, 250)
  , (EPAYLATER, 251)
  , (AXISNB, 300)
  , (ICICINB, 302)
  , (TPSL_SI, 400)
  , (AXIS_UPI, 500)
  , (HDFC_UPI, 501)
  , (INDUS_UPI, 502)
  , (KOTAK_UPI, 503)
  , (SBI_UPI, 504)
  , (ICICI_UPI, 505)
  , (VIJAYA_UPI, 506)
  , (HSBC_UPI, 507)
  , (YESBANK_UPI, 508)
  , (PAYTM_UPI, 509)
  , (LINEPAY, 600)
  , (CASH, 700)
  ]

gatewayMap :: Map.Map Gateway GatewayId
gatewayMap = Map.fromList gatewayList

gatewayRMap :: Map.Map GatewayId Gateway
gatewayRMap = Map.fromList $ map swap gatewayList

lookupGatewayName :: GatewayId -> Maybe Text
lookupGatewayName k = Map.lookup k gatewayRMap >>= Just . show

gatewayIdFromGateway :: Gateway -> Maybe GatewayId
gatewayIdFromGateway k = Map.lookup k gatewayMap
