{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Euler.Product.Domain.PaymentMethod.NB where

import EulerHS.Prelude
import Data.Data

data NBPaymentMethod
  = NB_AXIS
  | NB_BOI
  | NB_BOM
  | NB_CBI
  | NB_CORP
  | NB_DCB
  | NB_FED
  | NB_HDFC
  | NB_ICICI
  | NB_IDBI
  | NB_INDB
  | NB_INDUS
  | NB_IOB
  | NB_JNK
  | NB_KARN
  | NB_KVB
  | NB_SBBJ
  | NB_SBH
  | NB_SBI
  | NB_SBM
  | NB_SBT
  | NB_SOIB
  | NB_UBI
  | NB_UNIB
  | NB_VJYB
  | NB_YESB
  | NB_CUB
  | NB_CANR
  | NB_SBP
  | NB_CITI
  | NB_DEUT
  | NB_KOTAK
  | NB_DLS
  | NB_ING
  | NB_ANDHRA
  | NB_PNBCORP
  | NB_PNB
  | NB_BOB
  | NB_CSB
  | NB_OBC
  | NB_SCB
  | NB_TMB
  | NB_SARASB
  | NB_SYNB
  | NB_UCOB
  | NB_BOBCORP
  | NB_ALLB
  | NB_BBKM
  | NB_JSB
  | NB_LVBCORP
  | NB_LVB
  | NB_NKGSB
  | NB_PMCB
  | NB_PNJSB
  | NB_RATN
  | NB_RBS
  | NB_SVCB
  | NB_TNSC
  | NB_DENA
  | NB_COSMOS
  | NB_DBS
  | NB_DCBB
  | NB_SVC
  | NB_BHARAT
  | NB_KVBCORP
  | NB_UBICORP
  | NB_IDFC
  | NB_NAIB
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Data,Typeable)
