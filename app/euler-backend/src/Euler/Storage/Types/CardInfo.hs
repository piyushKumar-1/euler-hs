{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Euler.Storage.Types.CardInfo
  ( CardInfoT(..)
  , CardInfo
  , Id
  , cardInfoEMod
  ) where

import           EulerHS.Prelude hiding (id)
import           Data.Time
import           Euler.Common.Types.DefaultDate
import qualified Database.Beam as B


data CardInfoT f = CardInfo
  { cardIsin :: B.C f Text
  , cardIssuerBankName :: B.C f (Maybe Text)
  , cardSwitchProvider :: B.C f Text
  , cardType :: B.C f (Maybe Text)
  , cardSubType :: B.C f (Maybe Text)
  , cardIssuerCountry :: B.C f (Maybe Text)
  , hit :: B.C f (Maybe Int)
  , hdfcIvrEnabled :: B.C f (Maybe Bool)
  }
  deriving (Generic, B.Beamable)

instance B.Table CardInfoT where
  data PrimaryKey CardInfoT f =
    Id (B.C f Text) deriving (Generic, B.Beamable)
  primaryKey = Id . cardIsin

type CardInfo = CardInfoT Identity
type Id = B.PrimaryKey CardInfoT Identity

deriving instance Show CardInfo
deriving instance Eq CardInfo
deriving instance ToJSON CardInfo
deriving instance FromJSON CardInfo
deriving instance Read CardInfo
deriving instance Ord CardInfo

cardInfoEMod :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity CardInfoT)
cardInfoEMod = B.modifyTableFields
  B.tableModification
    { cardIsin = B.fieldNamed "cardIsin"
    , cardIssuerBankName = B.fieldNamed "cardIssuerBankName"
    , cardSwitchProvider = B.fieldNamed "cardSwitchProvider"
    , cardType = B.fieldNamed "cardType"
    , cardSubType = B.fieldNamed "cardSubType"
    , cardIssuerCountry = B.fieldNamed "cardIssuerCountry"
    , hit = B.fieldNamed "hit"
    , hdfcIvrEnabled = B.fieldNamed "hdfcIvrEnabled"
    }


