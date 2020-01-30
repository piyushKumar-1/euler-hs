{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.SecondFactorResponse
  ( SecondFactorResponseT (..)
  , SecondFactorResponse
  , Id
  , secondFactorResponseEMod
  ) where

import EulerHS.Prelude hiding (id)
import Data.Time
import qualified Database.Beam as B

data SecondFactorResponseT f = SecondFactorResponse
    { id :: B.C f (Maybe Text)
    , version :: B.C f Int
    , cavv :: B.C f (Maybe Text)
    , currency :: B.C f  (Maybe Text)
    , eci :: B.C f Text
    , mpiErrorCode :: B.C f (Maybe Text)
    , purchaseAmount :: B.C f (Maybe Double)
    , responseId :: B.C f (Maybe Text)
    , shoppingContext :: B.C f Text
    , status :: B.C f Text
    , xid :: B.C f Text
    , dateCreated :: B.C f LocalTime
    , secondFactorId :: B.C f (Maybe Text)
    , gatewayAuthResData :: B.C f (Maybe Text)
    }
    deriving (Generic, B.Beamable)

instance B.Table SecondFactorResponseT where
  data PrimaryKey SecondFactorResponseT f =
    Id (B.C f (Maybe Text)) deriving (Generic, B.Beamable)
  primaryKey = Id . id

type SecondFactorResponse = SecondFactorResponseT Identity
type Id = B.PrimaryKey SecondFactorResponseT Identity

deriving instance Show SecondFactorResponse
deriving instance Eq SecondFactorResponse
deriving instance ToJSON SecondFactorResponse
deriving instance FromJSON SecondFactorResponse
deriving instance Read SecondFactorResponse
deriving instance Ord SecondFactorResponse

secondFactorResponseEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity SecondFactorResponseT)
secondFactorResponseEMod = B.modifyTableFields
  B.tableModification
    { id = B.fieldNamed "id"
    , version = B.fieldNamed "version"
    , cavv = B.fieldNamed "cavv"
    , currency = B.fieldNamed "currency"
    , eci = B.fieldNamed "eci"
    , mpiErrorCode = B.fieldNamed "mpi_error_code"
    , purchaseAmount = B.fieldNamed "purchase_amount"
    , responseId = B.fieldNamed "response_id"
    , shoppingContext = B.fieldNamed "shopping_context"
    , status = B.fieldNamed "status"
    , xid = B.fieldNamed "xid"
    , dateCreated = B.fieldNamed "date_created"
    , secondFactorId = B.fieldNamed "second_factor_id"
    , gatewayAuthResData = B.fieldNamed "gateway_auth_res_data"
    }
