{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.Core.Types.KVDB where

import           EulerHS.Prelude
import           EulerHS.Core.Types.Serializable
import qualified Database.Redis  as RD
import qualified Data.Aeson      as A
import qualified GHC.Generics    as G

-- Key-value database connection
data KVDBConn
  = Mocked KVDBMockedValues
  -- ^ Mocked connection.
  | Redis RD.Connection
  -- ^ Real connection.

data KVDBMockedValues' = KVDBMockedValues'
  { kvdbSet    :: [KVDBStatus]
  , kvdbGet    :: [(Maybe ByteString)]
  , kvdbExists :: [Bool]
  , kvdbDel    :: [Integer]
  , kvdbExpire :: [Bool]
  , kvdbIncr   :: [Integer]
  , kvdbHSet   :: [Bool]
  , kvdbHGet   :: [(Maybe ByteString)]
  , kvdbTX     :: [TxResult Any]
  } deriving (Generic, Typeable)


type KVDBMockedValues = MVar (KVDBMockedValues')

----------------------------------------------------------------------

data KVDBReplyF bs
  = SingleLine bs
  | Err bs
  | Integer Integer
  | Bulk (Maybe bs)
  | MultiBulk (Maybe [KVDBReplyF bs])
  | ExceptionMessage String
  deriving (Eq, Show, Generic, Functor)

instance ToJSON   (KVDBReplyF ByteStringS)
instance FromJSON (KVDBReplyF ByteStringS)

type KVDBReply = KVDBReplyF ByteString

instance ToJSON KVDBReply where
  toJSON = toJSON . fromKVDBReply

instance FromJSON KVDBReply where
  parseJSON = fmap toKVDBReply . parseJSON

fromKVDBReply :: KVDBReply -> KVDBReplyF ByteStringS
fromKVDBReply = fmap fromByteString

toKVDBReply :: KVDBReplyF ByteStringS -> KVDBReply
toKVDBReply = fmap toByteString

----------------------------------------------------------------------

data KVDBStatusF bs
  = Ok
  | Pong
  | Status bs
  deriving (Eq, Show, Generic, Functor)

instance ToJSON   (KVDBStatusF ByteStringS)
instance FromJSON (KVDBStatusF ByteStringS)

type KVDBStatus = KVDBStatusF ByteString

instance ToJSON KVDBStatus where
  toJSON = toJSON . fromStatus

instance FromJSON KVDBStatus where
  parseJSON = fmap toStatus . parseJSON

fromStatus :: KVDBStatus -> KVDBStatusF ByteStringS
fromStatus Ok          = Ok
fromStatus Pong        = Pong
fromStatus (Status bs) = Status $ fromByteString bs

toStatus :: KVDBStatusF ByteStringS -> KVDBStatus
toStatus Ok          = Ok
toStatus Pong        = Pong
toStatus (Status bs) = Status $ toByteString bs

fromRdStatus :: RD.Status -> KVDBStatus
fromRdStatus RD.Ok          = Ok
fromRdStatus RD.Pong        = Pong
fromRdStatus (RD.Status bs) = Status $ bs

----------------------------------------------------------------------

data TxResult a
  = TxSuccess a
  | TxAborted
  | TxError String
  deriving (Eq, Show, Functor, G.Generic1, A.ToJSON1, A.FromJSON1)

fromRdTxResult :: RD.TxResult a -> TxResult a
fromRdTxResult (RD.TxSuccess a) = TxSuccess a
fromRdTxResult RD.TxAborted     = TxAborted
fromRdTxResult (RD.TxError s)   = TxError s

----------------------------------------------------------------------

type KVDBAnswer = Either KVDBReply

hedisReplyToKVDBReply :: RD.Reply -> KVDBReply
hedisReplyToKVDBReply (RD.SingleLine s) = SingleLine s
hedisReplyToKVDBReply (RD.Error s) = Err s
hedisReplyToKVDBReply (RD.Integer s) = Integer s
hedisReplyToKVDBReply (RD.Bulk s) = Bulk s
hedisReplyToKVDBReply (RD.MultiBulk s) = MultiBulk (map (hedisReplyToKVDBReply <$>) s)


exceptionToKVDBReply :: Exception e => e -> KVDBReply
exceptionToKVDBReply e = ExceptionMessage $ displayException e
