{-# LANGUAGE DeriveDataTypeable #-}
module EulerHS.Core.Types.KVDB where

import qualified Database.Redis  as RD (Reply (..), Connection, Status, TxResult)
import           EulerHS.Prelude

data KVDBConn
  = Mocked KVDBMockedValues
  | Redis RD.Connection

data KVDBMockedValues' = KVDBMockedValues'
  { kvdbSet    :: [ RD.Status]
  , kvdbGet    :: [ (Maybe ByteString)]
  , kvdbExists :: [ Bool]
  , kvdbDel    :: [ Integer]
  , kvdbExpire :: [ Bool]
  , kvdbIncr   :: [ Integer]
  , kvdbHSet   :: [ Bool]
  , kvdbHGet   :: [ (Maybe ByteString)]
  , kvdbTX     :: [RD.TxResult Any]
  } deriving (Generic, Typeable)


type KVDBMockedValues = MVar (KVDBMockedValues' )


data KVDBReply = SingleLine ByteString
               | Err ByteString
               | Integer Integer
               | Bulk (Maybe ByteString)
               | MultiBulk (Maybe [KVDBReply])
               | ExceptionMessage String
         deriving (Eq, Show, Generic)

type KVDBAnswer = Either KVDBReply

hedisReplyToKVDBReply :: RD.Reply -> KVDBReply
hedisReplyToKVDBReply (RD.SingleLine s) = SingleLine s
hedisReplyToKVDBReply (RD.Error s) = Err s
hedisReplyToKVDBReply (RD.Integer s) = Integer s
hedisReplyToKVDBReply (RD.Bulk s) = Bulk s
hedisReplyToKVDBReply (RD.MultiBulk s) = MultiBulk (map (hedisReplyToKVDBReply <$>) s)


exceptionToKVDBReply :: Exception e => e -> KVDBReply
exceptionToKVDBReply e = ExceptionMessage $ displayException e
