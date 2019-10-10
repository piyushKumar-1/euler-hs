module EulerHS.Core.Types.KVDB where

import qualified Database.Redis  as RD (Reply (..))
import           EulerHS.Prelude

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
