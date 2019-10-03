module EulerHS.Core.Types.KVDB where

import qualified Database.Redis  as RD (Reply (..))
import           EulerHS.Prelude

data KVDBReply = SingleLine ByteString
               | Err ByteString
               | Integer Integer
               | Bulk (Maybe ByteString)
               | MultiBulk (Maybe [KVDBReply])
               | ExceptionMessage ByteString
         deriving (Eq, Show, Generic)

hedisReplyToKVDBReplyMono :: RD.Reply -> KVDBReply
hedisReplyToKVDBReplyMono (RD.SingleLine s) = SingleLine s
hedisReplyToKVDBReplyMono (RD.Error s) = Err s
hedisReplyToKVDBReplyMono (RD.Integer s) = Integer s
hedisReplyToKVDBReplyMono (RD.Bulk s) = Bulk s
hedisReplyToKVDBReplyMono (RD.MultiBulk s) = MultiBulk (map (hedisReplyToKVDBReplyMono <$>) s)
