module KV.TestHelper where

import           EulerHS.Prelude

import           KV.FlowHelper
import           KV.TestSchema.ServiceConfiguration
import qualified EulerHS.Language as L
import           EulerHS.KVConnector.Types hiding(kvRedis)
import qualified Data.Aeson as A

getValueFromPrimaryKey :: (HasCallStack,FromJSON a) => Text -> L.Flow (Maybe a)
getValueFromPrimaryKey pKey = do
  res <- L.runKVDB kvRedis $ L.get $ encodeUtf8 pKey
  case res of
    Right (Just val) -> either (error . show) (pure . Just) (A.eitherDecodeStrict val)
    Right Nothing -> L.logInfoT "KEY_NOT_FOUND" pKey $> Nothing
    Left err -> error $ show err

getValueFromSecondaryKeys :: (HasCallStack,FromJSON a) => [Text] -> L.Flow [(Text,a)]
getValueFromSecondaryKeys secKeys = do
  eitherRefKeys <- L.runKVDB kvRedis $ L.lrange (encodeUtf8 $ partialHead secKeys) 0 (-1)
  case eitherRefKeys of
    Right refKeys -> fmap catMaybes $ sequence $ go <$> refKeys
    Left err -> error $ show err
  where
    go :: FromJSON a => ByteString -> L.Flow (Maybe (Text,a))
    go bkey = do
      let key = decodeUtf8 bkey
      mbRes <- getValueFromPrimaryKey key
      pure $ case mbRes of
                Just res -> Just (key,res)
                Nothing -> Nothing

deleteServiceConfigValueFromKV :: (KVConnector (table Identity)) => table Identity -> L.Flow ()
deleteServiceConfigValueFromKV sc = do
  let pKey = getLookupKeyByPKey sc
  let secKeys = getSecondaryLookupKeys sc
  void $ hush <$> (L.runKVDB kvRedis $ L.del ([encodeUtf8 pKey]))
  void $ hush <$> (L.runKVDB kvRedis $ L.del (encodeUtf8 <$> secKeys))

partialHead :: HasCallStack => [a] -> a
partialHead xs = 
  case listToMaybe xs of
    Just x -> x
    Nothing -> error "Found empty List" 