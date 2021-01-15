module MaskingSpec (spec) where

import EulerHS.Prelude hiding (readFile)
import qualified Data.ByteString.Lazy as LBS
import qualified EulerHS.Types as CType
import qualified Data.HashSet as HashSet
import           Test.Hspec

spec :: Spec
spec =
  describe "Outgoing API call log masking" $ do
    it "Should Mask All the blackListed Keys" $ do
      rawRequest <-
        LBS.readFile "./fixtures/masking/maskingData.json"
      let maskText = "$$$"
      let mbMaskConfig = Just $ makeLogMaskingConfig CType.BlackListKey ["id", "url1","a"] maskText
      let maskedValue = CType.parseRequestResponseBody (CType.shouldMaskKey mbMaskConfig) maskText (LBS.toStrict rawRequest)
      maskedValue `shouldBe` expectedOutput
    it "Should Mask All the blackListed Keys" $ do
      rawRequest <-
        LBS.readFile "./fixtures/masking/maskingData.json"
      let maskText = "$**$"
      let mbMaskConfig = Just $ makeLogMaskingConfig CType.WhiteListKey ["id", "url1","a"] maskText
      let maskedValue = CType.parseRequestResponseBody (CType.shouldMaskKey mbMaskConfig) maskText (LBS.toStrict rawRequest)
      maskedValue `shouldBe` expectedOutput'
    it "Should Not Mask Any Keys" $ do
      rawRequest <-
        LBS.readFile "./fixtures/masking/maskingData.json"
      let maskText = "$**$"
      let mbMaskConfig = Nothing
      let maskedValue = CType.parseRequestResponseBody (CType.shouldMaskKey mbMaskConfig) maskText (LBS.toStrict rawRequest)
      maskedValue `shouldBe` expectedOutput''
  
expectedOutput :: Text
expectedOutput = "{\"status\":\"INIT\",\"txnId\":\"paypal-tatapay_740-1\",\"txnDetailId\":\"2148428442\",\"responseAttempted\":{\"lastUpdated\":\"2020-09-25T05:58:13Z\",\"gatewayAuthReqParams\":\"{\\\"euler-api-gateway\\\":\\\"fehfioe\\\"}\",\"dateCreated\":\"2020-09-25T05:58:13Z\",\"challengesAttempted\":0,\"canAcceptResponse\":true,\"id\":\"$$$\"},\"version\":0,\"url1\":\"$$$\",\"type\":\"VBV\"}"

expectedOutput' :: Text
expectedOutput' = "{\"status\":\"$**$\",\"txnId\":\"$**$\",\"txnDetailId\":\"$**$\",\"responseAttempted\":\"$**$\",\"version\":\"$**$\",\"url1\":[{\"a\":\"b\"},\"wefojoefwj\"],\"type\":\"$**$\"}"

expectedOutput'' :: Text
expectedOutput'' = "{\"status\":\"INIT\",\"txnId\":\"paypal-tatapay_740-1\",\"txnDetailId\":\"2148428442\",\"responseAttempted\":{\"lastUpdated\":\"2020-09-25T05:58:13Z\",\"gatewayAuthReqParams\":\"{\\\"euler-api-gateway\\\":\\\"fehfioe\\\"}\",\"dateCreated\":\"2020-09-25T05:58:13Z\",\"challengesAttempted\":0,\"canAcceptResponse\":true,\"id\":\"2148361678\"},\"version\":0,\"url1\":[{\"a\":\"b\"},\"wefojoefwj\"],\"type\":\"VBV\"}"

makeLogMaskingConfig :: CType.MaskKeyType -> [Text] -> Text ->  CType.LogMaskingConfig
makeLogMaskingConfig keyType keyList maskText =
  CType.LogMaskingConfig
    { _maskKeys =  HashSet.fromList keyList
    , _maskText =  Just maskText
    , _keyType  =  keyType
    }