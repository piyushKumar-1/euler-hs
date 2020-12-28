module PSMessageFormatterSpec
  ( spec
  , benchmarking
  )
  where

import           EulerHS.Prelude
import           PSMimicLogger
import qualified EulerHS.Types as T

import qualified System.Logger as Log
import qualified System.Logger.Message as LogMsg
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Binary.Builder as BinaryBuilder
import qualified Data.ByteString.Builder as ByteStringBuilder

import           Test.Hspec
import           Criterion.Main (bgroup, bench, whnf, defaultMain)

dateFormat :: Log.DateFormat
dateFormat = "%0d-%0m-%Y %0H:%0M:%0S.000"

separator :: ByteString
separator = "\n"

logLvl :: Log.Level
logLvl = Log.Debug

appFramework :: String
appFramework = "euler-hs-application"

timestamp :: String
timestamp = "23-12-2020 03:52:16.000"

hostName :: String
hostName = "euler-api-order-8ba0ea3-5cb57644bc-mvrfs"

sourceCommit :: String
sourceCommit = "NA"

env :: String
env = "DEV"

appFrameworkT :: Text
appFrameworkT = "euler-hs-application"

timestampT :: Text
timestampT = "23-12-2020 03:52:16.000"

hostNameT :: Text
hostNameT = "euler-api-order-8ba0ea3-5cb57644bc-mvrfs"

sourceCommitT :: Text
sourceCommitT = "NA"

envT :: Text
envT = "DEV"

mbFlowGuid :: Maybe Text
mbFlowGuid  = Just "517bf125-6974-45c9-a268-96e5922a56ee"    -- x-request-id; ctx1

mbCtx2 :: Maybe Text
mbCtx2 = Just "some x-global-request-id"                -- x-global-request-id; ctx2

logMessage :: Text
logMessage  = "order doesn't exist, run create order"

logEntryNum :: Int
logEntryNum = 29660


pendingMsgSimple     = T.PendingMsg mbFlowGuid T.Debug "runOrderCreateUpdate" logMessage logEntryNum
pendingMsgWithQuotes = T.PendingMsg mbFlowGuid T.Debug "\"runOrderCreateUpdate\"" logMessage logEntryNum

test1Expected = "{\"timestamp\": \"23-12-2020 03:52:16.000\", \"app_framework\": \"euler-hs-application\", \"hostname\": \"euler-api-order-8ba0ea3-5cb57644bc-mvrfs\", \"source_commit\": \"NA\", \"level\": \"info\", \"txn_uuid\": \"null\", \"order_id\": \"null\", \"refund_id\": \"null\", \"refund_unique_request_id\": \"null\", \"merchant_id\": \"null\", \"message_number\": \"29660\", \"x-request-id\": \"517bf125-6974-45c9-a268-96e5922a56ee\", \"x-global-request-id\": \"some x-global-request-id\", \"message\": \"[Debug] <runOrderCreateUpdate> order doesn't exist, run create order\", \"message_type\": \"string\"}\n"

test2Expected = "{\"timestamp\": \"23-12-2020 03:52:16.000\", \"app_framework\": \"euler-hs-application\", \"hostname\": \"euler-api-order-8ba0ea3-5cb57644bc-mvrfs\", \"source_commit\": \"NA\", \"level\": \"info\", \"txn_uuid\": \"null\", \"order_id\": \"null\", \"refund_id\": \"null\", \"refund_unique_request_id\": \"null\", \"merchant_id\": \"null\", \"message_number\": \"29660\", \"x-request-id\": \"517bf125-6974-45c9-a268-96e5922a56ee\", \"x-global-request-id\": \"some x-global-request-id\", \"message\": \"[Debug] <\\\"runOrderCreateUpdate\\\"> order doesn't exist, run create order\", \"message_type\": \"string\"}\n"

defElements :: [LogMsg.Element]
defElements = [LogMsg.Bytes (LogMsg.bytes timestamp)]

renderer ::[LogMsg.Element] -> BinaryBuilder.Builder
renderer elements = originalPSMimicRenderer
  hostName env sourceCommit
  separator dateFormat logLvl (defElements ++ elements)

spec :: Spec
spec = describe "PS Mimic Logger tests" $ do
  describe "Original formatter and renderer logic" $ do

    it "Regular log message" $ do
      let bld = originalPSMimicFormatter mbCtx2 pendingMsgSimple
      case bld of
        T.MsgTransformer f -> do
          let resBS = Log.render renderer f
          resBS `shouldBe` test1Expected
        _ -> error "Original formatter is based on the Msg transforming function."

    it "Log message with tag having a quote" $ do
      let bld = originalPSMimicFormatter mbCtx2 pendingMsgWithQuotes
      case bld of
        T.MsgTransformer f -> do
          let resBS = Log.render renderer f
          resBS `shouldBe` test2Expected
        _ -> error "Original formatter is based on the Msg transforming function."


  describe "Custom formatter #1, fmt based, String" $ do

    it "Regular log message" $ do
      let bld = fmtPSMimicFormatterString
                  appFramework
                  timestamp
                  hostName env sourceCommit
                  mbCtx2 pendingMsgSimple
      case bld of
        T.SimpleString str -> LBS.pack str `shouldBe` test1Expected
        _ -> error "Custom formatting builder should return a SimpleString."

    it "Log message with tag having a quote" $ do
      let bld = fmtPSMimicFormatterString
                  appFramework
                  timestamp
                  hostName env sourceCommit
                  mbCtx2 pendingMsgWithQuotes
      case bld of
        T.SimpleString str -> LBS.pack str `shouldBe` test2Expected
        _ -> error "Custom formatting builder should return a SimpleString."

  describe "Custom formatter #2, fmt based, Text" $ do

    it "Regular log message" $ do
      let bld = fmtPSMimicFormatterText
                  appFrameworkT
                  timestampT
                  hostNameT envT sourceCommitT
                  mbCtx2 pendingMsgSimple
      case bld of
        T.SimpleText txt -> (LBS.fromStrict (T.encodeUtf8 txt)) `shouldBe` test1Expected
        _ -> error "Custom formatting builder should return a SimpleText."

    it "Log message with tag having a quote" $ do
      let bld = fmtPSMimicFormatterText
                  appFrameworkT
                  timestampT
                  hostNameT envT sourceCommitT
                  mbCtx2 pendingMsgWithQuotes
      case bld of
        T.SimpleText txt -> (LBS.fromStrict (T.encodeUtf8 txt)) `shouldBe` test2Expected
        _ -> error "Custom formatting builder should return a SimpleText."


benchmarking :: IO ()
benchmarking = do
  defaultMain [
    bgroup "psMimic" [ bench "original"  $ whnf origBenchSample 0
                     , bench "custom fmt #1 String" $ whnf custom1BenchSample 0
                     , bench "custom fmt #2 Text" $ whnf custom2BenchSample 0
                     ]
    ]

origBenchSample _ = sample
  where
    bld = originalPSMimicFormatter mbCtx2 pendingMsgWithQuotes
    sample = case bld of
      T.MsgTransformer f -> Log.render renderer f
      _ -> error "Original formatter is based on the Msg transforming function."

custom1BenchSample _ = sample
  where
    bld = fmtPSMimicFormatterString
                  appFramework
                  timestamp
                  hostName env sourceCommit
                  mbCtx2 pendingMsgWithQuotes
    sample = case bld of
        T.SimpleString str -> str
        _ -> error "Custom formatting builder should return a SimpleString."

custom2BenchSample _ = sample
  where
    bld = fmtPSMimicFormatterText
                  appFrameworkT
                  timestampT
                  hostNameT envT sourceCommitT
                  mbCtx2 pendingMsgWithQuotes
    sample = case bld of
        T.SimpleText txt -> txt
        _ -> error "Custom formatting builder should return a SimpleText."
