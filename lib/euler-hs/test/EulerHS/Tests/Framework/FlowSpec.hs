module EulerHS.Tests.Framework.FlowSpec where

import           EulerHS.Prelude hiding (getOption, get)
import           Test.Hspec hiding (runIO)
import           Network.Wai.Handler.Warp
import           Data.Aeson               (encode)
import qualified Data.ByteString.Lazy as BSL
import           Unsafe.Coerce
import qualified Data.UUID as UUID (fromText)
import qualified Control.Exception as E
import           Servant.Server
import           Servant.Client (BaseUrl(..), Scheme(..), ClientError(..))
import           EulerHS.Interpreters
import           EulerHS.Language as L
import qualified EulerHS.Types  as T
import           EulerHS.Runtime (withFlowRuntime)
import           EulerHS.TestData.Types
import           EulerHS.TestData.API.Client
import           EulerHS.TestData.Scenarios.Scenario1 (testScenario1)
import           EulerHS.Testing.Types (FlowMockedValues'(..))
import           EulerHS.Testing.Flow.Interpreter (runFlowWithTestInterpreter)
import           EulerHS.Tests.Framework.Common (initRTWithManagers)
import           EulerHS.Types (HttpManagerNotFound(..))


user :: Any
user = unsafeCoerce $ Right $ User "John" "Snow" "00000000-0000-0000-0000-000000000000"

localGUID :: Any
localGUID = unsafeCoerce ("FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF" :: String)

lhost :: BSL.ByteString
lhost = encode ("localhost" :: String)


scenario1MockedValues :: FlowMockedValues'
scenario1MockedValues = FlowMockedValues'
  { mockedCallServantAPI = [user]
  , mockedRunIO = [localGUID]
  , mockedGetOption = [lhost]
  , mockedGenerateGUID = ["00000000-0000-0000-0000-000000000000"]
  , mockedRunSysCmd = ["Neo"]
  }

ioActWithException :: IO Text
ioActWithException = do
  E.throw (E.AssertionFailed "Exception from IO")
  pure "Text from IO"

withServer :: IO () -> IO ()
withServer action = do
  serverStartupLock <- newEmptyMVar
  let settings = setBeforeMainLoop (putMVar serverStartupLock ()) $
        setPort port defaultSettings
  threadId          <- forkIO $ runSettings settings $ serve api server
  readMVar serverStartupLock
  finally action (killThread threadId)


spec :: Spec
spec = do
  around (withFlowRuntime Nothing) $ do

    describe "EulerHS flow language tests" $ do

      describe "TestInterpreters" $ do

        it "testScenario1" $ \rt -> do
          mv <- newMVar scenario1MockedValues
          res <- runFlowWithTestInterpreter mv rt testScenario1
          res `shouldBe` (User "John" "Snow" "00000000-0000-0000-0000-000000000000")

      around_ withServer $ do
        describe "CallServantAPI tests with server" $ do

          it "Simple request (book) with default manager" $ \rt -> do
            let url = BaseUrl Http "localhost" port ""
            bookEither <- runFlow rt $ callServantAPI Nothing url getBook
            bookEither `shouldSatisfy` isRight

          it "Simple request (user) with default manager" $ \rt -> do
            let url = BaseUrl Http "localhost" port ""
            userEither <- runFlow rt $ callServantAPI Nothing url getUser
            userEither `shouldSatisfy` isRight

          it "Simple request (book) with manager1" $ \_ -> do
            rt <- initRTWithManagers
            let url = BaseUrl Http "localhost" port ""
            bookEither <- runFlow rt $ callServantAPI (Just "manager1") url getBook
            bookEither `shouldSatisfy` isRight

          it "Simple request (user) with manager2" $ \_ -> do
            rt <- initRTWithManagers
            let url = BaseUrl Http "localhost" port ""
            userEither <- runFlow rt $ callServantAPI (Just "manager2") url getUser
            userEither `shouldSatisfy` isRight

          it "Simple request with not existing manager" $ \_ -> do
            rt <- initRTWithManagers
            let url = BaseUrl Http "localhost" port ""
            let error = displayException (ConnectionError (toException $ HttpManagerNotFound "notexist"))
            userEither <- runFlow rt $ callServantAPI (Just "notexist") url getUser
            case userEither of
              Left e -> displayException e `shouldBe` error
              Right x -> fail "Success result not expected"



      describe "CallServantAPI tests without server" $ do

        it "Simple request (book)" $ \rt -> do
          let url = BaseUrl Http "localhost" port ""
          bookEither <- runFlow rt $ callServantAPI Nothing url getBook
          bookEither `shouldSatisfy` isLeft

        it "Simple request (user)" $ \rt -> do
          let url = BaseUrl Http "localhost" port ""
          userEither <- runFlow rt $ callServantAPI Nothing url getUser
          userEither `shouldSatisfy` isLeft

      it "RunIO" $ \rt -> do
        result <- runFlow rt $ runIO (pure ("hi" :: String))
        result `shouldBe` "hi"

      it "RunIO with exception" $ \rt -> do
        result <- E.catch
          (runFlow rt $ do
            runIO ioActWithException
            pure ("Never returned" :: Text))
          (\e -> do let err = show (e :: E.AssertionFailed)
                    pure err)
        result `shouldBe` ("Exception from IO" :: Text)

      it "RunIO with catched exception" $ \rt -> do
        result <-runFlow rt $ do
            runIO $
              E.catch
                ioActWithException
                (\e -> do let err = show (e :: E.AssertionFailed)
                          pure err)
        result `shouldBe` ("Exception from IO" :: Text)

      describe "Options" $ do

        it "One key" $ \rt -> do
          result <- runFlow rt $ do
            _ <- setOption TestStringKey "lore ipsum"
            getOption TestStringKey
          result `shouldBe` (Just "lore ipsum")

        it "Not found" $ \rt -> do
          result <- runFlow rt $ do
            _ <- setOption TestStringKey "lore ipsum"
            getOption TestStringKey2
          result `shouldBe` Nothing


        it "Two keys" $ \rt -> do
          result <- runFlow rt $ do
            _ <- setOption TestStringKey "lore ipsum"
            _ <- setOption TestStringKey2 "lore ipsum2"
            s1 <- getOption TestStringKey
            s2 <- getOption TestStringKey2
            pure (s1,s2)
          result `shouldBe` (Just "lore ipsum", Just "lore ipsum2")

        it "Different encoding, types & payload" $ \rt -> do
          testKVals <- runFlow rt $ do
            _     <- setOption (TestStringKey                              ) "mbTestStringKey"
            _     <- setOption (TestStringKey2                             ) "mbTestStringKey2"
            _     <- setOption (TestIntKey                                 ) 1001
            _     <- setOption (TestIntKey2                                ) 2002
            _     <- setOption (TestStringKeyAnotherEnc                    ) "mbTestStringKeyAnotherEnc"
            _     <- setOption (TestStringKey2AnotherEnc                   ) "mbTestStringKey2AnotherEnc"
            _     <- setOption (TestKeyWithStringPayload             "SP1" ) "mbTestKeyWithStringPayloadS1"
            _     <- setOption (TestKeyWithStringPayload             "SP2" ) "mbTestKeyWithStringPayloadS2"
            _     <- setOption (TestKeyWithIntPayload                1001  ) "mbTestKeyWithIntPayloadS1"
            _     <- setOption (TestKeyWithIntPayload                2002  ) "mbTestKeyWithIntPayloadS2"
            _     <- setOption (TestKeyWithStringPayloadAnotherEnc   "SP1" ) "mbTestKeyWithStringPayloadAnotherEncS1"
            _     <- setOption (TestKeyWithStringPayloadAnotherEnc   "SP2" ) "mbTestKeyWithStringPayloadAnotherEncS2"
            _     <- setOption (TestKeyWithIntPayloadAnotherEnc      1001  ) "mbTestKeyWithIntPayloadAnotherEncS1"
            _     <- setOption (TestKeyWithIntPayloadAnotherEnc      2002  ) "mbTestKeyWithIntPayloadAnotherEncS2"
            _     <- setOption (NTTestKeyWithStringPayload           "SP1" ) "mbNTTestKeyWithStringPayloadS1"
            _     <- setOption (NTTestKeyWithStringPayload           "SP2" ) "mbNTTestKeyWithStringPayloadS2"
            _     <- setOption (NTTestKeyWithIntPayload              1001  ) 2333
            _     <- setOption (NTTestKeyWithIntPayload              2002  ) 3322
            _     <- setOption (NTTestKeyWithStringPayloadAnotherEnc "SP1" ) "mbNTTestKeyWithStringPayloadAnotherEncS1"
            _     <- setOption (NTTestKeyWithStringPayloadAnotherEnc "SP2" ) "mbNTTestKeyWithStringPayloadAnotherEncS2"
            _     <- setOption (NTTestKeyWithIntPayloadAnotherEnc    1001  ) 9009
            _     <- setOption (NTTestKeyWithIntPayloadAnotherEnc    2002  ) 1001

            TestKVals
               <$> getOption (TestStringKey                              )
               <*> getOption (TestStringKey2                             )
               <*> getOption (TestIntKey                                 )
               <*> getOption (TestIntKey2                                )
               <*> getOption (TestStringKeyAnotherEnc                    )
               <*> getOption (TestStringKey2AnotherEnc                   )
               <*> getOption (TestKeyWithStringPayload             "SP1" )
               <*> getOption (TestKeyWithStringPayload             "SP2" )
               <*> getOption (TestKeyWithIntPayload                1001  )
               <*> getOption (TestKeyWithIntPayload                2002  )
               <*> getOption (TestKeyWithStringPayloadAnotherEnc   "SP1" )
               <*> getOption (TestKeyWithStringPayloadAnotherEnc   "SP2" )
               <*> getOption (TestKeyWithIntPayloadAnotherEnc      1001  )
               <*> getOption (TestKeyWithIntPayloadAnotherEnc      2002  )
               <*> getOption (NTTestKeyWithStringPayload           "SP1" )
               <*> getOption (NTTestKeyWithStringPayload           "SP2" )
               <*> getOption (NTTestKeyWithIntPayload              1001  )
               <*> getOption (NTTestKeyWithIntPayload              2002  )
               <*> getOption (NTTestKeyWithStringPayloadAnotherEnc "SP1" )
               <*> getOption (NTTestKeyWithStringPayloadAnotherEnc "SP2" )
               <*> getOption (NTTestKeyWithIntPayloadAnotherEnc    1001  )
               <*> getOption (NTTestKeyWithIntPayloadAnotherEnc    2002  )

          testKVals `shouldBe` TestKVals
                  { mbTestStringKey                          = Just "mbTestStringKey"
                  , mbTestStringKey2                         = Just "mbTestStringKey2"
                  , mbTestIntKey                             = Just 1001
                  , mbTestIntKey2                            = Just 2002
                  , mbTestStringKeyAnotherEnc                = Just "mbTestStringKeyAnotherEnc"
                  , mbTestStringKey2AnotherEnc               = Just "mbTestStringKey2AnotherEnc"
                  , mbTestKeyWithStringPayloadS1             = Just "mbTestKeyWithStringPayloadS1"
                  , mbTestKeyWithStringPayloadS2             = Just "mbTestKeyWithStringPayloadS2"
                  , mbTestKeyWithIntPayloadS1                = Just "mbTestKeyWithIntPayloadS1"
                  , mbTestKeyWithIntPayloadS2                = Just "mbTestKeyWithIntPayloadS2"
                  , mbTestKeyWithStringPayloadAnotherEncS1   = Just "mbTestKeyWithStringPayloadAnotherEncS1"
                  , mbTestKeyWithStringPayloadAnotherEncS2   = Just "mbTestKeyWithStringPayloadAnotherEncS2"
                  , mbTestKeyWithIntPayloadAnotherEncS1      = Just "mbTestKeyWithIntPayloadAnotherEncS1"
                  , mbTestKeyWithIntPayloadAnotherEncS2      = Just "mbTestKeyWithIntPayloadAnotherEncS2"
                  , mbNTTestKeyWithStringPayloadS1           = Just "mbNTTestKeyWithStringPayloadS1"
                  , mbNTTestKeyWithStringPayloadS2           = Just "mbNTTestKeyWithStringPayloadS2"
                  , mbNTTestKeyWithIntPayloadS1              = Just 2333
                  , mbNTTestKeyWithIntPayloadS2              = Just 3322
                  , mbNTTestKeyWithStringPayloadAnotherEncS1 = Just "mbNTTestKeyWithStringPayloadAnotherEncS1"
                  , mbNTTestKeyWithStringPayloadAnotherEncS2 = Just "mbNTTestKeyWithStringPayloadAnotherEncS2"
                  , mbNTTestKeyWithIntPayloadAnotherEncS1    = Just 9009
                  , mbNTTestKeyWithIntPayloadAnotherEncS2    = Just 1001
                  }

      it "RunSysCmd" $ \rt -> do
        result <- runFlow rt $ runSysCmd "echo test"
        result `shouldBe` "test\n"

      it "RunSysCmd with bad command" $ \rt -> do
        result <- E.catch
          (runFlow rt $ runSysCmd "badEcho test")
          (\e -> do let err = show (e :: E.SomeException)
                    pure err)
        result `shouldBe` ("readCreateProcess: badEcho test (exit 127): failed" :: String)


      it "GenerateGUID" $ \rt -> do
        guid <- runFlow rt generateGUID
        let maybeGUID = UUID.fromText guid
        maybeGUID `shouldSatisfy` isJust

      it "ThrowException" $ \rt -> do
        result <- E.catch
          (runFlow rt $ do
            throwException (E.AssertionFailed "Exception message")
            pure "Never returned")
          (\e -> do let err = show (e :: E.AssertionFailed)
                    pure err)
        result `shouldBe` "Exception message"

      describe "ForkFlow" $ do
        let i :: Int = 101
        it "Fork and successful await infinitely" $ \rt -> do
          let flow = do
                awaitable <- forkFlow' "101" (pure i)
                await Nothing awaitable
          result <- runFlow rt flow
          result `shouldBe` (Just 101)

        -- This might or might not happen (race condition)
        -- it "Fork and successful await 0" $ \rt -> do
        --   let flow = do
        --         awaitable <- forkFlow' "101" (pure i)
        --         await (Just $ T.Microseconds 0) awaitable
        --   result <- runFlow rt flow
        --   result `shouldBe` (Just 101)

        it "Fork and successful await with a sufficient timeout 1" $ \rt -> do
          let flow = do
                awaitable <- forkFlow' "101" (pure i)
                await (Just $ T.Microseconds 1000000) awaitable
          result <- runFlow rt flow
          result `shouldBe` (Just 101)

        it "Fork and successful await with a sufficient timeout 2" $ \rt -> do
          let flow = do
                awaitable <- forkFlow' "101" (runIO (threadDelay 1000) >> pure i)
                await (Just $ T.Microseconds 1000000) awaitable
          result <- runFlow rt flow
          result `shouldBe` (Just 101)

        it "Fork and successful await with an unsufficient timeout" $ \rt -> do
          let flow = do
                awaitable <- forkFlow' "101" (runIO (threadDelay 1000000) >> pure i)
                await (Just $ T.Microseconds 1000) awaitable
          result <- runFlow rt flow
          result `shouldBe` Nothing

        it "Fork and successful await for 2 flows" $ \rt -> do
          let flow = do
                awaitable1 <- forkFlow' "101" (runIO (threadDelay 10000) >> pure i)
                awaitable2 <- forkFlow' "102" (runIO (threadDelay 100000) >> pure (i+1))
                mbRes1 <- await Nothing awaitable1
                mbRes2 <- await Nothing awaitable2
                pure (mbRes1, mbRes2)
          result <- runFlow rt flow
          result `shouldBe` (Just 101, Just 102)

        it "Fork and successful await 1 of 2 flows" $ \rt -> do
          let flow = do
                awaitable1 <- forkFlow' "101" (runIO (threadDelay 10000) >> pure i)
                awaitable2 <- forkFlow' "102" (runIO (threadDelay 1000000) >> pure (i+1))
                mbRes1 <- await Nothing awaitable1
                mbRes2 <- await (Just $ T.Microseconds 1000) awaitable2
                pure (mbRes1, mbRes2)
          result <- runFlow rt flow
          result `shouldBe` (Just 101, Nothing)

          
