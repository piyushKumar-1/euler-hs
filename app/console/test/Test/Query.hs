module Test.Query where

import Universum

import Data.Maybe (fromJust)
import Data.Time.ISO8601 (parseISO8601)
import Test.Hspec

import Test.Fixtures (queryAPINoAuth, testPort)
import qualified Dashboard.Query.Types as QT

import Servant.Client (client, runClientM, mkClientEnv, parseBaseUrl)
import Network.HTTP.Client (newManager, defaultManagerSettings)

makeTimestamp :: String -> QT.Timestamp
makeTimestamp = QT.Timestamp . fromJust . parseISO8601

testQuery :: QT.Query
testQuery = QT.Query (QT.Selection [ (Just QT.Sum, QT.Field "amount")
                                   ])
                     "bq-dashboard-test.express_checkout.console_dashboard_query_"
                     (QT.Interval { start = makeTimestamp "2019-10-21T00:00:00Z"
                                  , stop  = makeTimestamp "2019-10-21T23:59:59Z"
                                  , step  = Just . QT.Milliseconds $ 6 * 60 * 60 * 1000
                                  , field = "order_last_modified"
                                  })
                     (QT.Filter [("merchant_id", QT.Equal, QT.StringValue "com.twiggy2")])
                     (QT.GroupBy ["card_type"])

testResultRow :: QT.QueryResultRow
testResultRow = QT.QueryResultRow ts1 ts2 [QT.FloatValue 887.62]
  where
    ts1 = QT.Timestamp . fromJust . parseISO8601 $ "2019-10-21T00:00:00Z"
    ts2 = QT.Timestamp . fromJust . parseISO8601 $ "2019-10-21T06:00:00Z"

incorrectQuery :: QT.Query
incorrectQuery = QT.Query (QT.Selection
                            [(Just QT.Count, QT.All)])
                            "bad_table"
                            (QT.Interval { start = makeTimestamp "1970-01-01T00:00:00Z"
                                         , stop  = makeTimestamp "1970-01-01T00:10:00Z"
                                         , step  = Just $ QT.Milliseconds 15
                                         , field = "field1"})
                            (QT.Filter []) (QT.GroupBy [])

specs :: Spec
specs = describe "Query API" $ do
      let queryClient = client queryAPINoAuth
      baseUrl <- runIO . parseBaseUrl $ "http://localhost:" ++ show testPort
      manager <- runIO . newManager $ defaultManagerSettings
      let clientEnv = mkClientEnv manager baseUrl

      it "should return a queryresult when a query is given" $ do
        result <- runClientM (queryClient testQuery) clientEnv
        -- Check the first value
        map (\(QT.QueryResult xs) -> minimum xs) result `shouldBe` Right testResultRow
        -- Check the number of results
        map (\(QT.QueryResult xs) -> length xs) result `shouldBe` Right 4

      -- FIXME: Use hspec-wai and check for a 400 here and responseBody
      it "should return a failure when an incorrect query is given" $ do
        result <- runClientM (queryClient incorrectQuery) clientEnv
        result `shouldSatisfy` isLeft
