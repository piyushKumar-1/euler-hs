module Test.SQL where

import Universum

import Data.Maybe (fromJust)
import Data.Time.ISO8601 (parseISO8601)
import Test.Hspec

import Dashboard.Query.Backend.BigQuery.SQL (printSQL)
import qualified Dashboard.Query.Types as QT

makeTimestamp :: String -> QT.Timestamp
makeTimestamp = QT.Timestamp . fromJust . parseISO8601

startTime :: QT.Timestamp
startTime = makeTimestamp "2019-09-27T10:00:00Z"

stopTime :: QT.Timestamp
stopTime = makeTimestamp "2019-09-27T12:00:00Z"

testQuery :: QT.Query
testQuery = QT.Query (QT.Selection [ (Just QT.Sum, QT.Field "f1")])
                      "t"
                      (QT.Interval { start = startTime
                                   , stop  = stopTime
                                   , step  = Just . QT.Milliseconds $ 5 * 60 * 1000
                                   , field = "t1"
                                   })
                      (QT.Filter [("s1", QT.Equal, QT.StringValue "foo")])
                      (QT.GroupBy ["s2"])

singleSelectionQuery :: QT.Query
singleSelectionQuery = testQuery

multipleSelectionQuery :: QT.Query
multipleSelectionQuery = testQuery { QT.selection = QT.Selection [ (Just QT.Count, QT.All)
                                                                 , (Just QT.Sum, QT.Field "f1")
                                                                 , (Just QT.Average, QT.Field "i1")
                                                                 ]
                                   }

singleFilterQuery :: QT.Query
singleFilterQuery = testQuery

emptyFilterQuery :: QT.Query
emptyFilterQuery = testQuery { QT.filter = QT.Filter []}

multipleFilterQuery :: QT.Query
multipleFilterQuery = testQuery { QT.filter = QT.Filter [ ("s1", QT.Equal, QT.StringValue "foo")
                                                        , ("f1", QT.Equal, QT.FloatValue 1000.00)
                                                        ]
                                }

singleGroupByQuery :: QT.Query
singleGroupByQuery = testQuery

emptyGroupByQuery :: QT.Query
emptyGroupByQuery = testQuery { QT.groupBy = QT.GroupBy []}

multipleGroupByQuery :: QT.Query
multipleGroupByQuery = testQuery { QT.groupBy = QT.GroupBy ["f1", "i1", "s2"]}

noStepQuery :: QT.Query
noStepQuery = testQuery { QT.interval = QT.Interval { start = startTime
                                                    , stop  = stopTime
                                                    , step  = Nothing
                                                    , field = "t1"
                                                    }
                        }

selectStr :: Text
selectStr = "SELECT UNIX_SECONDS(TIMESTAMP(t1)) - MOD(UNIX_SECONDS(TIMESTAMP(t1)), 300) AS ts, "

whereStr :: Text
whereStr = " WHERE (UNIX_SECONDS(TIMESTAMP(t1)) BETWEEN 1569578400 AND 1569585600) "

groupByStr :: Text
groupByStr = " GROUP BY s2, ts;"

specs :: Spec
specs =
  describe "SQL generation" $ do
    it "should succeed with a single selection" $
      printSQL False singleSelectionQuery `shouldBe` selectStr <> "SUM(f1) FROM `t`" <> whereStr <> "AND s1 = \"foo\"" <> groupByStr

    it "should succeed with multiple selections" $
      printSQL False multipleSelectionQuery `shouldBe` selectStr <> "COUNT(*), SUM(f1), AVG(i1) FROM `t`" <> whereStr <> "AND s1 = \"foo\"" <> groupByStr 

    it "should succeed with a single filter" $
      printSQL False singleFilterQuery `shouldBe` selectStr <> "SUM(f1) FROM `t`" <> whereStr <> "AND s1 = \"foo\"" <> groupByStr 

    it "should succeed with an empty filter" $
      printSQL False emptyFilterQuery `shouldBe` selectStr <> "SUM(f1) FROM `t`" <> whereStr <> "GROUP BY s2, ts;"

    it "should succeed with an multiple filters" $
      printSQL False multipleFilterQuery `shouldBe` selectStr <> "SUM(f1) FROM `t`" <> whereStr <> "AND s1 = \"foo\" AND f1 = 1000.0" <> groupByStr 

    it "should succeed with a single group-by" $
      printSQL False singleGroupByQuery `shouldBe` selectStr <> "SUM(f1) FROM `t`" <> whereStr <> "AND s1 = \"foo\"" <> groupByStr 

    it "should succeed without a group-by" $
      printSQL False emptyGroupByQuery `shouldBe` selectStr <> "SUM(f1) FROM `t`" <> whereStr <> "AND s1 = \"foo\" GROUP BY ts;"

    it "should succeed without a multi-field group-by" $
      printSQL False multipleGroupByQuery `shouldBe` selectStr <> "SUM(f1) FROM `t`" <> whereStr <> "AND s1 = \"foo\" GROUP BY f1, i1, s2, ts;"

    it "should handle string date fields" $
      printSQL True multipleGroupByQuery `shouldBe` "SELECT UNIX_SECONDS(PARSE_TIMESTAMP('%F %T', t1)) - MOD(UNIX_SECONDS(PARSE_TIMESTAMP('%F %T', t1)), 300) AS ts, SUM(f1) FROM `t` WHERE (UNIX_SECONDS(PARSE_TIMESTAMP('%F %T', t1)) BETWEEN 1569578400 AND 1569585600) AND s1 = \"foo\" GROUP BY f1, i1, s2, ts;"

    it "should handle an interval without steps" $
      printSQL False noStepQuery `shouldBe` "SELECT 1569578400 AS ts, SUM(f1) FROM `t`" <> whereStr <> "AND s1 = \"foo\"" <> groupByStr 