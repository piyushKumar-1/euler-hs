module Dashboard.Metrics.Prometheus where

import Universum
import qualified Prometheus as P
import qualified Network.Wai.Middleware.Prometheus as PW

defaultPromSettings :: PW.PrometheusSettings
defaultPromSettings = PW.PrometheusSettings ["metrics"] True True

-- 20 buckets with an exponential factor 1.7 starting off with roughly 10 MB.
bqBuckets :: [Double]
bqBuckets = P.exponentialBuckets 10000000 1.7 20

redisResponseTime :: P.Histogram
redisResponseTime = P.unsafeRegister
                  $ P.histogram
                   (P.Info "redis_token_lookup" "Time for token lookup in Redis")
                    P.defaultBuckets

bqResponseTime :: P.Histogram
bqResponseTime = P.unsafeRegister
               $ P.histogram
                (P.Info "bq_response_time" "Big Query Response Time")
                 P.defaultBuckets

queryBytesProcessed :: P.Histogram
queryBytesProcessed = P.unsafeRegister
                    $ P.histogram
                     (P.Info "query_bytes_processed" "Total bytes processed for query")
                      bqBuckets

totalBytesProcessed :: P.Counter
totalBytesProcessed = P.unsafeRegister
                    $ P.counter
                    $ P.Info "query_bytes_processed_total" "Total bytes processed for query"

queryTotalRows :: P.Gauge
queryTotalRows = P.unsafeRegister
               $ P.gauge
               $ P.Info "query_total_rows" "Total number of rows in complete query result set"

authSuccess :: P.Counter
authSuccess = P.unsafeRegister
            $ P.counter
            $ P.Info "auth_success" "Counter for authentication successes"

authFailure :: P.Counter
authFailure = P.unsafeRegister
            $ P.counter
            $ P.Info "auth_failure" "Counter for authentication failures"

queryValidationFailed :: P.Counter
queryValidationFailed = P.unsafeRegister
                      $ P.counter
                      $ P.Info "queries_failed" "Total number of queries which failed validation"
