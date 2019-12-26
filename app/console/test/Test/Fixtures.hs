module Test.Fixtures where

import Universum

import qualified Control.Concurrent as C
import Dashboard.Auth.Types (LoginContext(..), Role(..), Token(..))
import Dashboard.Query.Backend.BigQuery (newBigQueryBackend)
import Dashboard.Query.Config
import qualified Network.Wai.Handler.Warp as Warp
import Servant (Proxy(..), serve)

import Console.API (QueryAPINoAuth, queryHandler)

testPort = 8084

bqCreds :: Text
bqCreds = "{ \"type\": \"service_account\", \"project_id\": \"instant-text-253509\", \"private_key_id\": \"687aeb97f56b420038358d0cd0fd6918c99aa4e4\", \"private_key\": \"-----BEGIN PRIVATE KEY-----\nMIIEvAIBADANBgkqhkiG9w0BAQEFAASCBKYwggSiAgEAAoIBAQCD89yZKlicuOlD\nuPYUxSFvO+QvHzcivs11dq9ZepwMvbWSonMhTwHOki5YpDK87h7kaEX6KAAhxAP9\niDmiYmRTXi2mZTSkITvQqsdPFskndW7TsepPXl32gwl5VMZWk/kyXdSdxoSdlRlC\n2bRm/yLfx/vNVFHFKGNraDw3nVglGvdjZA0nsXGPE5lDP88l8nOOdj8HHdYxubTK\nBjtSmqAsdx2MGROybp7JG8q1qDlMYGTH2A3wrF2heCNhef3dYZAvpB/6e1JS5Mdn\nQN8LeE/bDGnYOu2C4KWFrtW7ZOnKxuL/td3fOKW0lJhL6B1HjexzCDaF1p/CBMFx\nUlqoIfJnAgMBAAECggEAOFeW0KHHv3enRp8CGEFKMvRYTRSflsrw8OCSI/taCYix\nsKCD12ukYl+BwaCu94QXlcC+bH/K+p/3cjjKHz+zBfP3Ow8IyLJWF5XsT6Vzk9Et\nVuAno+cv6PPvxiqAG/TIGYz+HbZn5t+Er0pFIRt8+R+rsXN5DN3fgbu6xqZ9Pi4m\nD+h5x1QMXzW7+nxLdFOxOFvucafSMWAWjcT432zKLyhoTXZQlCzU5J5FaIJ0ZJ4V\nqZUx6a0IogaYBELLDVnqViGm6V2alPAT56sm2EYN65dzUfROq2vQB9XkhjW9HXC8\nfPNqin3mBG7Z5MM7AwMfxgdb2uYLfq7CmjsiLllHsQKBgQC5qlN6ChMN/cF8QMiP\nCiwDLNbnXLMDIQHcC0szuqp9+p3v2OQSVdLOjDf8dZq4yjp82TYdwAQ/G2OHB3dU\n9CtKMSpf7WiV+EI7Q5rdLWdA+mrud+xnfqzh+DTDcLXHuHo598dfu6Bl9wWuH/gi\njz69wC77ULUDu+D0I47mnBu61wKBgQC18IQ5oRupyF1jBhOucvA1nyXIRnc2bDGI\nYQUwSlmphUnnUbVh3wy9IKur+UYk7d+buJ0MbWR15euI49lJ34K1uvYs1S1gPW1+\nXBe3H1Oa8xRUcS8gJgmDXNPAxVBgjDzhVQivKlQRJ5CV+9y7yEkVHmTnOWnghPgF\nOiI+Yi2i8QKBgAzyXaPS9AUzvrxFYDbpRKxsAghcJHeBY7nOZ84RpDXotf61pyHI\nLk0M5hKKuOFN6Mov82AbtbAY3tXZpW6dIC+qQgQyb+qVRzkOu8vUkLCtRaufXTLS\nWIfhYPyH7YUhzhQgvRxl3wd9ND4yKIFwb7WXoy8PL7A097jOifZ2yFA9AoGAUpy5\njWloJDnEnbgeq7Go/NAc2cmVS/LosuJK9thryhupqQ1i7OPY5u+0w8vWW+JfDFhR\nh1kMqrQRtQCwo2iH83jHWygZCIAb/apXYmEpF0yUyJr0KQbcfvdhd0Cg9JX49uAj\np1XzAi8S6eeJHQMenCZG7u8U8kuuSsFQ1z7BNiECgYATiy/rQ1K1hkAvkGi0Ujs4\ncDJF7lsZaqiXvvzPoc9lY+oHM5gIr7FUTuNUOax0nzGRY+u1I2qDR8Mzqo73yRAz\n4YWv7CGxv9yrMUg1c7aK2F3fLLMkpvOOjLR35e6lFw92kO7osAoT5OYh8AdVHNAp\nwvCQd+4UVj+1dL9mb9x8/A==\n-----END PRIVATE KEY-----\n\", \"client_email\": \"jp-bigquery-test-acc@instant-text-253509.iam.gserviceaccount.com\", \"client_id\": \"109999465518538585297\", \"auth_uri\": \"https://accounts.google.com/o/oauth2/auth\", \"token_uri\": \"https://oauth2.googleapis.com/token\", \"auth_provider_x509_cert_url\": \"https://www.googleapis.com/oauth2/v1/certs\", \"client_x509_cert_url\": \"https://www.googleapis.com/robot/v1/metadata/x509/jp-bigquery-test-acc%40instant-text-253509.iam.gserviceaccount.com\" }"

ecQueryConf :: QueryConfiguration
ecQueryConf =
  QueryConfiguration
    [ TableConfiguration
        "godel-big-q.express_checkout.express_checkout20190927"
        [ FieldConfiguration "amount" FloatType
        , FieldConfiguration "gateway" StringType
        , FieldConfiguration "card_type" StringType
        , FieldConfiguration "merchant_id" StringType
        , FieldConfiguration "order_last_modified" StringType
        ]
    , TableConfiguration
        "properati-data-public.properties_cl.properties_rent_201801"
        [ FieldConfiguration "created_on" DateTimeType
        , FieldConfiguration "state_name" StringType
        , FieldConfiguration "price" FloatType
        , FieldConfiguration "place_name" StringType
        ]
    ]

token :: Token
token = Token { userId = 0
              , userName = "test"
              , email = Nothing
              , roles = [RoleAdmin]
              , ownerId = ""
              , loginContext = LoginContextJuspay
              , merchantAccountId = Nothing
              , merchantId = Nothing
              , resellerId = Nothing
              }

queryAPINoAuth :: Proxy QueryAPINoAuth
queryAPINoAuth = Proxy

withConsoleServer :: IO () -> IO ()
withConsoleServer action = do
  backend <- newBigQueryBackend "instant-text-253509" (Just bqCreds)
  let handler = queryHandler backend ecQueryConf token

  bracket (liftIO $ C.forkIO $ Warp.run testPort $ serve queryAPINoAuth handler)
    C.killThread
    (const action)
