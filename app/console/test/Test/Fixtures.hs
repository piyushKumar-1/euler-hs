module Test.Fixtures where

import Universum

import qualified Control.Concurrent as C
import Dashboard.Auth.Types (LoginContext(..), Role(..), Token(..))
import Dashboard.Query.Backend.BigQuery (BackendConfig(..), newBigQueryBackend)
import Dashboard.Query.Config
import qualified Network.Wai.Handler.Warp as Warp
import Servant (hoistServer, Proxy(..), serve)

import Console.API (QueryAPINoAuth, queryHandler)
import Console.HTTPServer (server, toHandler)

testPort = 8084

bqCreds :: Text
bqCreds = "{\"type\": \"service_account\", \"project_id\": \"bq-dashboard-test\", \"private_key_id\": \"fddb0b52a4e368dfbfb3f424e3598af916fcfa88\", \"private_key\": \"-----BEGIN PRIVATE KEY-----\nMIIEvAIBADANBgkqhkiG9w0BAQEFAASCBKYwggSiAgEAAoIBAQCr3eLgr8+DT+i6\nsYP2kCeTjA6hkm3RBBhX5wKBvkaCbqUUuwGFLtNoVOijSectzS1mBqdys7imU6Qo\nw4fQ98NpuAL7Mvx0lJH4r5takdUnlAwAJ4kMYw7YGrSP9kJiHeKtXL9Kpp4PkLF1\nEQd+N+0TzHZvfhrdejtCzIgqGcTl7cLXR515bYlvW7uGj8/fG7IagXl1+fyo8l50\npg1VFJre4yqlbARKFny4KZs3v3CNkDJvfEochxlz0WXvrCXAQyjrjky6GPYyFr+3\n7gfKw8OVhuBlF0+fXVqBpJAIf3qVNdFMG1dazdkgKdCBSlUZrSzPK4s75uLURa0e\nqN4KmpgJAgMBAAECggEADbh5FFb0yhbnDYB+eICpqC3WPS/jdHxPNTVMDe+S0Xbj\n1vPW+zrxxbKBoAeWXSG0LiD53T5bz2t9/4McpKIjB52ZH1TCssvpm2DpliFuUuqO\nyrRdTI/b74SyRYvzYMHiBtUQsHT8yWFTyXTClGECnEIGcoAuxN+tO3f4puPHxFNF\nW9IdLOBuce2mNFS8QhhbR0tGhr6zMR4w/4IAgRREr56U9iOT9gPxnf1RDhbkKYh3\nx6FtNrgny1FE0miCuuS19vKhhoZpt8GbNR8HRnyT4ij2TT99xv6CZo19avcjXuqZ\nSWbePtvGtgS2xm3T7qWGEzbfdnHY6R8SYP52KEb4AQKBgQDfdKPkcxssGN1SskYX\n3AFVTq8KufBOZsfO5yFfXWZYvy+3HSIuTtyEOS7CUuYnyoT3v4ofJNYfxy//jh6b\n1l7uyB9mizVgtbLB1GSysvBDDcedEZVq/trBUtW1dHZYMAO4fsxl1LTklHe4EIVg\noRS8phImTMMaEusOPIeiEYQQ2QKBgQDE5cv995Bwy1VNVO1PjjqWvvmu1NWGVzx2\nnQdrRbwRZTeIcGJkxyuBZ4u5zcGwFKCxre4ENUaTuw/HRxDPbER6i+r2LO9Qrrku\nGQ3QhHUMugFkr5pn1ST6AvjvTVqifZBsX7LG9+gk/wCKMeoGplKIAIn/SqwJMnrw\nTI9FCatCsQKBgDUBfCcizt1FAKsvMsVSINbVlVwcA071vovr89SlWTPQFkd/l64Y\ncuLClb7u3z2tIqGKQ/9jQ4/CNjaOxd/9mANYYWp3ty+7qi2kbkadf7TIPgOaOjFp\nRwb42Uzz1WbR7PAE1AOL02UtesCk2p6zVzHwyvle2CEFP9DetuXQ9dzxAoGANt2p\n6E0R7LxcOr5NmnuRi+wPe05HabXpXUwIgzXdYCOKQ/S9FkQBtX97Cf1x2XUGCjzj\nNDz8O7JGhUsiPgp3UoampI6E6UCmzVeh59bhHvi95qagKgrU0O3/G/+8H8dJdUl9\nccAm6hg8uEWMZun12Xv8+CyAn9d3Nnq++YfmNyECgYANfbt1dQ6KD3PI1BjCnBoP\nRLMGqC8X5bA7Jl7zZ/KNPWF4JkIpv7GoGT6fJzWxq1IlWcBC321X3iR4hDKyMHVv\nL5PsXATyVqbOW7QjgULM83gneuOFNlloU2AIdAbGJWEMPrQFpdXqoJwx7EYSmen2\nWFwBjujSDCoXF50vfyqGFg==\n-----END PRIVATE KEY-----\n\", \"client_email\": \"bq-query@bq-dashboard-test.iam.gserviceaccount.com\", \"client_id\": \"118335525984380740448\", \"auth_uri\": \"https://accounts.google.com/o/oauth2/auth\", \"token_uri\": \"https://oauth2.googleapis.com/token\", \"auth_provider_x509_cert_url\": \"https://www.googleapis.com/oauth2/v1/certs\", \"client_x509_cert_url\": \"https://www.googleapis.com/robot/v1/metadata/x509/bq-query%40bq-dashboard-test.iam.gserviceaccount.com\" }"

ecQueryConf :: QueryConfiguration
ecQueryConf =
  QueryConfiguration
    [ TableConfiguration
        "bq-dashboard-test.express_checkout.console_dashboard_query_"
        [ FieldConfiguration "amount" FloatType []
        , FieldConfiguration "gateway" StringType []
        , FieldConfiguration "card_type" StringType []
        , FieldConfiguration "merchant_id" StringType []
        , FieldConfiguration "order_last_modified" StringType []
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
  backend <- newBigQueryBackend "bq-dashboard-test" (Just bqCreds)
  let bec = BackendConfig backend ecQueryConf
  let app = serve queryAPINoAuth $
              hoistServer queryAPINoAuth (toHandler bec) (server token)

  bracket (liftIO $ C.forkIO $ Warp.run testPort app)
    C.killThread
    (const action)
