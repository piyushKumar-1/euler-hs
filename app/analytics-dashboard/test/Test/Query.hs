module Test.Query where

import Universum

import Test.Hspec

import Test.Fixtures (withConsoleServer, testPort)
import Console.API (app, queryAPI)
import Console.Query (dummyResult, ts)
import qualified Dashboard.Query.Types as QT

import Servant.Client (client, runClientM, mkClientEnv, parseBaseUrl)
import Network.HTTP.Client (newManager, defaultManagerSettings)

dummyQuery :: QT.Query
dummyQuery = QT.Query (QT.Selection (QT.COUNT, QT.All)) "table" (QT.Interval { start = ts
                         , stop = ts
                         , step = Just $ QT.Milliseconds 3928
                         }) (QT.Filter []) (QT.GroupBy [])

specs :: Spec
specs = describe "Prelude.read" $ do
      let queryClient = client queryAPI
      baseUrl <- runIO $ parseBaseUrl $ "http://localhost:" ++ show testPort
      manager <- runIO $ newManager defaultManagerSettings
      let clientEnv = mkClientEnv manager baseUrl

      it "should return a queryresult when a query is given" $ do
        result <- runClientM (queryClient dummyQuery) clientEnv
        result `shouldBe` Right dummyResult
