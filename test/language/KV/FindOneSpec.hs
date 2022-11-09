module KV.FindOneSpec where

import           EulerHS.Prelude hiding (id)
import           KV.FlowHelper
import           KV.TestHelper
import qualified EulerHS.CachedSqlDBQuery as DB
import qualified EulerHS.Language as L
import qualified Data.Text as Text
-- import           Database.Beam.MySQL (MySQLM)
-- import qualified EulerHS.Types as T
import           Test.Hspec
import           KV.TestSchema.ServiceConfiguration
import           KV.TestSchema.Mesh
import           Sequelize (Clause(..), Term(..))

spec :: HasCallStack => Spec
spec = flowSpec $ do
    itFlow "Should be able to fetch created entry using secondary key and filter in application" $ do
      randomName <- Text.take 5 <$> L.generateGUID
      let value1 = "value1" <> randomName
      let value2 = "value2" <> randomName
      let serviceConfig1 = mkServiceConfig "name1" value1
      let serviceConfig2 = mkServiceConfig "name1" value2
      withTableEntry serviceConfig1 $ \serviceConfig dbConf -> do
        _eitherSc2 <- fromRightErr <$> DB.createReturning dbConf meshConfig serviceConfig2 Nothing
        maybeRes  <- fromRightErr <$> DB.findOne' dbConf meshConfig Nothing [Is id (Eq serviceConfig.id),Is value (Eq $ serviceConfig.value)]
        asserting $ ((.id) <$>  maybeRes) `shouldBe` (Just serviceConfig.id)
