{-# OPTIONS_GHC -Werror #-}

module Main (main) where

import           Control.Exception.Safe (bracket)
import           EulerHS.Prelude hiding (bracket)
import qualified EulerHS.Tests.Framework.ArtSpec as Art
import qualified EulerHS.Tests.Framework.FlowSpec as Framework
import qualified EulerHS.Tests.Framework.KVDBArtSpec as KVDB
import qualified EulerHS.Tests.Framework.PubSubSpec as PubSub
import qualified EulerHS.Tests.Framework.SQLArtSpec as SQL
import qualified EulerHS.Types as T
import           System.Directory (createDirectory, getTemporaryDirectory,
                                   removePathForcibly)
import           System.FilePath ((<.>), (</>))
import           System.Process.Typed (proc, startProcess, stopProcess)
import           System.Random (getStdRandom, random)
import           Test.Hspec (hspec)

main :: IO ()
main = do
  withRedis $ hspec $ do
    Framework.spec logsDisabled
    Art.spec
    -- Disable until it work in jenkins. Need to install redis
    -- CachedSqlDBQuery.spec
    KVDB.spec
    SQL.spec
    PubSub.spec

-- Helpers

withRedis :: IO () -> IO ()
withRedis act = withTempRedisDir $ \redisDir ->
  withTempRedisConfig redisDir go
  where
    go :: FilePath -> IO ()
    go redisConfPath =
      bracket (startProcess . proc "redis-server" $ [redisConfPath])
              stopProcess
              (const act)

logsDisabled :: Maybe T.LoggerConfig
logsDisabled = Nothing

withTempRedisDir :: (FilePath -> IO a) -> IO a
withTempRedisDir act = do
  rand :: Word <- liftIO . getStdRandom $ random
  tmp <- liftIO getTemporaryDirectory
  let tempDir = tmp </> ("redis" <> show rand)
  bracket (liftIO . createDirectory $ tempDir)
          (\_ -> liftIO . removePathForcibly $ tempDir)
          (\_ -> act tempDir)

withTempRedisConfig :: FilePath -> (FilePath -> IO ()) -> IO ()
withTempRedisConfig tmpRedisDir act = do
  let tmpRedisConfPath = tmpRedisDir </> "redis" <.> "conf"
  bracket (withFile tmpRedisConfPath WriteMode go)
          (\_ -> removePathForcibly tmpRedisConfPath)
          (\_ -> act tmpRedisConfPath)
  where
    go :: Handle -> IO ()
    go h = hPutStrLn @String h $ "dir " +| tmpRedisDir |+ ""
