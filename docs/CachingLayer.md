# Caching Layer
This layer executes sequelize queries on Beam storage types.
Each function optionally takes a cache key. If no key is provided
the query is executed using the Beam backend. If a key is provided
this layer will lookup the key and return the result if something is there. Otherwise it will use the DB backend and cache the result using the provided key.

## Caching
This layer just uses the provided keys for caching and does not provide any consistency guarantees.
The user also has to ensure that queries returning `[a]`
and queries returning `Maybe a` use different keys.
Queries returning `Maybe a` are:
`findOne`, `updateOne` and `create`
Queries returning `[a]` are:
`findAll`

## Example usage
Queries work on Beam storage types:
```haskell
{-# LANGUAGE DeriveAnyClass #-}

import qualified Database.Beam as B

import qualified EulerHS.Types as T
import EulerHS.Prelude

-- The beam storage type
-- https://haskell-beam.github.io/beam/user-guide/models/
data UserT f = UserT
  { _email :: B.Columnar f Text
  , _name :: B.Columnar f Text
  }

-- The type of a row. This is what queries return.
type User = UserT Identity

instance B.Beamable UserT

instance B.Table UserT where
  data PrimaryKey UserT f = UserId (B.Columnar f Text) deriving (Generic, B.Beamable)
  primaryKey = UserId . _email

instance ModelMeta UserT where
  -- You can specify the mapping from record field names
  -- to database field names here.
  -- The default mapping in this case is:
  -- _email -> "email"
  -- _name -> "name"
  modelFieldModification = B.tableModification
  modelTableName = "user"
```

Some of the query functions take where clauses, which are constructed using sequelize.
For examples look at [Artyom's Sequelize port](https://bitbucket.org/juspay/haskell-sequelize/src/master/).

Creating a Model and finding it:
```haskell
import Database.Beam.Sqlite.Connection (SqliteM)
import Sequelize

import EulerHS.CachedSqlDBQuery
import EulerHS.Language (Flow)
import EulerHS.Types as T

poolConfig = T.PoolConfig
  { stripes = 1
  , keepAlive = 10
  , resourcesPerStripe = 50
  }

sqliteCfg :: DBConfig SqliteM
sqliteCfg = T.mkSQLitePoolConfig "SQliteDB" "testDB" poolConfig

program :: Flow (Maybe User)
program = do
  -- NOTE:
  -- For caching to work, redis has to be initialized.
  -- otherwise the queries will always use the DB
  -- _ <- initKVDBConnection redisCfg

  -- Create a model and insert it into the cache and the DB.
  let user = User "test@google.com" "Alonzo Curch"
  let testKey = "cacheKey0"
  _ <- create sqliteCfg user (Just testKey)

  -- findOne takes a list of clauses.
  -- This query uses the provided cache key which
  -- `create` should have written to.
  res <- findOne sqliteCfg (Just testKey)
    [ Is _email (Eq "test@google.com") ]
  case res of
    Right r -> return r
    Left _ -> return Nothing
```

