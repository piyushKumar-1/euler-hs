# EulerHS Project

* [Books on Software Architecture in Haskell](#Books-on-Software-Architecture-in-Haskell)
* [Application Architectures](#Application-Architectures)

### EulerHS Framework

***euler-hs/Flow*** is a free monadic framework for building backend and console applications in Haskell.

The framework exports the Flow monad which provides the following facilities:

  - SQL DB interaction (using the `beam` library). Postgres, MySQL and SQLite DBs supported.
  - KV DB interaction. Redis is supported.
  - Forking flows in separate threads (green threads are used).
  - HTTP services interaction (using servant-client facilities).
  - Logging (tiny-logger inside).
  - Typed mutable options.
  - Pub/Sub mechanism (using Redis pub sub subsystem).
  - Safe call to IO actions.
  - Running system commands.
  - ART (Automatic Regression Testing) - white box testing facilities.
  - Integration testing framework.

### Euler Backend

***euler-backend*** is a web/REST/HTTP application, a direct port of euler-ps for implementing
  [Juspay APIs](https://www.juspay.in/docs/api/ec/) in Haskell.
  The application is based on the Servant web framework.
  for HTTP facilities and the Flow framework for business logic.

# Installation

### Build tools

You can use any of the two building tools supported:

- **Stack**, install from [https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/)
- **Nix package manager**: [A minimal guide to getting started with the Nix package manager on any Linux or OSX machine.](https://chris-martin.org/2016/installing-nix-package-manager) (current nix-channel is nixos-19.09)
  Setup in your ~/.cabal/config

  ```
  write-ghc-environment-files: never
  nix: True
  ```

### Dependencies

**Install development tools and libraries with your distro package manager:**

- binutils
- libssl-dev
- libpq-dev
- postgresql-server-dev-all
- libmysqlclient-dev
- libsqlite
- postgresql
- mysql-server
- ... maybe some others

git clone [git@bitbucket.org](mailto:git@bitbucket.org):juspay/euler-hs.git

or

git clone [https://user_name@bitbucket.org/juspay/euler-hs.git](https://user_name@bitbucket.org/juspay/euler-hs.git)

### Building and testing

#### Stack

**Build**

- `stack build` (Will build all the projects)
- `stack build --fast -j4` (Will skip some optimisations)
- `stack build euler-hs`
- `stack build euler-backend`

**Tests:**

- All tests:

    `stack test`

- backend dsl language tests:
    - `stack test euler-hs:language`
- SQL DB sub set of backend dsl (by default enabled only sqlite tests, if you want to test MySQL and Postgres then uncomment corresponding specs in `lib/euler-hs/testSqlDB/Main.hs` , you need an appropriate DB and tables)
    - `stack test euler-hs:sql`
- euler-backend tests:
    - `stack test euler-backend`
- ART tests:
    - `cd ./app/euler-backend`
    - `art.sh`

**Run:**

- `stack run euler-backend`

#### Nix

**Build**

From root project dir run

- For euler-hs lib  `nix-build -A euler-hs`
- For euler-backend app  `nix-build -A euler-backend`

    resulting binary should be in `./result/bin/`


# Usage guidelines

***See also:***

* [Tutorial](./TUTORIAL.md)
* [Architecture diagram](./docs/Architecture.png)
* [Beam query examples](./lib/euler-hs/testDB/SQLDB/Tests/QueryExamplesSpec.hs)

***Methods for connection management:***

*Takes SQL DB config and create connection that can be used in queries.*
```haskell
initSqlDBConnection :: T.DBConfig beM -> Flow (T.DBResult (T.SqlConn beM))
```

*Deinit the given connection if you want to deny access over that connection.*
```haskell
deinitSqlDBConnection :: T.SqlConn beM -> Flow ()
```

*Get existing connection. If there is no such connection, returns error.*
```haskell
getSqlDBConnection ::T.DBConfig beM -> Flow (T.DBResult (T.SqlConn beM))
```

*Get existing SQL connection, or init a new connection.*
```haskell
getOrInitSqlConn :: T.DBConfig beM -> L.Flow (T.DBResult (T.SqlConn beM))
```

***Methods for querying***

*Takes connection, sql query (described using BEAM syntax) and make request.*
```haskell
runDB
  ::
    ( T.JSONEx a
    , T.BeamRunner beM
    , T.BeamRuntime be beM
    )
  => T.SqlConn beM
  -> L.SqlDB beM a
  -> Flow (T.DBResult a)
```

*Extracting existing connection from FlowRuntime by given db config and runs sql query (described using BEAM syntax). Acts like 'getSqlDBConnection' + 'runDB'*
```haskell
withDB ::
  ( T.JSONEx a
  , T.BeamRunner beM
  , T.BeamRuntime be beM
  )
  => T.DBConfig beM -> L.SqlDB beM a -> Flow a
```

When you start the application, you can initialize all the connections that you plan to use.
```haskell
keepConnsAliveForSecs :: NominalDiffTime
keepConnsAliveForSecs = 60 * 10 -- 10 mins

maxTotalConns :: Int
maxTotalConns = 8

mySQLCfg :: MySQLConfig
mySQLCfg = MySQLConfig
  { connectHost     = "localhost"
  , connectPort     = 3306
  , connectUser     = "username"
  , connectPassword = "password"
  , connectDatabase = "dbname"
  , connectOptions  = [T.CharsetName "utf8"]
  , connectPath     = ""
  , connectSSL      = Nothing
  }

sqlDBcfg = mkMySQLPoolConfig "eulerMysqlDB" mySQLCfg
    $ PoolConfig 1 keepConnsAliveForSecs maxTotalConns

prepareDBConnections :: Flow ()
prepareDBConnections = do
  ePool <- initSqlDBConnection sqlDBcfg
  throwOnFailedWithLog ePool SqlDBConnectionFailedException "Failed to connect to SQL DB."

```

And then run flow methods with it
```haskell
endpointHandler :: RequestType -> Flow (Maybe Int)
endpointHandler req = do
    logInfo @String "endpointHandler" "endpointHandler started"
    validReq <- validateRequest req
    -- ...
    -- some other actions
    -- ...
    res <- withDB sqlDBcfg $ do
      let predicate DBTableType {idField} =
          (idField    ==. B.val_ (validReq ^. reqIdField))
      findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (dbTableName dbSchema)
    pure $ (^. intField) <$> res
```

Also, you can put your dbConfig in Options and take it back later in specialized `withDB` wrappers. Maybe helpful when you should create config on startup, so config can't be hardcoded as constant and easily passed in methods (e.g. read DB password from env var and decode it with some IO operation). You can manage many different db configs

At first define keys for DBs:
```haskell
data DB1Cfg = DB1Cfg
  deriving (Generic, Typeable, Show, Eq, ToJSON, FromJSON)

instance OptionEntity DB1Cfg (DBConfig MySQLM)

data DB2Cfg = DB2Cfg
  deriving (Generic, Typeable, Show, Eq, ToJSON, FromJSON)

instance OptionEntity DB2Cfg (DBConfig Pg)
```

Then you can define a specialized wrapper for each db:
```haskell
withDB1 :: JSONEx a => SqlDB MySQLM a -> Flow a
withDB1 act = do
  dbcfg <- getOption DB1Cfg
  case dbcfg of
    Just cfg -> withDB cfg act
    Nothing -> do
      logError @String "MissingDB identifier" "Can't find DB1 identifier in options"
      throwException YourException

withDB2 :: JSONEx a => SqlDB Pg a -> Flow a
withDB2 act = do
  dbcfg <- getOption DB2Cfg
  case dbcfg of
    Just cfg -> withDB cfg act
    Nothing -> do
      logError @String "MissingDB identifier" "Can't find DB2 identifier in options"
      throwException YourException
```
On startup initialization just put configs in Options

```haskell
prepareDBConnections :: Flow ()
prepareDBConnections = do
  sqlDBcfg1 <- runIO getFromEnvAndDecodeMySqlDbCfg
  ePool1 <- initSqlDBConnection sqlDBcfg1
  setOption DB1Cfg sqlDBcfg1
  throwOnFailedWithLog ePool SqlDBConnectionFailedException "Failed to connect to SQL DB1."
  sqlDBcfg2 <- runIO getFromEnvAndDecodePostgresDbCfg
  ePool2 <- initSqlDBConnection sqlDBcfg2
  setOption DB2Cfg sqlDBcfg2
  throwOnFailedWithLog ePool SqlDBConnectionFailedException "Failed to connect to SQL DB2."
```
