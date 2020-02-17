# Changelog for euler-hs


## [1.3.0.0] - 2020-02-17
- Euler-hs: (breaking changes) Options reworked.
  Interface modified (Typeable instance required now).
  Fixed a bug with identical encoding of different keys.
- Euler-hs: added GHC options: Wcompat Widentities fhide-source-paths
- Euler-hs: added wrappers for kvdb actions
- Euler-hs: added callServantApi request | response logging
- Euler-hs: changed `Serializable` instances for ByteStrings
- Euler-hs: fixed recording forked flow with exception
- Euler-hs: fixed throwException method entry record/replay
- Improvements in business logic and tests
- console: removed from repo
- dashboard: removed from repo

## [1.2.0.0] - 2019-12-20

### Added

- beam-mysql support of insertReturning
- beam-mysql BIT / TEXT problem solved
- Transactions for SQL DB subsystem made right
- `getOrInit` methods for SQL DB & KV DB
- Improvements in business logic and tests
- Flex casing

## [1.1.0.0] - 2019-12-16

### Added

- InitKVDB method
- Shared connections
- `getOrInitKVDB` method

## [1.0.0.0] - 2019-12-09

### Added

- Shared connections `getSqlDBConnection` and  `getOrInitSqlConn` for SQL DB

## [0.9.0.0] - 2019-12-02

### 2019-12

- Added branching policy guide

### 2019-11

- Euler-hs: Added metrics base
- Euler-hs: Added strictness annotations to reduce memory consumption under heavy logger load
- Euler-hs: Tune sqlite to wait for resource in case of concurrent access
- Euler-hs: Added Load tester base app
- Euler-hs: Agnostic ART record|replay Optimized
- Euler-hs: Added ART player/recorder & art.sh script
- CI: Implement continuous deployment of console
- Added CODESTYLE guide
- console: Add Docker image building stage to Nix
- Euler-hs: Added redis tests mocked with ART
- Euler-hs: Added descriptions. Imports refactored
- Euler-backend: Validation refactored and cleaned up
- Euler-hs: Rollback to the BeamRunner instances
- Euler-hs: Fixed bugs with DB initialization

### 2019-10

- Nix: Added deriviations for broken xml libs
- Euler-backend: Added App, Server & Some Types
- Euler-hs: Add art in KVDB layer
- Euler-hs: Fixed fork recording race
- Euler-hs: Added own Serializable class
- Euler-hs: Added JSONEx as union of Serializable ans To/FromJSON
- Euler-hs: Art tests improved
- Euler-hs: Added art entries and art support for new methods.
- Euler-hs: Added connection pool
- Euler-hs: Added deinitSqlDBConn method
- CI: build: Enable tests for euler-hs
- dashboard: Add tests for SQL generation
- Euler-hs: Fork Flow logic fixes and improvements
- Euler-hs: Add art rec/rep for InitSqlDBConnection method
- Euler-hs: Added ART
- Euler-hs: Added mysql tests
- Euler-hs: Postgres support added
- console: Add setup instructions for running console
- console: Make test more robust and not dependent on external data
- dashboard: Allow providing BigQuery credentials to the backend
- Console: Conditionally parse date time fields as strings
- Euler-hs: MySQL support added
- Euler-hs: Introducing kvdb mock
- CI: Add a Jenkinsfile to enable CI
- Nix: Add basic infrastructure for using Nix
- Euler-hs: SQL DB support reworked and improved
- dashboard: Deal with null values from the database
- Euler-hs: Added KVDB transactions support
- dashboard: Reduce dependencies to the minimal set
- Console: Add a way to allow cross-origin requests for development
- Euler-backend: Added fail test cases
- Euler-backend: Added infix versions of transformations
- Euler-backend: Added transformation & validation for txns

### 2019-09

- Euler-hs: Added kvdb methods wrappers
- Euler-hs: Added KVDB sub-language
- Euler-hs: Test DB clearing added
- Euler-hs: Tests for SqlDB added
- Euler-hs: Run db in transaction
- Euler-hs: Added custom ConnectInfo (isomorphic to Beam.Postgres.ConnectInfo)
- Euler-hs: SQL DB Support Beam
- Euler-hs: Added SQL DB Support
- Console: Add a way to generate PureScript types for API
- Euler-hs: Added mocked values in test interpreter
- Console: Implement a BigQuery backend to run queries
- dashboard: Supports multiple select fields and validation for same
- dashboard: Validate filters against their field types
- dashboard: Add Query -> SQL generation for BigQuery
- dashboard: Add a field name for Query intervals
- dashboard: Add a mechanism to validate queries
- Console: Add dashboard query API
- Euler-hs: Added additional language methods `GenerateGUID`, `runSysCmd`, `forkFlow`, `throwException`
- Euler-hs: Test app implemented
- Euler-hs: Added logger logic, language and interpreter
- dashboard: Add basic query and query result types
- Euler-hs: Added `runIO` and get/setOptions tests
- Euler-hs: ServantClient types added
- Euler-hs: `runIO`, `getOption`, `setOption` methods added
- Credit platform: Added basic app facilities
- Credit platform: Types for FINVU APIs added
- Euler-hs: `interpretFlowMethodL` and `runFlowMethodL` methods added
- Euler-hs: Added basic project layout, test app layout, initial CallAPI facilities, sample API
