# Changelog for euler-hs

## [2.9.4.1] - 2021-11-23

* Add `withJSONBody` to make building of JSON HTTP requests easier

## [2.9.4.0] - 2021-11-23

* Moved all non Euler-specific and non `Flow`-dependent code into brand-new `juspay-exra` library and made it a dependency
* Move common Euler-specific utility code from `euler-webservice` and `euler-db` into `Extra` folder, including `EulerHS.Extra.Aeson`, `EulerHS.Extra.Combinators`, `EulerHS.Extra.Time`)

## [2.9.3.1] - 2021-11-09

* Optimize exception passing to db connection pools

## [2.9.3.0] - 2021-10-18

* Treat `Text` and `String` tags in logging entries without show'ing them

## [2.9.2.0] - 2021-09-22

* Add logger functions `logM`  , `log` , `logV` , `logInfoM` , `logInfoV`
  , `logErrorM` , `logErrorV` , `logDebugM` , `logDebugV` , `logWarningM`
  ,  ability to log jsonable values as message.
* Adjust log messages from `callHTTP` and `callAPI` methods

## [2.9.1.2] - 2021-09-22

* Use `Data.ByteString.Builder` instead of deprecated `Data.ByteString.Lazy.Builder`
  in `EulerHS.HttpAPI`

## [2.9.1.1] - 2021-09-03

* Keep previous (as in 2.9.0.2) callHTTPWithCert interface

## [2.9.1.0] - 2021-08-12

* HTTP ManagerSettings builders
* Dynamic HTTP managers
* Unify callHTTP* and callAPI* actions

## [2.9.0.2] - 2021-08-10

* Add `withRunFlow`

## [2.9.0.1] - 2021-08-10

* Fix adding mask config to `LoggerRuntime`

## [2.9.0.0] - 2021-08-04

* Added `generic-lens`, reverted `withField` method to old declaration, changed New `withField` to `withField'`, added `checkFailedWithLog` from webservice.

## [2.8.0.0] - 2021-08-03

* Update `beam-mysql` to 1.3.0.1 version
* Update Servant-* deps and record-dot-preprocessor version
* Refactor and fix stack config

## [2.7.0.0] - 2021-07-27

* Expand `HTTPMethod` with `Trace`, `Connect`, `Options`, `Patch` methods

## [2.6.0.2] - 2021-07-27

* Add `rSetexBulk`, `rSetexBulkB`

## [2.6.0.1] - 2021-07-07

* Remove `MonadFlow m` constraint from `withModifiedRuntime` method

## [2.6.0.0] - 2021-07-06

* Expand `CallHTTP` with `Maybe ManagerSelector`
* Add `callHTTPWithManager`

## [2.5.0.5] - 2021-07-06

* Add dump erroneous value on decoding failure in `rGet`

## [2.5.0.4] - 2021-07-06

* Fix resource cleanup on exception inside `withResource`

## [2.5.0.3] - 2021-07-06

* Add a test-case for callHTTPWithCert

## [2.5.0.2] - 2021-07-06

* Adjust `runRedis` errror message when connection not found

## [2.5.0.1] - 2021-06-21

* Fix uncaught exceptions from runClientM in callAPI

## [2.5.0.0] - 2021-04-29

* Remove `Mocked` constructors everywhere.

## [2.4.0.0] - 2021-04-28

* Remove `Serializable` module.

## [2.3.0.0] - 2021-04-26

* Remove all `lens` and `generic-lens` usage and exports.

## [2.2.0.1] - 2021-04-26

* Add a range of additional functions to `EulerHS.Extra.Language`, originally
  from `euler-webservice`.

## [2.2.0.0] - 2021-04-23

* Major version bump for `beam-mysql`.

## [2.1.1.0] - 2021-04-20

* Use RDP plugin

## [2.1.0.0] - 2021-04-12

* `ManagerSelector` is now a newtype.
* Add `CertificateRegistrationError` data type.
* Add 'withSelfSignedFlowRuntime` function for registering self-signed
  certificates in a `FlowRuntime`.

## [2.0.4.4] - 2021-04-13

* Rename `changeLoggerContext` to `withLoggerContext`
* Add `updateLoggerContext`

## [2.0.4.3] - 2021-04-12

* Add `changeLoggerContext` to EulerHS.Extra.Language

## [2.0.4.2] - 2021-04-07

* Fixed a bug with MVar blocking when disposing `Async` logger.

## [2.0.4.1] - 2021-03-03

* Remove waste entries.
* Add `rSetOpts` and `rSetOptsB`

## [2.0.4] - 2021-02-25

* Add `ShouldLogSQL(SafelyOmitSqlLogs, UnsafeLogSQL_DO_NOT_USE_IN_PRODUCTION)` to `LoggerConfig`

## [2.0.3.2] - 2021-02-12

* Add `rawRequest` to KVDB API, plus implementation for KVDB interpreter.

## [2.0.3.1] - 2021-01-28

* Add `withModifiedRuntime` method to `MonadFlow`, plus implementation for
  `Flow` interpreter.

## [2.0.3.0] - 2021-01-20

* Allow passing of a character set to a MySQL connection, along with a type
  specifying the character sets supported currently

## [1.10.0.0] - 2020-06-16
* Euler-HS 1.10.0.0: a significant update with new features and fixes.
  - New authentication service in euler-api-order
  - Rework of repository structure, each library has its own repo now.
  - Added an `eulerBuild` --  a collection of nix functions for building and developing euler-based projects.
  - See `eulerBuild` documentation [BUILD.md](BUILD.md) for upgrade and usage instructions.
  - Compatibility with GHC 8.8.
  - Updated stack resolver to 15.15.
  - Updated pinned nixpkgs to `nixos-unstable` at `0f5ce2fac0c726036ca69a5524c59a49e2973dd4` (~ 18.05.2020)
  - Added CI builds.
  - See notes on how to upgrade in [UPGRADE.md](UPGRADE.md).
  - New policy for releases -- create a new tag with the same name for all core libraries,
    i.e. `euler-hs`, `euler-db`, `euler-types`, `euler-webservice` have a tag `EulerHS-1.10.0.0` for this release.
### `euler-hs`
  - Added `run[Update/Delete]ReturningList` for Postgres.
  - Added `delOption`.
  - Added `runUntracedIO` for reading/writing sensitive data.
  - Log format temporarily changed to partly mimic euler-ps.
  - Added untyped HTTP calls to `Flow`.
  - Lots of various fixes not listed here.
  - Added `insertRowReturningMySql` function which does not use temporary tables internally.
  - Beware of `SqlBool` vs `Bool` when writing `beam` queries, and other gotchas: [see BEAM_NOTES.md](BEAM_NOTES.md),
    read this if you use database at all via `euler-hs`.
    Some of this can have a critical effect on performance, especially on MySQL.
### `euler-db`
  - Contains DB-facing types.
  - https://bitbucket.org/juspay/euler-db
### `euler-types`
  - Contains domain types.
  - https://bitbucket.org/juspay/euler-types
### `euler-webservice`
  - Contains utilities for writing servant-based web services.
  - https://bitbucket.org/juspay/euler-webservice

## [2.0.0.0] - 2020-07-01

* Use `beam-mysql` based on `mysql-haskell` instead of `mysql`

## [1.9.5.0] - 2020-04-13
* Euler-HS 1.9.5.0: fixes
  - Async logger mem consumption fixed.
  - Integration tests with MySQL disabled
  - Improved documentation [see README.md](README.md)
* Euler-backend 0.9.0.0:
  - Order Status API
  - Integration tests disabled

## [1.9.0.0] - 2020-04-13
* Euler-HS 1.9.0.0: a significant update with new features and fixes.
  - RunSafeFlow method: ability to catch exceptions thrown by throwException
    (breaking change for recordings)
  - Exceptions in forked flows are now handled (breaking change)
  - Options made a bit more effective
  - Raw SQL now is printed into recordings (breaking change for recordings)
* Euler-backend 0.9.0.0:
  - Configs updated

## [1.8.0.0] - 2020-04-03
* Euler-HS 1.8.0.0: new features and fixes.
  - Redis cluster support (switched to other hedis fork).
  - Framework and Logger performance tuned.
* Euler-backend 0.8.0.0:
  - DB model and storage types separated into own repo.

## [1.7.0.0] - 2020-03-30
* Euler-HS 1.7.0.0: new features.
  - Granular DB errors added (breaking change)
  - Test framework added

## [1.6.0.0] - 2020-03-27
* Euler-HS 1.6.0.0: a significant update with new features and fixes.
  - beam-mysql updated: temporary tables fix, autocommit fix, bytestrings encoding fix
  - MySQL transactions bug fixed
  - New feature: awaiting for results from forked flows added
  - runIO' with description added
  - KV DB hardcorded DB name fixed (breaking change)
  - More documentation on SQL subsystem usage added (see README.md)

* Euler-Backend: small updates
  - AWS-KMS encryption functionality added
  - Environment variables support added

## [1.5.0.0] - 2020-03-13
* Euler-Backend 0.5.1.0:
  - Added MerchantGatewayAccount Storage type. MerchantGatewayAccount added to DB scheme
  - Added EulerAccountDetails with HasSqlValueSyntax and FromBackEndRow instances
  - stack.yaml fixed to avoid multiple rebuilds of euler-backend code

## [1.4.0.0] - 2020-03-12
* Euler-hs 1.4.0.0:
  - Performance analysed and tuned
    N.B. Async logger has a lazy mem leak. Will be fixed in the next version.
    Use sync logger for now.
  - Pub-Sub mechanism added
  - Beam-MySQL updated (support of the `Day` type added)
  - Small fixes and additions
* Euler-Backend 0.5.0.0:
  - DB types added.
  - Order Create completely implemented
  - Integration tests & ART tests added
  - Customer & Card API types, domain types and validators added

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
