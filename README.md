# euler-hs

## console
Pre-requisites:- 
- Stack, install from https://docs.haskellstack.org/en/stable/README/
- `godel-big-q` service account credentials, this is used by console for data retrieval

The following steps are needed for running the Console backend
1. Export `GOOGLE_APPLICATION_CREDENTIALS=/path/to/godel-big-q.json`
2. Export `CONSOLE_CONFIG=$PWD/app/console/config/console.dhall.sample`
3. Run with `stack run console`

Tests can be run with `stack test console`.


## euler-hs & euler-backend

Library ***euler-hs*** - backend DSL on free monads.

Application ***euler-backend*** - haskell re-implementation of euler-ps API, based on euler-hs.

### Installation

Pre-requisites:

**Stack**, install from [https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/)

Or

**Nix package manager**: [A minimal guide to getting started with the Nix package manager on any Linux or OSX machine.](https://chris-martin.org/2016/installing-nix-package-manager) (current nix-channel is nixos-19.09)

and set in your ~/.cabal/config

write-ghc-environment-files: never

nix: True

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

**Run:**

- `stack run euler-backend`

#### Nix

**Build**

From root project dir run

- For euler-hs lib  `nix-build -A euler-hs`
- For euler-backend app  `nix-build -A euler-backend`

    resulting binary should be in `./result/bin/`
