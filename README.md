# euler-hs

## console
Pre-requisites:- 
- Stack, install from https://docs.haskellstack.org/en/stable/README/
- `godel-big-q` service account credentials, this is used by console for data retrieval

The following steps are needed for running the Console backend
1. Export `GOOGLE_APPLICATION_CREDENTIALS=/path/to/godel-big-q.json`
2. Export `CONSOLE_ALLOW_CORS=1`
3. Run with `stack run console`

Tests can be run with `stack test console`.


