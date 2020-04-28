# How to run tests

## How to run tests localy

1. add `127.0.0.1 localhost mysql redis` to `/etc/hosts/`

2. run `mysql -h mysql -u cloud -p`

3. Change root password to your own local at `Euler.Tests.Common` in `mySQLRootCfg`. Do not forget to revert before commit.

4. `stack test euler-backend`

## How to run tests within docker
