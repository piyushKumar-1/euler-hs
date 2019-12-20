module SQLDB.TestData.Scenarios.Postgres where

import           EulerHS.Prelude

import qualified EulerHS.Language as L
import qualified EulerHS.Types as T

import           SQLDB.TestData.Connections (connectOrFail)
import           SQLDB.TestData.Types

import           Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import qualified Database.Beam.Postgres as BP


-- Scenarios

uniqueConstraintViolationDbScript :: T.DBConfig BP.Pg -> L.Flow (T.DBResult ())
uniqueConstraintViolationDbScript cfg = do
  connection <- connectOrFail cfg

  L.runDB connection
    $ L.insertRows
    $ B.insert (_users eulerDb)
    $ B.insertValues [User 1 "Eve" "Beon"]

  L.runDB connection
    $ L.insertRows
    $ B.insert (_users eulerDb)
    $ B.insertValues [User 1 "Eve" "Beon"]

selectUnknownDbScript :: T.DBConfig BP.Pg -> L.Flow (T.DBResult (Maybe User))
selectUnknownDbScript cfg = do
  connection <- connectOrFail cfg

  L.runDB connection $ do
    let predicate User {..} = _userFirstName ==. B.val_ "Unknown"

    L.findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (_users eulerDb)

selectOneDbScript :: T.DBConfig BP.Pg-> L.Flow (T.DBResult (Maybe User))
selectOneDbScript cfg = do
  connection <- connectOrFail cfg

  L.runDB connection $ do
    let predicate User {..} = _userFirstName ==. B.val_ "John"

    L.findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (_users eulerDb)

updateAndSelectDbScript :: T.DBConfig BP.Pg -> L.Flow (T.DBResult (Maybe User))
updateAndSelectDbScript cfg = do
  connection <- connectOrFail cfg

  L.runDB connection $ do
    let predicate1 User {..} = _userFirstName ==. B.val_ "John"

    L.updateRows $ B.update (_users eulerDb)
      (\User {..} -> mconcat
        [ _userFirstName <-. B.val_ "Leo"
        , _userLastName  <-. B.val_ "San"
        ]
      )
      predicate1

    let predicate2 User {..} = _userFirstName ==. B.val_ "Leo"
    L.findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate2
      $ B.all_ (_users eulerDb)

insertReturningScript :: T.DBConfig BP.Pg -> L.Flow (T.DBResult [User])
insertReturningScript cfg = do
  connection <- connectOrFail cfg
  L.runDB connection
    $ L.insertRowsReturningList
    $ B.insert (_users eulerDb)
    $ B.insertExpressions
      [ User B.default_
        ( B.val_ "John" )
        ( B.val_ "Doe"  )
      , User B.default_
        ( B.val_ "Doe"  )
        ( B.val_ "John" )
      ]
