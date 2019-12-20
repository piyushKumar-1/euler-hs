module SQLDB.TestData.Scenarios.MySQL where

import           EulerHS.Prelude

import qualified EulerHS.Language as L
import qualified EulerHS.Types as T

import           SQLDB.TestData.Connections (connectOrFail)
import           SQLDB.TestData.Types

import           Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import qualified Database.Beam.MySQL as BM


-- Scenarios

uniqueConstraintViolationDbScript :: T.DBConfig BM.MySQLM -> L.Flow (T.DBResult ())
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


selectUnknownDbScript :: T.DBConfig BM.MySQLM -> L.Flow (T.DBResult (Maybe User))
selectUnknownDbScript cfg = do
  connection <- connectOrFail cfg

  L.runDB connection $ do
    let predicate User {..} = _userFirstName ==.  "Unknown"

    L.findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (_users eulerDb)


selectOneDbScript :: T.DBConfig BM.MySQLM -> L.Flow (T.DBResult (Maybe User))
selectOneDbScript cfg = do
  connection <- connectOrFail cfg
  L.runDB connection
    $ L.insertRows
    $ B.insert (_users eulerDb)
    $ B.insertExpressions (mkUser <$> susers)
  L.runDB connection $ do
    let predicate User {..} = _userFirstName ==.  "John"

    L.findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (_users eulerDb)


insertReturningScript :: T.DBConfig BM.MySQLM -> L.Flow (T.DBResult [User])
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


updateAndSelectDbScript :: T.DBConfig BM.MySQLM -> L.Flow (T.DBResult (Maybe User))
updateAndSelectDbScript cfg = do
  connection <- connectOrFail cfg

  L.runDB connection $ do
    let predicate1 User {..} = _userFirstName ==. "John"

    L.updateRows $ B.update (_users eulerDb)
      (\User {..} -> mconcat
        [ _userFirstName <-. "Leo"
        , _userLastName  <-. "San"
        ]
      )
      predicate1

    let predicate2 User {..} = _userFirstName ==. "Leo"
    L.findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate2
      $ B.all_ (_users eulerDb)
