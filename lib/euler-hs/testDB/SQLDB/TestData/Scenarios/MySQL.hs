module SQLDB.TestData.Scenarios.MySQL where

import           EulerHS.Prelude

import qualified EulerHS.Language as L
import qualified EulerHS.Types as T

import           SQLDB.TestData.Connections (connectOrFail)
import           SQLDB.TestData.Types

import           Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import qualified Database.Beam.MySQL as BM


uniqueConstraintViolationDbScript :: T.DBConfig BM.MySQLM -> L.Flow (T.DBResult ())
uniqueConstraintViolationDbScript dbcfg = do
    econn <- L.getSqlDBConnection dbcfg

    flip (either $ error "Unable to get connection") econn $ \conn -> do
      L.runDB conn $
        L.insertRows
          $ B.insert (_users eulerDb)
          $ B.insertValues [User 2 "Rosa" "Rosa"]

      L.runDB conn $
        L.insertRows
          $ B.insert (_users eulerDb)
          $ B.insertValues [User 2 "Rosa" "Rosa"]


uniqueConstraintViolationEveDbScript :: T.DBConfig BM.MySQLM -> L.Flow (T.DBResult ())
uniqueConstraintViolationEveDbScript dbcfg = do
    econn <- L.getSqlDBConnection dbcfg

    flip (either $ error "Unable to get connection") econn $ \conn -> do
      L.runDB conn $ do
        L.insertRows
          $ B.insert (_users eulerDb)
          $ B.insertValues [User 3 "Eve" "Beon"]

        L.insertRows
          $ B.insert (_users eulerDb)
          $ B.insertValues [User 3 "Eve" "Beon"]

uniqueConstraintViolationMickeyDbScript :: T.DBConfig BM.MySQLM -> L.Flow (T.DBResult ())
uniqueConstraintViolationMickeyDbScript dbcfg = do
    econn <- L.getSqlDBConnection dbcfg

    flip (either $ error "Unable to get connection") econn $ \conn -> do
      L.runDB conn $
        L.insertRows
          $ B.insert (_users eulerDb)
          $ B.insertValues [User 4 "Mickey" "Mouse"]

      L.runDB conn $
        L.insertRows
          $ B.insert (_users eulerDb)
          $ B.insertValues [User 4 "Mickey" "Mouse"]


data MyException = ThisException | ThatException
    deriving Show

instance Exception MyException

throwExceptionFlowScript :: T.DBConfig BM.MySQLM -> L.Flow (T.DBResult ())
throwExceptionFlowScript dbcfg = do
    econn <- L.getSqlDBConnection dbcfg

    flip (either $ error "Unable to get connection") econn $ \conn -> do
      L.runDB conn $ do
        L.insertRows
          $ B.insert (_users eulerDb)
          $ B.insertValues [User 6 "Billy" "Evil"]

        -- Could we emulate exceptin here?
        --fail "Bang!"


        L.insertRows
          $ B.insert (_users eulerDb)
          $ B.insertValues [User 7 "Billy" "Bang"]


selectUnknownDbScript :: T.DBConfig BM.MySQLM -> L.Flow (T.DBResult (Maybe User))
selectUnknownDbScript dbcfg = do
    econn <- L.getSqlDBConnection dbcfg

    flip (either $ error "Unable to get connection") econn $ \conn ->
      L.runDB conn $ do
        let predicate User {..} = _userFirstName ==. "Unknown"
        L.findRow
          $ B.select
          $ B.limit_ 1
          $ B.filter_ predicate
          $ B.all_ (_users eulerDb)

selectAbsentRowEveDbScript :: T.DBConfig BM.MySQLM -> L.Flow (T.DBResult (Maybe User))
selectAbsentRowEveDbScript dbcfg = do
    econn <- L.getSqlDBConnection dbcfg

    flip (either $ error "Unable to get connection") econn $ \conn ->
      L.runDB conn $ do
        let predicate User {..} = _userId ==. 2
        L.findRow
          $ B.select
          $ B.limit_ 1
          $ B.filter_ predicate
          $ B.all_ (_users eulerDb)

selectAbsentRowMickeyDbScript :: T.DBConfig BM.MySQLM -> L.Flow (T.DBResult (Maybe User))
selectAbsentRowMickeyDbScript dbcfg = do
    econn <- L.getSqlDBConnection dbcfg

    flip (either $ error "Unable to get connection") econn $ \conn ->
      L.runDB conn $ do
        let predicate User {..} = _userId ==. 4
        L.findRow
          $ B.select
          $ B.limit_ 1
          $ B.filter_ predicate
          $ B.all_ (_users eulerDb)

selectAbsentRowBillyDbScript :: T.DBConfig BM.MySQLM -> L.Flow (T.DBResult (Maybe User))
selectAbsentRowBillyDbScript dbcfg = do
    econn <- L.getSqlDBConnection dbcfg

    flip (either $ error "Unable to get connection") econn $ \conn ->
      L.runDB conn $ do
        let predicate User {..} = _userId ==. 6
        L.findRow
          $ B.select
          $ B.limit_ 1
          $ B.filter_ predicate
          $ B.all_ (_users eulerDb)


selectOneDbScript :: T.DBConfig BM.MySQLM -> L.Flow (T.DBResult (Maybe User))
selectOneDbScript dbcfg = do
    econn <- L.getSqlDBConnection dbcfg

    flip (either $ error "Unable to get connection") econn $ \conn -> do
      L.runDB conn
        $ L.insertRows
        $ B.insert (_users eulerDb)
        $ B.insertExpressions (mkUser <$> susers)

      L.runDB conn $ do
        let predicate User {..} = _userFirstName ==. "John"

        L.findRow
          $ B.select
          $ B.limit_ 1
          $ B.filter_ predicate
          $ B.all_ (_users eulerDb)


insertReturningScript :: T.DBConfig BM.MySQLM -> L.Flow (T.DBResult [User])
insertReturningScript dbcfg = do
    econn <- L.getSqlDBConnection dbcfg

    flip (either $ error "Unable to get connection") econn $ \conn ->
      L.runDB conn
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
updateAndSelectDbScript dbcfg = do
    econn <- L.getSqlDBConnection dbcfg

    flip (either $ error "Unable to get connection") econn $ \conn ->
      L.runDB conn $ do
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
