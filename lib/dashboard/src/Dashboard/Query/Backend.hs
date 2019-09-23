module Dashboard.Query.Backend where

import Universum

import Dashboard.Query.Types

class QueryBackend qb where
  runQuery :: qb -> QueryConfiguration -> Query -> IO QueryResult
