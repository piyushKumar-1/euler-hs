module Dashboard.Query.Validation
  ( validateQuery
  ) where

import Dashboard.Query.Types
import Universum

-- FIXME: Eventually we want to return a single QueryValidationError which
-- will nest the Query as well details of the QueryValidationFailure
validateQuery :: QueryConfiguration -> Query -> Either [QueryValidationError] ()
-- FIXME: Don't do subsequent vavlidations if tablename is invalid
validateQuery qc q =
  case validateTable qc q of
    Just err -> Left [err]
    _ ->
      let validationResults =
            concat
              [ validateSelectFields qc q
              , validateFilterFields qc q
              , validateGroupByFields qc q
              ]
      in if null validationResults
           then Right ()
           else Left validationResults

validateTable :: QueryConfiguration -> Query -> Maybe QueryValidationError
validateTable (QueryConfiguration queryConfig) (Query _ tableName _ _ _) =
  let validTableNames = [x | (Table x _) <- queryConfig]
  in if tableName `notElem` validTableNames
       then Just $
            QueryValidationError (TableError tableName) "Invalid table name"
       else Nothing

-- QueryConfiguration has multiple tables, but a query works only with a single table
-- FIXME: We have an awful lot of pattern matches going on. Can we have record syntax for
-- Query.Types once they are fixed?
-- FIXME: Name validation functions more aptly once we extend them to do more
-- e.g. validSelections, validFilters
validateSelectFields :: QueryConfiguration -> Query -> [QueryValidationError]
-- FIXME: We will have to extend selection for multiple fields\
validateSelectFields queryConfig (Query (Selection (_, Field field)) table _ _ _) =
  validateFields
    queryConfig
    table
    [field]
    SelectFieldError
    "Invalid select field name"
-- To match when Select field is ALL
validateSelectFields _ (Query (Selection (_, _)) _ _ _ _) = []

validateFilterFields :: QueryConfiguration -> Query -> [QueryValidationError]
validateFilterFields queryConfig (Query _ table _ (Filter filters) _) =
  validateFields
    queryConfig
    table
    [x | (x, _, _) <- filters]
    FilterFieldError
    "Invalid filter field name"

validateGroupByFields :: QueryConfiguration -> Query -> [QueryValidationError]
validateGroupByFields queryConfig (Query _ table _ _ (GroupBy groupByFields)) =
  validateFields
    queryConfig
    table
    groupByFields
    GroupByFieldError
    "Invalid group-by field name"

validateFields ::
     QueryConfiguration
  -> TableName
  -> [FieldName]
  -> (FieldName -> QueryErrorType)
  -> String
  -> [QueryValidationError]
validateFields (QueryConfiguration queryConfig) table fields err msg =
  let (Just tableConfig) = find (\(Table tn _) -> tn == table) queryConfig
      (Table _ validFields) = tableConfig
  in if null fields
       then []
       else lefts $ map (validateField validFields err msg) fields

-- Note: - Right now this is just an elem and we can return name of the field that
-- failed. But doing it with a Field error since eventually we'll support functions
-- passed down by the caller. Eventually we'll pass on functions to construct the
-- specific error and corresponding predicate
validateField ::
     [FieldName]
  -> (FieldName -> QueryErrorType)
  -> String
  -> FieldName
  -> Either QueryValidationError ()
validateField validFields err msg field =
  if field `elem` validFields
    then Right ()
    else Left (QueryValidationError (err field) msg)
