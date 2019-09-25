module Dashboard.Query.Validation
  ( validateQuery
  ) where

import Data.List (lookup)
import Dashboard.Query.Types
import Universum

-- FIXME: Eventually we want to return a single QueryValidationError which
-- will nest the Query as well details of the QueryValidationFailure
validateQuery :: QueryConfiguration -> Query -> Either [QueryValidationError] ()
-- FIXME: Don't do subsequent vavlidations if tablename is invalid
validateQuery qc q =
  case validateTable qc q of
    Left err -> Left [err]
    Right tc ->
      let
          validationResults =
            concat
              [ validateSelectFields tc q
              , validateFilters tc q
              , validateGroupByFields tc q
              , validateIntervalField tc q
              ]
      in if null validationResults
           then Right ()
           else Left validationResults

validateTable :: QueryConfiguration -> Query -> Either QueryValidationError TableConfiguration
validateTable (QueryConfiguration queryConfig) (Query _ tableName _ _ _) =
  case lookup tableName queryConfig of
    Nothing     -> Left $
                   QueryValidationError (TableError tableName) "Invalid table name"
    Just config -> Right config

-- QueryConfiguration has multiple tables, but a query works only with a single table
-- FIXME: We have an awful lot of pattern matches going on. Can we have record syntax for
-- Query.Types once they are fixed?
-- FIXME: Name validation functions more aptly once we extend them to do more
-- e.g. validSelections, validFilters
validateSelectFields :: TableConfiguration -> Query -> [QueryValidationError]
-- FIXME: We will have to extend selection for multiple fields\
validateSelectFields tableConfig (Query (Selection (_, Field field)) _ _ _ _) =
  validateFields
    tableConfig
    [field]
    SelectFieldError
    "Invalid select field name"
-- To match when Select field is ALL
validateSelectFields _ (Query (Selection (_, _)) _ _ _ _) = []

validateFilterFields :: TableConfiguration -> Query -> [QueryValidationError]
validateFilterFields tableConfig (Query _ _ _ (Filter filters) _) =
  validateFields
    tableConfig
    [x | (x, _, _) <- filters]
    FilterFieldError
    "Invalid filter field name"

validateFilters :: TableConfiguration -> Query -> [QueryValidationError]
validateFilters (TableConfiguration fieldData) (Query _ _ _ (Filter filters) _)=
    if null filters
    then
      []
    else
      lefts $ map ( validateFilter fieldData ) filters

validateFilter :: [(FieldName, FieldType)] -> (FieldName, FilterOp, Value) -> Either QueryValidationError ()
validateFilter fields (fn, op, value) =
  let result = find (\ (fieldname , fieldtype) -> fieldname == fn) fields
  in
    if result == Nothing
    then
       Left (QueryValidationError (FilterFieldError fn) "Invalid field name")
    else
      let Just (fieldname, fieldtype) = result
      in
         if ( check (value, fieldtype) )
         then
           Right ()
         else
           Left (QueryValidationError (FilterError fieldname) "Invalid operation, type mismatch")
 
check :: (Value , FieldType) -> Bool
check ((IntValue _),IntType) = True
check ((FloatValue _),FloatType) = True
check ((StringValue _),StringType) = True
check (_,_) = False

validateGroupByFields :: TableConfiguration -> Query ->  [QueryValidationError]
validateGroupByFields tableConfig (Query _ _ _ _ (GroupBy groupByFields)) =
  validateFields
    tableConfig
    groupByFields
    GroupByFieldError
    "Invalid group-by field name"

validateIntervalField :: TableConfiguration -> Query -> [QueryValidationError]
validateIntervalField tableConfig (Query _ _ (Interval _ _ _ intervalField) _ _) =
  validateFields 
    tableConfig
    [intervalField]
    IntervalFieldError
    "Invalid interval field name"

validateFields ::
     TableConfiguration
  -> [FieldName]
  -> (FieldName -> QueryErrorType)
  -> String
  -> [QueryValidationError]
validateFields (TableConfiguration tableConfig) fields err msg =
  let validFields = fst <$> tableConfig
  in lefts . map (validateField validFields err msg) $ fields

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
