module Dashboard.Query.Validation
  ( validateQuery
  ) where

import Data.List (lookup)
import Dashboard.Query.Types
import Universum hiding (All)

validateQuery :: QueryConfiguration -> Query -> Either [QueryValidationError] ()
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
                   QueryValidationError (TableNotFound tableName) "Invalid table name"
    Just config -> Right config

validateSelectFields :: TableConfiguration -> Query -> [QueryValidationError]
validateSelectFields tableConfig (Query (Selection selections) _ _ _ _) =
    lefts $ validateSelectField tableConfig <$> selections

validateSelectField :: TableConfiguration->(SelectOp,SelectField)->Either QueryValidationError ()
validateSelectField (TableConfiguration tableConfig) (op, Field fieldname) =
  case lookup fieldname tableConfig of
    Nothing -> Left $ QueryValidationError (SelectFieldNotFound fieldname) "Invalid field name"
    Just fieldtype -> validateSelectOperation op fieldtype
validateSelectField _ (_, All) =
  Right ()

validateSelectOperation :: SelectOp -> FieldType -> Either QueryValidationError ()
validateSelectOperation op fieldtype =
  case (op,fieldtype) of
    (SUM, StringType) -> Left $ QueryValidationError (SelectOperationNotValid op) "Invalid Operation"
    (AVG, StringType) -> Left $ QueryValidationError (SelectOperationNotValid op) "Invalid Operation"
    (_, _)            -> Right ()

validateFilters :: TableConfiguration -> Query -> [QueryValidationError]
validateFilters (TableConfiguration fieldData) (Query _ _ _ (Filter filters) _) =
  lefts $ map (validateFilter fieldData) filters
  where
    validateFilter fields (fieldName, _, value) =
      case lookup fieldName fields of
        Nothing        -> Left $ QueryValidationError (FilterFieldNotFound fieldName) "Invalid field name"
        Just fieldType -> verifyFieldTypes value fieldType fieldName

    verifyFieldTypes value fieldType fieldName =
      case (value, fieldType) of
        (IntValue _, IntType)       -> Right ()
        (FloatValue _, FloatType)   -> Right ()
        (StringValue _, StringType) -> Right ()
        _                           -> Left (QueryValidationError (FilterTypeMismatch fieldName) "Field type mismatch")

validateGroupByFields :: TableConfiguration -> Query ->  [QueryValidationError]
validateGroupByFields tableConfig (Query _ _ _ _ (GroupBy groupByFields)) =
  validateFields
    tableConfig
    groupByFields
    GroupByFieldNotFound
    "Invalid group-by field name"

validateIntervalField :: TableConfiguration -> Query -> [QueryValidationError]
validateIntervalField tableConfig (Query _ _ (Interval _ _ _ intervalField) _ _) =
  validateFields
    tableConfig
    [intervalField]
    IntervalFieldNotFound
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