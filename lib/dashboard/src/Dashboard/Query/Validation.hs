module Dashboard.Query.Validation
  ( printQueryValidationError
  , validateQuery
  ) where

import Dashboard.Query.Config
import Dashboard.Query.Types
import Universum hiding (All, Sum)

printQueryValidationError :: QueryValidationError -> String
printQueryValidationError (QueryValidationError qve _) =
  "Validation failed: " ++ case qve of
    (TableNotFound name)         -> "Table not found: " ++ name
    (SelectFieldNotFound name)   -> "Select field not found " ++ name
    (FilterFieldNotFound name)   -> "Filter field not found: " ++ name
    (GroupByFieldNotFound name)  -> "Group by field not found" ++ name
    (FilterTypeMismatch name)    -> "Filter type mismatch on: " ++ name
    (IntervalFieldNotFound name) -> "Interval field not found: " ++ name
    (SelectOperationNotValid op) -> "Select operation invalid: " ++ show op
    SelectEmpty                  -> "No fields selected"

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
validateTable queryConfig (Query _ tableName _ _ _) =
  case lookupTable tableName queryConfig of
    Nothing     -> Left $
                   QueryValidationError (TableNotFound tableName) "Invalid table name"
    Just config -> Right config

validateSelectFields :: TableConfiguration -> Query -> [QueryValidationError]
validateSelectFields tableConfig (Query (Selection selections) _ _ _ _) =
  if null selections
     then [QueryValidationError SelectEmpty "Invalid selection"]
     else lefts $ validateSelectField tableConfig <$> selections

validateSelectField :: TableConfiguration -> (Maybe SelectOp, SelectField) -> Either QueryValidationError ()
validateSelectField tableConfig (op, Field fieldname) =
  case lookupField fieldname tableConfig of
    Nothing -> Left $ QueryValidationError (SelectFieldNotFound fieldname) "Invalid field name"
    Just fieldtype -> validateSelectOperation op fieldtype
validateSelectField _ (_, All) =
  Right ()

validateSelectOperation :: Maybe SelectOp -> FieldType -> Either QueryValidationError ()
validateSelectOperation op fieldtype =
  case (op, fieldtype) of
    (Nothing, _)               -> Right ()
    (Just Sum, StringType)     -> Left $ QueryValidationError (SelectOperationNotValid Sum) "Invalid Operation"
    (Just Average, StringType) -> Left $ QueryValidationError (SelectOperationNotValid Average) "Invalid Operation"
    (_, _)                     -> Right ()

validateFilters :: TableConfiguration -> Query -> [QueryValidationError]
validateFilters tableConf (Query _ _ _ (Filter filters) _) =
  lefts $ map validateFilter filters
  where
    validateFilter (fieldName, _, value) =
      case lookupField fieldName tableConf of
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
validateFields tableConfig queryFields err msg =
  let validFields = fieldName <$> fields tableConfig
  in lefts . map (validateField validFields err msg) $ queryFields

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
