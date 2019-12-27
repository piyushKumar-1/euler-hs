module Dashboard.Query.Process
  ( processResult
  ) where

import Data.List (lookup)
import Data.Maybe (fromJust)
import Universum

import Dashboard.Query.Config as QT
import Dashboard.Query.Types as QT

processResult :: QT.QueryConfiguration -> QT.Query -> QT.QueryResult -> QT.QueryResult
processResult queryConfig query (QueryResult rows) =
  let (Selection sel) = QT.selection query
      selectionFields = map (maybeSelFieldName . snd) sel
      fieldConfigs    = map (\(QT.FieldConfiguration name _ config) -> (name, config))
                        . QT.fields
                        . fromJust
                        . QT.lookupTable (QT.table query)
                        $ queryConfig
      newRows         = map (processRow selectionFields fieldConfigs) rows
  in
    QueryResult newRows

  where
    processRow fieldNames fieldConfigs (QueryResultRow start end cols) =
      QueryResultRow start end
      . map (processColumn fieldConfigs)
      . zip fieldNames
      $ cols

    -- This was a SELECT FOO(*) field, just return the value
    processColumn _ (Nothing, fieldValue) = fieldValue
    -- This is a string value field, try to transform
    processColumn fieldConfigs (Just fieldName, StringValue s) =
      let maybeValueMap = lookup fieldName fieldConfigs
      in
        StringValue
        . fromMaybe s
        $ lookup s =<< maybeValueMap
    -- This is not a string value field, just return the value
    processColumn _ (_ , fieldValue) = fieldValue

    maybeSelFieldName QT.All          = Nothing
    maybeSelFieldName (QT.Field name) = Just name
