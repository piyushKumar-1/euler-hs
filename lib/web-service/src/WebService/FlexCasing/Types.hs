-----------------------------------------------------------------------------
-- | Module : WebService.FlexCasing.Types 
--
-- Basic types and constants for multi casing requests support.
-----------------------------------------------------------------------------

module WebService.FlexCasing.Types
  (
    -- case styles constants
    camelCase
  , snakeCase
  , unsupportedCase
    -- classtype for response
  , FlexCasingResponse
  , casing
    -- default aeson options for response serialization 
  , flexCasingOptions
) where

import           EulerHS.Prelude hiding (pack)

import           Data.Aeson                           
                (Options, defaultOptions, fieldLabelModifier)
import           Data.Text                            
                (Text, pack)
import qualified Data.Aeson.Casing as Casing
                (camelCase, snakeCase)

camelCase :: String                
camelCase = "camel"

snakeCase :: String
snakeCase = "snake"

unsupportedCase :: String
unsupportedCase = "unsupported"

-----------------------------------------------------------------------------
                                
class FlexCasingResponse a where
  casing :: a -> Text

-----------------------------------------------------------------------------

flexCasingOptions :: FlexCasingResponse r => r -> Options
flexCasingOptions v = defaultOptions { fieldLabelModifier = convertFun v }  

convertFun :: (FlexCasingResponse r) => r -> (String -> String)
convertFun p  
  | c == camelCase' = Casing.camelCase
  | c == snakeCase' = Casing.snakeCase
  | otherwise = undefined
  where 
    c = casing p
    camelCase' = pack camelCase
    snakeCase' = pack snakeCase

