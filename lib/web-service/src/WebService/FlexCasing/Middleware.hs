-----------------------------------------------------------------------------
-- | Module : WebService.FlexCasing.Middleware 
--
-- Support for multi-casing requests, i.e. ones which could use different 
-- casing for GET query parameters. It's expected that all parameters go
-- in the same case style, but actually it's not mandatory.  
-----------------------------------------------------------------------------
module WebService.FlexCasing.Middleware 
  (
    -- param constants
    casingParam
    -- middleware builder
  , mkFlexCaseMiddleware
  ) where 

import           EulerHS.Prelude hiding (fromString, toString)

import           Data.ByteString.UTF8 
                (fromString, toString)
import qualified Data.Text as Text
import           Network.HTTP.Types 
                (Query, QueryItem, RequestHeaders)
import           Network.Wai 
                (Middleware, Request, queryString)
import           Network.Wai.Middleware.Rewrite
                (PathsAndQueries, rewritePureWithQueries)
import           Text.Casing 
                (toQuietSnake, fromAny, toCamel, camel)
import           WebService.FlexCasing.Types                

-----------------------------------------------------------------------------


-- | The name of an artificial query parameter to use with servant API types 
-- combinator QueryParam.
-- The original request casing, determined by the casing of driving parameter is stored here.
-- It can be used later to form a response using the same casing.                    
casingParam :: String                
casingParam = "casing"

-----------------------------------------------------------------------------

type DrivingParamName = String

-- | Makes ready-to-use middleware to process requests with flexible casing.
-- All parameter names will be rewrtitten to camelCase and the original casing
-- of driving parameter will be added in `casingParam` parameter.
mkFlexCaseMiddleware :: DrivingParamName -> Middleware
mkFlexCaseMiddleware drivingParam = 
    (mkOriginalCaseTagMiddleware drivingParam) -- add artificial casing query parameter
    . normalizeCase                            -- normalize (to camelCase) all query parameters

-- create middleware for adding artificial "casing" query parameter
-- it's name casing id used to detect which casing should be used for response
mkOriginalCaseTagMiddleware :: DrivingParamName -> Middleware
mkOriginalCaseTagMiddleware p app req = 
    app req'
  where 
    paramId = fromAny p
    camelParam = fromString $ toCamel paramId
    snakeParam = fromString $ toQuietSnake paramId
    -- here one can add more casing if needed
    
    params = fst $ unzip $ queryString req              
    
    cParam origCase = (fromString casingParam, Just $ fromString origCase)
    
    addCaseParameter :: Request -> QueryItem -> Request
    addCaseParameter r i = r { queryString = i : queryString req }
    
    req'
      | camelParam `elem` params = addCaseParameter req (cParam camelCase)
      | snakeParam `elem` params = addCaseParameter req (cParam snakeCase)
      | otherwise                = addCaseParameter req (cParam unsupportedCase)

-----------------------------------------------------------------------------

-- middleware to convert all query parameters into camelCase (kind of default one)
normalizeCase :: Middleware
normalizeCase = 
    rewritePureWithQueries camelizeQuery
  where 
    camelizeQuery :: PathsAndQueries -> RequestHeaders -> PathsAndQueries
    camelizeQuery (pieces, queries) _ = 
        piecesConvert pieces queries
      where
        piecesConvert :: [Text.Text] -> Query -> PathsAndQueries
        piecesConvert ps qs = (ps, camelizeItem <$> qs)
        camelizeItem :: QueryItem -> QueryItem
        camelizeItem (k,v) = ((fromString . camel . toString) k,v)


