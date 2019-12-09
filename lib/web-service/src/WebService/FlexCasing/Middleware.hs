module WebService.FlexCasing.Middleware 
(mkFlexCaseMiddleware)
where 

import           EulerHS.Prelude

import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Text as Text
import           Network.HTTP.Types 
                 (Query, QueryItem, RequestHeaders)
import           Network.Wai 
                 (Middleware, Request, requestHeaders, queryString)
import           Network.Wai.Middleware.Rewrite
                 (PathsAndQueries, rewritePureWithQueries)
import           Text.Casing 
                 (toQuietSnake, fromAny, toCamel, camel)


-- create middleware for adding "_case" query parameter containing the original case of the particular parameter
-- parameter's name could be passed in any case
type AnyCaseParameterName = String

mkFlexCaseMiddleware :: AnyCaseParameterName -> Middleware
mkFlexCaseMiddleware drivingParam = (mkOriginalCaseTagMiddleware drivingParam) -- add artificial `_case` query parameter
                                    . normalizeCase                            -- normalize (to camelCase) all query parameters

mkOriginalCaseTagMiddleware :: AnyCaseParameterName -> Middleware
mkOriginalCaseTagMiddleware p app req = app req'
    where 
        paramId = fromAny p
        camelParam = BSU.fromString $ toCamel paramId
        snakeParam = BSU.fromString $ toQuietSnake paramId -- TODO other cases?
        keys = fst $ unzip $ queryString req              
        caseParam casing = (BSU.fromString "casing", Just $ BSU.fromString casing)
        addCaseParameter :: Request -> QueryItem -> Request
        addCaseParameter r i = r { queryString = i : queryString req }
        req'
            | camelParam `elem` keys = addCaseParameter req (caseParam "camel")
            | snakeParam `elem` keys = addCaseParameter req (caseParam "snake")
            | otherwise = addCaseParameter req (caseParam "unspecified")

        
-- middleware to convert all query parameters into camelCase 
normalizeCase :: Middleware
normalizeCase = rewritePureWithQueries camelizeQuery'
    where 
        camelizeQuery' :: PathsAndQueries -> RequestHeaders -> PathsAndQueries
        camelizeQuery' (pieces, queries) _ = piecesConvert pieces queries
            where
                piecesConvert :: [Text.Text] -> Query -> PathsAndQueries
                piecesConvert ps qs = (ps, camelizeItem <$> qs)
                camelizeItem :: QueryItem -> QueryItem
                camelizeItem (k,v) = ((BSU.fromString . camel .  BSU.toString) k,v)
