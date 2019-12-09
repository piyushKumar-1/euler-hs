{-# LANGUAGE UndecidableInstances #-}

module WebService.FlexCasing.Servant where

import           EulerHS.Prelude

import           Data.List(lookup)
import qualified Data.Text as Text
import           Data.String.Conversions (cs)
import           GHC.TypeLits (KnownSymbol, symbolVal, Symbol)
import           Network.HTTP.Types (queryToQueryText)
import           Network.Wai (Request, queryString)
import           Servant
--import           Servant (HasServer, FromHttpApiData, Optional, SBoolI, Strict, ServerT)
import           Servant.Client
import qualified Servant.Client.Core  as Core
import           Servant.API.Modifiers 
                (foldRequiredArgument, unfoldRequestArgument, 
                 RequestArgument, FoldRequired, FoldLenient, RequiredArgument)
import           Servant.Server.Internal (DelayedIO, withRequest, delayedFailFatal, addParameterCheck)


-- Fixed version of 'QueryParam' which uses queryParam, not rawQueryParam
-- this is allows WAI rewite middleware to work properly
data QueryParamC' (mods :: [*]) (sym :: Symbol) (a :: *)
    deriving Typeable

type QueryParamC = QueryParamC' '[Optional, Strict]

instance
  ( KnownSymbol sym, FromHttpApiData a, HasServer api context
  , SBoolI (FoldRequired mods), SBoolI (FoldLenient mods)
  )
  => HasServer (QueryParamC' mods sym a :> api) context where
------
  type ServerT (QueryParamC' mods sym a :> api) m =
    RequestArgument mods a -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =    
    let querytext = queryToQueryText . queryString 
        paramname = cs $ symbolVal (Proxy :: Proxy sym)        

        parseParam :: Request -> DelayedIO (RequestArgument mods a)
        parseParam req =
            unfoldRequestArgument (Proxy :: Proxy mods) errReq errSt mev
          where
            mev :: Maybe (Either Text.Text a)
            mev = fmap parseQueryParam 
                $ join 
                $ lookup paramname 
                $ querytext req

            errReq = delayedFailFatal err400
              { errBody = cs $ "Query parameter " <> paramname <> " is required"
              }

            errSt e = delayedFailFatal err400
              { errBody = cs $ "Error parsing query parameter "
                               <> paramname <> " failed: " <> e
              }

        delayed = addParameterCheck subserver . withRequest $ \req ->
                    parseParam req

    in route (Proxy :: Proxy api) context delayed

instance (KnownSymbol sym, ToHttpApiData a, HasClient m api, SBoolI (FoldRequired mods))
    => HasClient m (QueryParamC' mods sym a :> api) where

  type Client m (QueryParamC' mods sym a :> api) =
    RequiredArgument mods a -> Client m api

  -- if mparam = Nothing, we don't add it to the query string
  clientWithRoute pm Proxy req mparam =
    clientWithRoute pm (Proxy :: Proxy api) $ foldRequiredArgument
      (Proxy :: Proxy mods) add (maybe req add) mparam
    where
      add :: a -> Core.Request
      add param = Core.appendToQueryString pname (Just $ toQueryParam param) req

      pname :: Text.Text
      pname  = Text.pack $ symbolVal (Proxy :: Proxy sym)

  hoistClientMonad pm _ f cl = \arg ->
    hoistClientMonad pm (Proxy :: Proxy api) f (cl arg)