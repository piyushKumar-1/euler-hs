module Euler.Product.OLTP.Services.AuthTokenService
  ( authWithToken
  ) where

import           EulerHS.Prelude                      hiding (id)

import           EulerHS.Language                     as L

-- EHS: is it ok to have it here?
import           Servant.Server (errBody, err500, err400, err403)

import qualified Data.Text                            as Text
import qualified Euler.API.RouteParameters            as RP
import qualified Euler.Common.Errors.PredefinedErrors as Errs
import           Euler.Common.Types.Order             (ClientAuthTokenData(..))
import qualified Euler.KVDB.Redis                     as R
import           Euler.Lens
import qualified Euler.Product.Domain.Customer        as DMC
import qualified Euler.Product.Domain.Order           as DMO
import qualified Euler.Product.Domain.MerchantAccount as DM
import qualified Euler.Storage.Repository             as Rep

type AuthToken = Text

authWithToken
  :: forall req resp.
  (RP.RouteParameters -> req -> DM.MerchantAccount -> Flow resp)
  -> RP.RouteParameters
  -> req
  -> Flow resp

authWithToken f rp req = do
  merchant <- authenticate rp
  f rp req merchant

authenticate :: RP.RouteParameters -> Flow DM.MerchantAccount
authenticate rps = do
  let mbToken = RP.lookupRP @RP.ClientAuthToken rps
  case mbToken of
    Nothing -> do
      logError "AuthTokenService" "No auth token found in header"
      throwException Errs.authTokenNotFound
    Just token -> do
      mbTokenData :: Maybe ClientAuthTokenData <- R.rGet token
      case mbTokenData of
        Nothing ->  do
          logError "AuthTokenService" "No auth token data found in cache"
          throwException Errs.authTokenNotFound
        Just tokenData -> do
          --_ <- pure () -- updateAuthTokenUsage authToken authTokenData
          mbMA <- getMerchantAccountForAuthToken tokenData
          case mbMA of
            Nothing -> do
              logError "AuthTokenService" "Can't load merchant for token from cache"
              throwException Errs.noMerchantAccountForToken
            Just ma -> do
              return ma

-- EHS: where is the best place to have it?
resourceTypeOrder :: IsString s => s
resourceTypeOrder = "ORDER"

resourceTypeCustomer :: IsString s => s
resourceTypeCustomer = "CUSTOMER"


getMerchantAccountForAuthToken :: ClientAuthTokenData -> Flow (Maybe DM.MerchantAccount)
getMerchantAccountForAuthToken ClientAuthTokenData {..} = do
  case resourceType of
    "ORDER" -> do
       -- DB.findOneWithErr ecDB (where_ := WHERE ["id" /\ Int (parseInt otokenData.resourceId)]) ecAccessDenied
      let orderId = resourceId
      mbOrder <- Rep.loadOrderById $ read $ Text.unpack orderId
      case mbOrder of
        Nothing -> do
          -- EHS: rework
          logError ("AuthTokenService" :: Text) ("No auth token data found in cache")
          throwException Errs.authTokenNotFound
        Just order -> do
          Rep.loadMerchantById $ read $ Text.unpack $ order ^. _merchantId


    -- EHS: rework
    "CUSTOMER" -> do
      DMC.Customer {..} <- pure DMC.defaultCustomer -- DB.findOneWithErr ecDB (where_ := WHERE ["id" /\ String otokenData.resourceId]) ecAccessDenied
      pure $ Just DM.defaultMerchantAccount -- DB.findOneWithErr ecDB (where_ := WHERE ["id" /\ Int merchantAccountId]) ecAccessDenied

    -- EHS: rework
    _                          -> throwException err403
