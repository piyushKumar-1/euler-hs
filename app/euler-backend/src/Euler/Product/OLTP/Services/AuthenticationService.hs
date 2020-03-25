{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.OLTP.Services.AuthenticationService where


import EulerHS.Prelude hiding (id)
import Euler.Lens
import EulerHS.Language

import qualified EulerHS.Extra.Validation as V

import Data.List (intersect)
import Servant.Server

import Euler.API.RouteParameters
import Euler.Common.Errors.PredefinedErrors
import Euler.Common.Types.Merchant
import qualified Euler.Config.Config                  as Config
import qualified Euler.Product.Domain.MerchantAccount as DM
import qualified Euler.Storage.Types.IngressRule as DBIR
import qualified Euler.Storage.Types.MerchantAccount as DBM
import qualified Euler.Storage.Validators.MerchantAccount as MV
import Euler.Storage.Types.IngressRule
import Euler.Storage.Types.MerchantKey
import Euler.Storage.Types.EulerDB
import Euler.Storage.DBConfig (eulerDB)

import qualified Prelude as P (show, id)
import qualified Data.ByteString.Base64 as BH
import qualified Data.Text as T

import qualified EulerHS.Language as L
import qualified Database.Beam as B
import Database.Beam ((==.), (&&.))


withMacc :: forall req resp . (RouteParameters -> req -> DM.MerchantAccount -> Flow  resp) -> RouteParameters -> req -> Flow resp
withMacc f rp req = do
  ma <- authenticateRequest rp
  f rp req ma



decodeB64Text :: Text -> Either Text Text
decodeB64Text k = toUTF8Text =<< (fromB64 $ encodeUtf8 k)
  where
    fromB64 x = bimap toText P.id $ BH.decode x
    toUTF8Text x = bimap (toText . P.show) P.id $ decodeUtf8' x

extractApiKey :: Text -> Either Text Text
extractApiKey k = T.takeWhile (/=':') <$> decodeB64Text (T.strip $ snd $ T.break (== ' ') k)



-- used "X-Auth-Scope", "Authorization" and "x-forwarded-for" headers
authenticateRequest :: RouteParameters -> Flow DM.MerchantAccount
authenticateRequest routeParams = case (lookupRP @Authorization routeParams) of
  Just apiKeyStr -> do
    case extractApiKey apiKeyStr of
      Left err -> do
        logError @String "API key extracting" $ "Can't extract API key from " <> apiKeyStr <> " error: " <> err
        throwException $ eulerAccessDenied "Invalid API key."
      Right apiKey' -> do
        mk <- do
          mMKey <- withDB eulerDB $ do
            let predicate MerchantKey{apiKey, status} = apiKey ==. B.just_ (B.val_ apiKey')
                 &&. status ==. B.just_ "ACTIVE"
            L.findRow
              $ B.select
              $ B.filter_ predicate
              $ B.all_ (merchant_key eulerDBSchema)
          case mMKey of
            Just mk -> pure mk
            Nothing -> throwException ecAccessDenied
           -- findOneWithErr ecDB (where_ := WHERE ["api_key" /\ String apiKeyStr, "status" /\ String "ACTIVE"]) ecAccessDenied
        merchantAccount' <- do
          mMAcc <- withDB eulerDB $ do
            let predicate DBM.MerchantAccount {id} = id ==. (B.val_ $  mk ^. _merchantAccountId)
            L.findRow
              $ B.select
              $ B.filter_ predicate
              $ B.all_ (merchant_account eulerDBSchema)
          case mMAcc of
            Just ma -> pure ma
            Nothing -> throwException ecAccessDenied
        -- findOneWithErr ecDB (where_ := WHERE ["id" /\ Int (fromMaybe 0 (merchantAccountId))]) ecAccessDenied
        merchantAccount <- pure $ merchantAccount' & _apiKey .~ (Just apiKey') -- setField @"apiKey" (Just apiKey') merchantAccount'
        --TODO: Need to validate the X-Auth-Scope with MerchantKey.scope -> If different, throw ecAccessDenied
        let eValidMerchant = MV.transSMaccToDomMacc merchantAccount
        case eValidMerchant of
          V.Failure e -> do
            logError @String "DB MerchantAccount Validation" $ show e
            throwException internalError
          V.Success validMAcc -> do
            authScope <- getAuthScope routeParams
            _ <- if authScope == MERCHANT then ipAddressFilters validMAcc (lookupRP @XForwardedFor routeParams) else pure True
            pure validMAcc
  Nothing -> do
    logError @String "authenticateRequestWithouthErr" "No authorization found in header"
    throwException $ eulerAccessDenied "API key not present in Authorization header"
 -- where getApiKeyFromHeader routeParams = do
 --         ((split (Pattern ":") <<< decodeBase64) <$> ((split (Pattern " ") <$> (lookup "Authorization" routeParams)) >>= last))
 --         >>= head


-- getAuthScope mKeyScope = pure $ fromMaybe MERCHANT mKeyScope
getAuthScope :: RouteParameters -> Flow KeyScope
getAuthScope routeParams = pure $ case (lookupRP @XAuthScope routeParams) of
  Just scope -> case scope of
    "MERCHANT" -> MERCHANT
    "CLIENT" -> CLIENT
    "DASHBOARD" -> DASHBOARD
    _ -> MERCHANT
  Nothing -> MERCHANT
-- ##############

--poolConfig :: PoolConfig
--poolConfig = T.PoolConfig
--  { stripes = 1
--  , keepAlive = 10
--  , resourcesPerStripe = 50
--  }
--
--mySQLCfg :: MySQLConfig
--mySQLCfg = MySQLConfig
--  { connectHost     = "localhost"
--  , connectPort     = 3306
--  , connectUser     = "cloud"
--  , connectPassword = "scape"
--  , connectDatabase = "jdb"
--  , connectOptions  = [CharsetName "utf8"]
--  , connectPath     = ""
--  , connectSSL      = Nothing
--  }
--
--mysqlDBC = mkMySQLPoolConfig "eulerMysqlDB" mySQLCfg poolConfig
--
--sqLiteConfig = mkSQLitePoolConfig "testDB" "/home/vg/work/haskell/euler-hs/app/euler-backend/test/Euler/TestData/test.db" poolConfig

--connMySQLorFail :: T.DBConfig beM -> Flow (T.SqlConn beM)
--connMySQLorFail cfg = L.initSqlDBConnection cfg >>= \case
--  Left e     -> error $ T.pack $  P.show e
--  Right conn -> pure conn

--connSQLITEorFail :: T.DBConfig beM -> Flow (T.SqlConn beM)
--connSQLITEorFail cfg = L.initSqlDBConnection cfg >>= \case
--  Left e     -> error $ T.pack $  P.show e
--  Right conn -> pure conn

--inSQLITEconn :: Flow ()
--inSQLITEconn= do
--  connection <- connSQLITEorFail $ sqLiteConfig
--  pure ()

--inConn :: Flow ()
--inConn = do
--  connection <- connMySQLorFail $ mysqlDBC
--  pure ()

--getConn :: DBConfig beM -> Flow (SqlConn beM)
--getConn cfg = do
--  conn <- getSqlDBConnection cfg
--  case conn of
--    Right c -> pure c
--    Left err -> do
--      logError "SqlDB" $ toText $ P.show err
--      throwException err500 {errBody = "getConn"}



--getMACC :: Int -> Flow DBM.MerchantAccount
--getMACC mid = do
--  inConn
--  conn <- getConn mysqlDBC
--  let (k :: Text) = "lllllllllllllllllllllllllll"
--  res <- L.runDB conn $ do
--    let predicate DBM.MerchantAccount {id} = id ==. B.just_ (B.val_ mid)
--    L.findRow
--      $ B.select
--      $ B.limit_ 1
--      $ B.filter_ predicate
--      $ B.all_ (merchant_account eulerDBSchema)
--  case res of
--    Right (Just ma) -> pure ma
--    Right (Nothing) -> do
--      runIO $ putTextLn "Nothing"
--      pure $ DBM.defaultMerchantAccount & _apiKey .~ (Just  k)
--    Left err -> do
--      runIO $ putStrLn  $ P.show err
--      pure $ DBM.defaultMerchantAccount & _apiKey .~ (Just k)

-- #############
--getAuthScope :: RouteParameters -> Flow KeyScope
--getAuthScope routeParams = pure $ case (lookup "X-Auth-Scope" routeParams) of
--  Just scope -> case scope of
--    "MERCHANT" -> MERCHANT
--    "CLIENT" -> CLIENT
--    "DASHBOARD" -> DASHBOARD
--    _ -> MERCHANT
--  Nothing -> MERCHANT

ipAddressFilters ::  DM.MerchantAccount -> Maybe Text -> Flow Bool
ipAddressFilters mAcc mForward =  do
  whitelistedIps <- getWhitelistedIps mAcc
  case whitelistedIps of
    Just ips -> do
      _ <- logInfo @String "ipAddressFilters" $  "IP whitelisting is enable for this merchant: " <> mAcc ^. _merchantId
      case (mForward) of
        Just forward -> do
          reqIPs <- pure $ T.strip <$> (filter (not . T.null) $ T.split (==',') forward)
          if (length $ intersect reqIPs ips) > 0
            then do
              _ <- logInfo @String "ipAddressFilters" $ "IP whitelist validated for this origin: " <> forward
              pure True
            else do
            _ <- logInfo @String "ipAddressFilters" $ "Rejecting request due to bad origin: " <> mconcat reqIPs
            throwException err403 {errBody = "Bad origin."}
        Nothing -> pure True
    Nothing -> pure True

getWhitelistedIps :: DM.MerchantAccount -> Flow (Maybe [Text])
getWhitelistedIps mAcc = do
  ipAddresses <- L.rGet Config.redis ("euler_ip_whitelist_for_" <> mAcc ^. _merchantId) -- getCachedValEC ("euler_ip_whitelist_for_" <> mId)
  case ipAddresses of
    Just ips -> pure (Just ips)
    Nothing -> do
      ir <- do
        withDB eulerDB $ do
          let predicate IngressRule {merchantAccountId} = merchantAccountId ==. B.val_ ( mAcc ^. _id)
          findRows
            $ B.select
            $ B.filter_ predicate
            $ B.all_ (ingress_rule eulerDBSchema)
      -- findAll ecDB (where_ := WHERE ["merchant_account_id" /\ Int (fromMaybe 0 $ mAcc ^. _id)] :: WHERE IngressRule)
      if (length ir) == 0 then pure Nothing else do
        ips <- pure $ (\r -> r ^. _ipAddress) <$> ir
        let fiveHours :: Integer = 5 * 60 * 60
        _   <- rSetex Config.redis ("euler_ip_whitelist_for_" <> mAcc ^. _merchantId) ips fiveHours-- setCacheEC (convertDuration $ Hours 5.0) ("euler_ip_whitelist_for_" <> mId) ips
        pure (Just ips)
