{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Euler.Storage.Types.EulerDB
  ( eulerDBSchema
  , EulerDb(..)
  ) where

import EulerHS.Prelude hiding (id)

import qualified Database.Beam as B

import qualified Euler.Storage.Types.Customer                  as SCustomer
import qualified Euler.Storage.Types.Feature                   as SFeature
import qualified Euler.Storage.Types.IngressRule               as SIngressRule
import qualified Euler.Storage.Types.Mandate                   as SMandate
import qualified Euler.Storage.Types.MerchantAccount           as SMerchantAccount
import qualified Euler.Storage.Types.MerchantIframePreferences as SMerchIframePrefs
import qualified Euler.Storage.Types.MerchantKey               as SMerchantKey
import qualified Euler.Storage.Types.OrderAddress              as SOrderAddress
import qualified Euler.Storage.Types.OrderMetadataV2           as SOrderMetadataV2
import qualified Euler.Storage.Types.OrderReference            as SOrderReference
import qualified Euler.Storage.Types.Promotions                as SPromotions
import qualified Euler.Storage.Types.ResellerAccount           as SResellerAccount
import qualified Euler.Storage.Types.ServiceConfiguration      as SServiceConfiguration
import qualified Euler.Storage.Types.SqliteTest as SSQLite


data EulerDb f = EulerDb
  { customer :: f (B.TableEntity SCustomer.CustomerT)
  , feature :: f (B.TableEntity SFeature.FeatureT)
  , ingress_rule :: f (B.TableEntity SIngressRule.IngressRuleT)
  , mandate :: f (B.TableEntity SMandate.MandateT)
  , merchant_account :: f (B.TableEntity SMerchantAccount.MerchantAccountT)
  , merchant_iframe_preferences :: f (B.TableEntity SMerchIframePrefs.MerchantIframePreferencesT)
  , merchant_key     :: f (B.TableEntity SMerchantKey.MerchantKeyT)
  , order_address :: f (B.TableEntity SOrderAddress.OrderAddressT)
  , order_metadata_v2 :: f (B.TableEntity SOrderMetadataV2.OrderMetadataV2T)
  , order_reference :: f (B.TableEntity SOrderReference.OrderReferenceT)
  , promotions :: f (B.TableEntity SPromotions.PromotionsT)
  , reseller_account :: f (B.TableEntity SResellerAccount.ResellerAccountT)
  , service_configuration :: f (B.TableEntity SServiceConfiguration.ServiceConfigurationT)
  , test_table :: f (B.TableEntity SSQLite.TestTableT)
  } deriving (Generic, B.Database be)

eulerDb :: B.DatabaseSettings be EulerDb
eulerDb = B.defaultDbSettings `B.withDbModification`
  B.dbModification
    { customer = SCustomer.customerEMod
    , feature = SFeature.featureEMod
    , ingress_rule = SIngressRule.ingressRuleEMod
    , mandate = SMandate.mandateEMod
    , merchant_account = SMerchantAccount.merchantAccountEMod
    , merchant_iframe_preferences = SMerchIframePrefs.merchantIframePreferencesEMod
    , merchant_key = SMerchantKey.merchantKeyEMod
    , order_address = SOrderAddress.orderAddressEMod
    , order_metadata_v2 = SOrderMetadataV2.orderMetadataV2EMod
    , order_reference = SOrderReference.orderReferenceEMod
    , promotions = SPromotions.promotionsEMod
    , reseller_account = SResellerAccount.resellerAccountEMod
    , service_configuration = SServiceConfiguration.serviceConfigurationEMod
    , test_table = SSQLite.testTableEMod
    }

eulerDBSchema :: B.DatabaseSettings be EulerDb
eulerDBSchema = eulerDb