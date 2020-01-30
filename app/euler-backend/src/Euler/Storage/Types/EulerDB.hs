{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Euler.Storage.Types.EulerDB
  ( eulerDBSchema
  , EulerDb(..)
  ) where

import EulerHS.Prelude hiding (id)

import qualified Database.Beam as B

import qualified Euler.Storage.Types.AuthenticationAccount     as SAuthenticationAccount
import qualified Euler.Storage.Types.Chargeback                as SChargeback
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
import qualified Euler.Storage.Types.PaymentGatewayResponse    as SPaymentGatewayResp
import qualified Euler.Storage.Types.Promotions                as SPromotions
import qualified Euler.Storage.Types.Refund                    as SRefund
import qualified Euler.Storage.Types.ResellerAccount           as SResellerAccount
import qualified Euler.Storage.Types.RiskManagementAccount     as SRiskManagementAccount
import qualified Euler.Storage.Types.SecondFactor              as SSecondFactor
import qualified Euler.Storage.Types.SecondFactorResponse      as SSecondFactorResponse
import qualified Euler.Storage.Types.ServiceConfiguration      as SServiceConfiguration
import qualified Euler.Storage.Types.TxnCardInfo               as STxnCardInfo
import qualified Euler.Storage.Types.TxnDetail                 as STxnDetail

import qualified Euler.Storage.Types.SqliteTest as SSQLite


data EulerDb f = EulerDb
  { authentication_account :: f (B.TableEntity SAuthenticationAccount.AuthenticationAccountT)
  , chargeback :: f (B.TableEntity SChargeback.ChargebackT)
  , customer :: f (B.TableEntity SCustomer.CustomerT)
  , feature :: f (B.TableEntity SFeature.FeatureT)
  , ingress_rule :: f (B.TableEntity SIngressRule.IngressRuleT)
  , mandate :: f (B.TableEntity SMandate.MandateT)
  , merchant_account :: f (B.TableEntity SMerchantAccount.MerchantAccountT)
  , merchant_iframe_preferences :: f (B.TableEntity SMerchIframePrefs.MerchantIframePreferencesT)
  , merchant_key     :: f (B.TableEntity SMerchantKey.MerchantKeyT)
  , order_address :: f (B.TableEntity SOrderAddress.OrderAddressT)
  , order_metadata_v2 :: f (B.TableEntity SOrderMetadataV2.OrderMetadataV2T)
  , order_reference :: f (B.TableEntity SOrderReference.OrderReferenceT)
  , payment_gateway_response :: f (B.TableEntity SPaymentGatewayResp.PaymentGatewayResponseT)
  , promotions :: f (B.TableEntity SPromotions.PromotionsT)
  , refund :: f (B.TableEntity SRefund.RefundT)
  , reseller_account :: f (B.TableEntity SResellerAccount.ResellerAccountT)
  , risk_management_account :: f (B.TableEntity SRiskManagementAccount.RiskManagementAccountT)
  , second_factor :: f (B.TableEntity SSecondFactor.SecondFactorT)
  , second_factor_response :: f (B.TableEntity SSecondFactorResponse.SecondFactorResponseT)
  , service_configuration :: f (B.TableEntity SServiceConfiguration.ServiceConfigurationT)
  , txn_card_info ::  f (B.TableEntity STxnCardInfo.TxnCardInfoT)
  , txn_detail ::  f (B.TableEntity STxnDetail.TxnDetailT)
  , test_table :: f (B.TableEntity SSQLite.TestTableT)

  } deriving (Generic, B.Database be)

eulerDb :: B.DatabaseSettings be EulerDb
eulerDb = B.defaultDbSettings `B.withDbModification`
  B.dbModification
    { authentication_account = SAuthenticationAccount.authenticationAccountEMod
    , chargeback = SChargeback.chargebackEMod
    , customer = SCustomer.customerEMod
    , feature = SFeature.featureEMod
    , ingress_rule = SIngressRule.ingressRuleEMod
    , mandate = SMandate.mandateEMod
    , merchant_account = SMerchantAccount.merchantAccountEMod
    , merchant_iframe_preferences = SMerchIframePrefs.merchantIframePreferencesEMod
    , merchant_key = SMerchantKey.merchantKeyEMod
    , order_address = SOrderAddress.orderAddressEMod
    , order_metadata_v2 = SOrderMetadataV2.orderMetadataV2EMod
    , order_reference = SOrderReference.orderReferenceEMod
    , payment_gateway_response = SPaymentGatewayResp.paymentGatewayResponseEMod
    , promotions = SPromotions.promotionsEMod
    , refund = SRefund.refundEMod
    , reseller_account = SResellerAccount.resellerAccountEMod
    , risk_management_account = SRiskManagementAccount.riskManagementAccountEMod
    , second_factor = SSecondFactor.secondFactorEMod
    , second_factor_response = SSecondFactorResponse.secondFactorResponseEMod
    , service_configuration = SServiceConfiguration.serviceConfigurationEMod
    , txn_detail = STxnDetail.txnDetailEMod
    , txn_card_info = STxnCardInfo.txnCardInfoEMod
    , test_table = SSQLite.testTableEMod
    }

eulerDBSchema :: B.DatabaseSettings be EulerDb
eulerDBSchema = eulerDb
