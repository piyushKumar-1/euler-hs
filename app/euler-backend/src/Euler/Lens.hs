{-# language TemplateHaskell #-}
--{-# OPTIONS_GHC -ddump-splices #-}

-- ToDo :: Move to Storage.Types.Lens

module Euler.Lens where

import EulerHS.Prelude
import Data.Generics.Product.Fields
import Euler.GenericLensTH

import Euler.Storage.Types.AuthenticationAccount as AuthenticationAccount
import Euler.Storage.Types.Chargeback as Chargeback
import Euler.Storage.Types.Customer as Customer
import Euler.Storage.Types.Feature as Feature
import Euler.Storage.Types.IngressRule as IngressRule
import Euler.Storage.Types.Mandate as Mandate
import Euler.Storage.Types.MerchantAccount as MerchantAccount
import Euler.Storage.Types.MerchantIframePreferences as MerchantIframePreferences
import Euler.Storage.Types.MerchantKey as MerchantKey
import Euler.Storage.Types.OrderAddress as OrderAddress
import Euler.Storage.Types.OrderMetadataV2 as OrderMetadataV2
import Euler.Storage.Types.OrderReference as OrderReference
import Euler.Storage.Types.PaymentGatewayResponse as PaymentGatewayResponse
import Euler.Storage.Types.Promotions as Promotions
import Euler.Storage.Types.Refund as Refund
import Euler.Storage.Types.ResellerAccount as ResellerAccount
import Euler.Storage.Types.RiskManagementAccount as RiskManagementAccount
import Euler.Storage.Types.SecondFactor as SecondFactor
import Euler.Storage.Types.SecondFactorResponse as SecondFactorResponse
import Euler.Storage.Types.ServiceConfiguration as ServiceConfiguration
import Euler.Storage.Types.TxnCardInfo as TxnCardInfo
import Euler.Storage.Types.TxnDetail as TxnDetail
import Euler.Storage.Types.TxnRiskCheck as TxnRiskCheck

makeGenericLenses ''AuthenticationAccount.AuthenticationAccountT
makeGenericLenses ''Chargeback.ChargebackT
makeGenericLenses ''Customer.CustomerT
makeGenericLenses ''Feature.FeatureT
makeGenericLenses ''IngressRule.IngressRuleT
makeGenericLenses ''Mandate.MandateT
makeGenericLenses ''MerchantAccount.MerchantAccountT
makeGenericLenses ''MerchantIframePreferences.MerchantIframePreferencesT
makeGenericLenses ''MerchantKey.MerchantKeyT
makeGenericLenses ''OrderAddress.APIBillingAddress
makeGenericLenses ''OrderAddress.APIShippingAddress
makeGenericLenses ''OrderAddress.OrderAddressT
makeGenericLenses ''OrderMetadataV2.OrderMetadataV2T
makeGenericLenses ''OrderReference.OrderReferenceT
makeGenericLenses ''PaymentGatewayResponse.PaymentGatewayResponseT
makeGenericLenses ''Promotions.PromotionsT
makeGenericLenses ''Refund.RefundT
makeGenericLenses ''ResellerAccount.ResellerAccountT
makeGenericLenses ''RiskManagementAccount.RiskManagementAccountT
makeGenericLenses ''SecondFactor.SecondFactorT
makeGenericLenses ''SecondFactorResponse.SecondFactorResponseT
makeGenericLenses ''ServiceConfiguration.ServiceConfigurationT
makeGenericLenses ''TxnCardInfo.TxnCardInfoT
makeGenericLenses ''TxnDetail.TxnDetailT
makeGenericLenses ''TxnRiskCheck.TxnRiskCheckT


-- Lens for types defined outside of Storage.Types
-- ToDo :: Move it somewhere

_billingAddr :: HasField' "billingAddr" s a => Lens s s a a
_billingAddr = field' @"billingAddr"

_billingAddrHolder :: HasField' "billingAddrHolder" s a => Lens s s a a
_billingAddrHolder = field' @"billingAddrHolder"

_email :: HasField' "email" s a => Lens s s a a
_email = field' @"email"

_host :: HasField' "host" s a => Lens s s a a
_host = field' @"host"

_mandate :: HasField' "mandate" s a => Lens s s a a
_mandate = field' @"mandate"

_orderTokenNeeded :: HasField' "orderTokenNeeded" s a => Lens s s a a
_orderTokenNeeded = field' @"orderTokenNeeded"

_protocol :: HasField' "protocol" s a => Lens s s a a
_protocol = field' @"protocol"

_shippingAddr :: HasField' "shippingAddr" s a => Lens s s a a
_shippingAddr = field' @"shippingAddr"

_shippingAddrHolder :: HasField' "shippingAddrHolder" s a => Lens s s a a
_shippingAddrHolder = field' @"shippingAddrHolder"
