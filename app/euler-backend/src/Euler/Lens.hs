{-# language TemplateHaskell #-}
--{-# OPTIONS_GHC -ddump-splices #-}

module Euler.Lens where

import Euler.GenericLensTH

import Euler.Storage.Types.AuthenticationAccount as STAuthenticationAccount
import Euler.Storage.Types.Chargeback as STChargeback
import Euler.Storage.Types.Customer as STCustomer
import Euler.Storage.Types.Feature as STFeature
import Euler.Storage.Types.IngressRule as STIngressRule
import Euler.Storage.Types.Mandate as STMandate
import Euler.Storage.Types.MerchantAccount as STMerchantAccount
import Euler.Storage.Types.MerchantGatewayAccount as STMerchantGatewayAccount
import Euler.Storage.Types.MerchantIframePreferences as STMerchantIframePreferences
import Euler.Storage.Types.MerchantKey as STMerchantKey
import Euler.Storage.Types.OrderAddress as STOrderAddress
import Euler.Storage.Types.OrderMetadataV2 as STOrderMetadataV2
import Euler.Storage.Types.OrderReference as STOrderReference
import Euler.Storage.Types.PaymentGatewayResponse as STPaymentGatewayResponse
import Euler.Storage.Types.Promotion as STPromotion
import Euler.Storage.Types.Refund as STRefund
import Euler.Storage.Types.ResellerAccount as STResellerAccount
import Euler.Storage.Types.RiskManagementAccount as STRiskManagementAccount
import Euler.Storage.Types.SecondFactor as STSecondFactor
import Euler.Storage.Types.SecondFactorResponse as STSecondFactorResponse
import Euler.Storage.Types.ServiceConfiguration as STServiceConfiguration
import Euler.Storage.Types.TxnCardInfo as STTxnCardInfo
import Euler.Storage.Types.TxnDetail as STTxnDetail
import Euler.Storage.Types.TxnRiskCheck as STTxnRiskCheck

import Euler.Product.Domain.Card as PDCard
import Euler.Product.Domain.CardPayment as PDCardPayment
import Euler.Product.Domain.Chargeback as PDChargeback
import Euler.Product.Domain.Customer as PDCustomer
import Euler.Product.Domain.MerchantAccount as PDMerchantAccount
import Euler.Product.Domain.MerchantPaymentGatewayResponse as PDMerchantPaymentGatewayResponse
import Euler.Product.Domain.MerchantSecondFactorResponse as PDMerchantSecondFactorResponse
import Euler.Product.Domain.NBPayment as PDNBPayment
import Euler.Product.Domain.Order as PDOrder
import Euler.Product.Domain.OrderStatusResponse as PDOrderStatusResponse
import Euler.Product.Domain.Promotion as PDPromotion
import Euler.Product.Domain.TxnDetail as PDTxnDetail
import Euler.Product.Domain.TxnFlowInfo as PDTxnFlowInfo
import Euler.Product.Domain.TxnRiskCheck as PDTxnRiskCheck
import Euler.Product.Domain.Transaction as PDTransaction
import Euler.Product.Domain.UPIPayment as PDUPIPayment
import Euler.Product.Domain.WalletPayment as PDWalletPayment

import Euler.Product.Domain.Templates.Address as PDTAddress
import Euler.Product.Domain.Templates.Customer as PDTCustomer
import Euler.Product.Domain.Templates.Order as PDTOrder

import Euler.Common.Types.Customer as CTCustomer
import Euler.Common.Types.GatewayMetadata as CTGatewayMetadata
import Euler.Common.Types.Order as CTOrder
import Euler.Common.Types.Promotion as CTPromotion
import Euler.Common.Types.RMSIDResult as CTRMSIDResult
import Euler.Common.Types.ViesFlow as CTViesGatewayAuthReqParams

import Euler.API.Authentication as APIAuthentication
import Euler.API.Card as APICard
import Euler.API.CardPS as APICardPS
import Euler.API.Customer as APICustomer
import Euler.API.MerchantPaymentGatewayResponse as APIMerchantPGR
import Euler.API.Order as APIOrder
import Euler.API.Payment as APIPayment
import Euler.API.Refund as APIRefund
import Euler.API.Transaction as APITransaction

import Euler.Config.Config as CConfig
-- import Euler.Config.ServiceConfiguration as CServiceConfiguration

makeGenericLenses ''STAuthenticationAccount.AuthenticationAccountT
makeGenericLenses ''STChargeback.ChargebackT
makeGenericLenses ''STCustomer.CustomerT
makeGenericLenses ''STFeature.FeatureT
makeGenericLenses ''STIngressRule.IngressRuleT
makeGenericLenses ''STMandate.MandateT
makeGenericLenses ''STMerchantAccount.MerchantAccountT
makeGenericLenses ''STMerchantGatewayAccount.MerchantGatewayAccountT
makeGenericLenses ''STMerchantIframePreferences.MerchantIframePreferencesT
makeGenericLenses ''STMerchantKey.MerchantKeyT
makeGenericLenses ''STOrderAddress.APIBillingAddress
makeGenericLenses ''STOrderAddress.APIShippingAddress
makeGenericLenses ''STOrderAddress.OrderAddressT
makeGenericLenses ''STOrderMetadataV2.OrderMetadataV2T
makeGenericLenses ''STOrderReference.OrderReferenceT
makeGenericLenses ''STPaymentGatewayResponse.PaymentGatewayResponseT
makeGenericLenses ''STPromotion.PromotionT
makeGenericLenses ''STRefund.RefundT
makeGenericLenses ''STResellerAccount.ResellerAccountT
makeGenericLenses ''STRiskManagementAccount.RiskManagementAccountT
makeGenericLenses ''STSecondFactor.SecondFactorT
makeGenericLenses ''STSecondFactorResponse.SecondFactorResponseT
makeGenericLenses ''STServiceConfiguration.ServiceConfigurationT
makeGenericLenses ''STTxnCardInfo.TxnCardInfoT
makeGenericLenses ''STTxnDetail.TxnDetailT
makeGenericLenses ''STTxnRiskCheck.TxnRiskCheckT

-- makeGenericLenses ''PDCard.CardInfo
makeGenericLenses ''PDCard.Card
makeGenericLenses ''PDCard.StoredCard
makeGenericLenses ''PDCardPayment.ATMRedirectionPayment
makeGenericLenses ''PDCardPayment.ATMSeamlessPayment
makeGenericLenses ''PDCardPayment.CardPayment
makeGenericLenses ''PDCardPayment.CardPaymentType
makeGenericLenses ''PDChargeback.Chargeback
makeGenericLenses ''PDCustomer.CreateCustomer
makeGenericLenses ''PDCustomer.Customer
makeGenericLenses ''PDMerchantAccount.MerchantAccount
makeGenericLenses ''PDMerchantPaymentGatewayResponse.MerchantPaymentGatewayResponse
makeGenericLenses ''PDMerchantSecondFactorResponse.MerchantSecondFactorResponse
makeGenericLenses ''PDNBPayment.NBPayment
makeGenericLenses ''PDOrder.Order
makeGenericLenses ''PDOrder.OrderTokenResp
makeGenericLenses ''PDOrderStatusResponse.OrderStatusResponse
makeGenericLenses ''PDPromotion.Promotion
makeGenericLenses ''PDPromotion.PromotionActive
makeGenericLenses ''PDTransaction.Transaction
makeGenericLenses ''PDTxnDetail.TxnDetail
makeGenericLenses ''PDTxnFlowInfo.TxnFlowInfo
makeGenericLenses ''PDTxnRiskCheck.TxnRiskCheck
makeGenericLenses ''PDTxnRiskCheck.Risk
makeGenericLenses ''PDUPIPayment.UPIPayment
makeGenericLenses ''PDWalletPayment.DirectWalletPayment
makeGenericLenses ''PDWalletPayment.WalletPayment

makeGenericLenses ''PDTAddress.AddressHolderTemplate
makeGenericLenses ''PDTAddress.AddressTemplate
makeGenericLenses ''PDTCustomer.CustomerTemplate
makeGenericLenses ''PDTOrder.OrderCreateTemplate
makeGenericLenses ''PDTOrder.OrderUpdateTemplate

makeGenericLenses ''CTCustomer.Customer
makeGenericLenses ''CTGatewayMetadata.GatewayMetaEntry
makeGenericLenses ''CTOrder.ClientAuthTokenData
makeGenericLenses ''CTOrder.OrderTokenExpiryData
makeGenericLenses ''CTOrder.UDF
makeGenericLenses ''CTPromotion.Rules
makeGenericLenses ''CTRMSIDResult.RMSIDResult
makeGenericLenses ''CTRMSIDResult.Output
makeGenericLenses ''CTViesGatewayAuthReqParams.ViesGatewayAuthReqParams

makeGenericLenses ''APIAuthentication.Signed
makeGenericLenses ''APICard.AddCard
makeGenericLenses ''APICard.AddCardInputRequest
makeGenericLenses ''APICard.AddCardResponse
makeGenericLenses ''APICard.AddCardUsingToken
makeGenericLenses ''APICard.CardDeleteReq
makeGenericLenses ''APICard.CardDeleteResponseScheme
makeGenericLenses ''APICard.CardDetail
makeGenericLenses ''APICard.GetCardRequest
makeGenericLenses ''APICard.GetCardResponse
makeGenericLenses ''APICard.ListCardRequest
makeGenericLenses ''APICard.ListCardResponse
makeGenericLenses ''APICardPS.AddCardRequest
makeGenericLenses ''APICardPS.AddCardResp
makeGenericLenses ''APICardPS.AddCardResponse
makeGenericLenses ''APICardPS.CardData
makeGenericLenses ''APICardPS.CardDeleteReq
makeGenericLenses ''APICardPS.CardDeleteResponseScheme
makeGenericLenses ''APICardPS.GetCardRequest
makeGenericLenses ''APICardPS.GetCardResp
makeGenericLenses ''APICardPS.GetCardResponse
makeGenericLenses ''APICardPS.ListCardRequest
makeGenericLenses ''APICardPS.ListCardResp
makeGenericLenses ''APICardPS.ListCardResponse
makeGenericLenses ''APICustomer.CustomerReq
makeGenericLenses ''APICustomer.CustomerReqSignaturePayload
makeGenericLenses ''APICustomer.CustomerWithTokenData
makeGenericLenses ''APICustomer.TokenData
makeGenericLenses ''APIOrder.Card
makeGenericLenses ''APIOrder.Chargeback'
makeGenericLenses ''APIOrder.Mandate'
makeGenericLenses ''APIOrder.OrderStatusRequest
makeGenericLenses ''APIOrder.Promotion'
makeGenericLenses ''APIMerchantPGR.MerchantPaymentGatewayResponse
makeGenericLenses ''APIOrder.OrderCreateRequest
makeGenericLenses ''APIOrder.OrderCreateResponse
makeGenericLenses ''APIOrder.OrderStatusRequestLegacy
makeGenericLenses ''APIOrder.OrderStatusResponse
makeGenericLenses ''APIOrder.OrderTokenResp
makeGenericLenses ''APIOrder.OrderUpdateRequest
makeGenericLenses ''APIOrder.Paymentlinks
makeGenericLenses ''APIRefund.Refund'
makeGenericLenses ''APIOrder.Risk'
makeGenericLenses ''APIOrder.TxnDetail'
makeGenericLenses ''APIPayment.JsonError
makeGenericLenses ''APIPayment.PaymentStatus
makeGenericLenses ''APIPayment.PaymentStatusResponse
makeGenericLenses ''APITransaction.Authentication
makeGenericLenses ''APITransaction.PaymentAuth
makeGenericLenses ''APITransaction.Transaction
makeGenericLenses ''APITransaction.TransactionResponse

makeGenericLenses ''CConfig.Config
-- makeGenericLenses ''CServiceConfiguration.OtpAttempts
-- makeGenericLenses ''CServiceConfiguration.TokenCacheData
-- makeGenericLenses ''CServiceConfiguration.TokenExpiryData
