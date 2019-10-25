{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Euler.API.Validators.Transaction where

import EulerHS.Prelude
import EulerHS.Extra.Validation

import Data.Generics.Product.Fields
import Data.Functor.Alt
import qualified Data.Text as T
import qualified Euler.Product.Domain.Transaction as DT
import Euler.Product.Domain.CardPayment
import Euler.Product.Domain.NBPayment
import Euler.Product.Domain.WalletPayment
import Euler.Product.Domain.UPIPayment as UPI
import Euler.Product.Domain.Types
import Euler.Product.Domain.PaymentMethod.Card
import Euler.Product.Domain.PaymentMethod.ATMCard
import Euler.Product.Domain.PaymentMethod.NB
import Euler.Product.Domain.PaymentMethod.Wallet
import Euler.Product.Domain.PaymentMethod.WalletDirect
import Euler.Product.Domain.PaymentMethod.UPI
import qualified Euler.API.Transaction as AT
import qualified Euler.API.Types as AT
import Euler.API.Types

instance Transform AT.Transaction DT.Transaction where
  transform apiTxn = DT.Transaction
      <$> (OrderId <$> ((takeField @"order_id" apiTxn) <!*> textNotEmpty)) -- :: OrderId                -- ^
      <*> (MerchantId <$> ((takeField @"merchant_id" apiTxn) <!*> textNotEmpty)) -- :: MerchantId
      <*> (transform apiTxn) -- :: TransactionType
      <*> (takeField @"redirect_after_payment" apiTxn) <!*> alwaysValid -- :: Bool
      <*> (takeField @"format" apiTxn) <!*> textNotEmpty -- :: Text


instance Transform AT.Transaction DT.TransactionType where
  transform apiTxn = case ( getField @"payment_method_type" apiTxn
                          , getField @"auth_type" apiTxn
                          , getField @"direct_wallet_token" apiTxn) of
    (AT.CARD, Just ATMPIN, _) -> DT.ATMRedirectionTransaction <$> transform apiTxn
                            <!>  DT.ATMSeamlessTransaction <$> transform apiTxn
    (AT.CARD,   _        , _) -> DT.CardTransaction <$> transform apiTxn
    (AT.NB  ,   _        , _) -> DT.NBTransaction <$> transform apiTxn
    (AT.WALLET, _  , Nothing) -> DT.WalletTransaction <$> transform apiTxn
    (AT.WALLET, _  ,  Just _) -> DT.WalletDirectDebitTransaction <$> transform apiTxn
    (AT.UPI,    _        , _) -> DT.UPITransaction <$> transform apiTxn

instance Transform AT.Transaction ATMSeamlessPayment where
  transform apiTxn = ATMSeamlessPayment
    <$> (decodeTo @CardPaymentMethod $ fromMaybe' @"payment_method" apiTxn)
        <!*> alwaysValid  -- payment_method :: CPM.CardPaymentMethod
    <*> (transform apiTxn) -- card_payment_type      :: CardPaymentType
    <*> (fromMaybe' @"auth_type" apiTxn) <!*> isAtmCardAuthType

instance Transform AT.Transaction ATMRedirectionPayment where
  transform apiTxn = ATMRedirectionPayment
    <$> (decodeTo @ATMCardPaymentMethod $ fromMaybe' @"payment_method" apiTxn)
        <!*> alwaysValid
    <*> (fromMaybe' @"auth_type" apiTxn) <!*> isAtmCardAuthType

instance Transform AT.Transaction UPIPayment where
  transform apiTxn
    =   UPICollect
        <$> (decodeTo @UPIPaymentMethod $ fromMaybe' @"payment_method" apiTxn)
            <!*> alwaysValid
        <*> (decodeTo @UPITxnType $ fromMaybe' @"txn_type" apiTxn)
            <!*> isUPICollectTxnType
        <*> (fromMaybe' @"upi_vpa" apiTxn) <!*> textNotEmpty
    <!> UPIPay
        <$> (decodeTo @UPIPaymentMethod $ fromMaybe' @"payment_method" apiTxn)
            <!*> alwaysValid
        <*> (decodeTo @UPITxnType $ fromMaybe' @"txn_type" apiTxn)
            <!*> isUPIPayTxnType

instance Transform AT.Transaction WalletPayment where
  transform apiTxn = WalletPayment
    <$> (decodeTo @WalletPaymentMethod $ fromMaybe' @"payment_method" apiTxn)
        <!*> alwaysValid

instance Transform AT.Transaction DirectWalletPayment where
  transform apiTxn = DirectWalletPayment
    <$> (decodeTo @WalletDirectPaymentMethod $ fromMaybe' @"payment_method" apiTxn)
        <!*> alwaysValid
    <*> (fromMaybe' @"direct_wallet_token" apiTxn) <!*> textNotEmpty

instance Transform AT.Transaction NBPayment where
  transform apiTxn = NBPayment
    <$> ( decodeTo @NBPaymentMethod $ fromMaybe' @"payment_method" apiTxn)
        <!*> alwaysValid

instance Transform AT.Transaction CardPayment where
  transform apiTxn = CardPayment
    <$> (decodeTo @CardPaymentMethod $ fromMaybe' @"payment_method" apiTxn)
        <!*> alwaysValid  -- payment_method :: CPM.CardPaymentMethod
    <*> (transform apiTxn) -- card_payment_type      :: CardPaymentType
    <*> (takeField @"is_emi" apiTxn ) <?*> alwaysValid
    <*> (takeField @"emi_bank" apiTxn) <?*> textNotEmpty
    <*> (takeField @"emi_tenure" apiTxn) <?*> alwaysValid
    <*> (takeField @"auth_type" apiTxn) <?*> isRegularCardAuthType

instance Transform AT.Transaction CardPaymentType where
  transform apiTxn
    =   SavedCardPayment
          <$> (fromMaybe' @"card_token" apiTxn) <!*> textNotEmpty
          <*> (fromMaybe' @"card_security_code" apiTxn) <!*> textNotEmpty
    <!> NewCardPayment
          <$> (fromMaybe' @"card_number" apiTxn) <!*> cardNumberValidators
          <*> (fromMaybe' @"name_on_card" apiTxn ) <!*> textNotEmpty
          <*> (fromMaybe' @"card_exp_month" apiTxn ) <!*> textNotEmpty
          <*> (fromMaybe' @"card_exp_year" apiTxn ) <!*> textNotEmpty
          <*> (fromMaybe' @"card_security_code" apiTxn ) <!*> textNotEmpty
          <*> (fromMaybe' @"save_to_locker" apiTxn ) <!*> alwaysValid


isRegularCardAuthType :: NonEmpty (Text -> AuthType -> Validation [Text] AuthType)
isRegularCardAuthType = mkValidator "inappropriate auth_type" (`elem` [THREE_DS, OTP, VISA_CHECKOUT])

isAtmCardAuthType:: NonEmpty (Text -> AuthType -> Validation [Text] AuthType)
isAtmCardAuthType = mkValidator "inappropriate auth_type" (== ATMPIN)

cardNumberValidators :: NonEmpty (Text -> Text -> Validation [Text] Text)
cardNumberValidators = textNotEmpty <> testValidator1 <> testValidator2

alwaysValid :: NonEmpty (Text -> t -> Validation [Text] t)
alwaysValid = mkValidator "always should pass" (const True)

textNotEmpty :: NonEmpty (Text -> Text -> Validation [Text] Text)
textNotEmpty = mkValidator "can't be empty" (not . T.null)

testValidator1 :: NonEmpty (Text -> b -> Validation [Text] b)
testValidator1  = mkValidator "always should pass" (const True)

testValidator2 :: NonEmpty (Text -> b -> Validation [Text] b)
testValidator2  = mkValidator "always should pass" (const True)

isUPICollectTxnType :: NonEmpty (Text -> UPITxnType -> Validation [Text] UPITxnType)
isUPICollectTxnType = mkValidator "inappropriate txn_type" (`elem` txnTypes)
  where txnTypes = [UPI.UPI_COLLECT]

isUPIPayTxnType :: NonEmpty (Text -> UPITxnType -> Validation [Text] UPITxnType)
isUPIPayTxnType = mkValidator "inappropriate txn_type" (`elem` txnTypes)
  where txnTypes = [UPI.UPI_PAY, UPI.BHARAT_PAY]
