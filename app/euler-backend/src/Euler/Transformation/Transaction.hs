{-# LANGUAGE AllowAmbiguousTypes #-}
module Euler.Transformation.Transaction where

import EulerHS.Prelude hiding (pack, pred, or)
import qualified Prelude as P

import Data.Generics.Product.Fields
import Data.Validation
import Data.Functor.Alt
import           Data.Aeson (eitherDecode, decode)
import GHC.TypeLits
import Type.Reflection
import Control.Lens hiding (transform, cons)
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
import qualified Data.Text as T
import Data.List.NonEmpty (cons)
import Data.Data hiding (typeRep)

import qualified Euler.API.Transaction as AT
import qualified Euler.API.Types as AT
import Euler.API.Types

class Transform a b where
  transform :: a -> Validation [Text] b

instance Transform AT.Transaction DT.Transaction where
  transform apiTxn = DT.Transaction 
      <$> (OrderId <$> ((fieldWithName @"order_id" apiTxn) <@*> textNotEmpty)) -- :: OrderId                -- ^
      <*> (MerchantId <$> ((fieldWithName @"merchant_id" apiTxn) <@*> textNotEmpty)) -- :: MerchantId 
      <*> (transform apiTxn) -- :: TransactionType
      <*> (fieldWithName @"redirect_after_payment" apiTxn) <@*> alwaysValid -- :: Bool
      <*> (fieldWithName @"format" apiTxn) <@*> textNotEmpty -- :: Text


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
    _ -> _Failure # ["transaction not supported, check fields"]

instance Transform AT.Transaction ATMSeamlessPayment where
  transform apiTxn = ATMSeamlessPayment
    <$> (decodeTo @CardPaymentMethod $ fromMaybe' @"payment_method" apiTxn)
        <@*> alwaysValid  -- payment_method :: CPM.CardPaymentMethod
    <*> (transform apiTxn) -- card_payment_type      :: CardPaymentType
    <*> (fromMaybe' @"auth_type" apiTxn) <@*> isAtmCardAuthType

instance Transform AT.Transaction ATMRedirectionPayment where
  transform apiTxn = ATMRedirectionPayment
    <$> (decodeTo @ATMCardPaymentMethod $ fromMaybe' @"payment_method" apiTxn)
        <@*> alwaysValid
    <*> (fromMaybe' @"auth_type" apiTxn) <@*> isAtmCardAuthType

instance Transform AT.Transaction UPIPayment where
  transform apiTxn
    =   UPICollect
        <$> (decodeTo @UPIPaymentMethod $ fromMaybe' @"payment_method" apiTxn)
            <@*> alwaysValid
        <*> (decodeTo @UPITxnType $ fromMaybe' @"txn_type" apiTxn)
            <@*> isUPICollectTxnType
        <*> (fromMaybe' @"upi_vpa" apiTxn) <@*> textNotEmpty
    <!> UPIPay
        <$> (decodeTo @UPIPaymentMethod $ fromMaybe' @"payment_method" apiTxn)
            <@*> alwaysValid
        <*> (decodeTo @UPITxnType $ fromMaybe' @"txn_type" apiTxn)
            <@*> isUPIPayTxnType

instance Transform AT.Transaction WalletPayment where
  transform apiTxn = WalletPayment
    <$> (decodeTo @WalletPaymentMethod $ fromMaybe' @"payment_method" apiTxn)
        <@*> alwaysValid

instance Transform AT.Transaction DirectWalletPayment where
  transform apiTxn = DirectWalletPayment
    <$> (decodeTo @WalletDirectPaymentMethod $ fromMaybe' @"payment_method" apiTxn)
        <@*> alwaysValid
    <*> (fromMaybe' @"direct_wallet_token" apiTxn) <@*> textNotEmpty

instance Transform AT.Transaction NBPayment where
  transform apiTxn = NBPayment
    <$> ( decodeTo @NBPaymentMethod $ fromMaybe' @"payment_method" apiTxn)
        <@*> alwaysValid

instance Transform AT.Transaction CardPayment where
  transform apiTxn = CardPayment
    <$> (decodeTo @CardPaymentMethod $ fromMaybe' @"payment_method" apiTxn)
        <@*> alwaysValid  -- payment_method :: CPM.CardPaymentMethod
    <*> (transform apiTxn) -- card_payment_type      :: CardPaymentType
    <*> (fieldWithName @"is_emi" apiTxn ) <?*> alwaysValid
    <*> (fieldWithName @"emi_bank" apiTxn) <?*> textNotEmpty
    <*> (fieldWithName @"emi_tenure" apiTxn) <?*> alwaysValid
    <*> (fieldWithName @"auth_type" apiTxn) <?*> isRegularCardAuthType

instance Transform AT.Transaction CardPaymentType where
  transform apiTxn 
    =   SavedCardPayment 
          <$> (fromMaybe' @"card_token" apiTxn) <@*> textNotEmpty
          <*> (fromMaybe' @"card_security_code" apiTxn) <@*> textNotEmpty
    <!> NewCardPayment
          <$> (fromMaybe' @"card_number" apiTxn) <@*> cardNumberValidators
          <*> (fromMaybe' @"name_on_card" apiTxn ) <@*> textNotEmpty
          <*> (fromMaybe' @"card_exp_month" apiTxn ) <@*> textNotEmpty
          <*> (fromMaybe' @"card_exp_year" apiTxn ) <@*> textNotEmpty
          <*> (fromMaybe' @"card_security_code" apiTxn ) <@*> textNotEmpty
          <*> (fromMaybe' @"save_to_locker" apiTxn ) <@*> alwaysValid


isRegularCardAuthType :: NonEmpty (Text -> AuthType -> Validation [Text] AuthType)
isRegularCardAuthType = mkValidator "inappropriate auth_type" (`elem` [THREE_DS, OTP, VISA_CHECKOUT])

isAtmCardAuthType:: NonEmpty (Text -> AuthType -> Validation [Text] AuthType)
isAtmCardAuthType = mkValidator "inappropriate auth_type" (== ATMPIN)

cardNumberValidators :: NonEmpty (Text -> Text -> Validation [Text] Text)
cardNumberValidators = textNotEmpty <> testValidator1 <> testValidator2

alwaysValid' :: Text -> t -> Validation [Text] t
alwaysValid' _ v = _Success # v

alwaysValid :: NonEmpty (Text -> t -> Validation [Text] t)
alwaysValid = alwaysValid':|[]

textNotEmpty :: NonEmpty (Text -> Text -> Validation [Text] Text)
textNotEmpty = mkValidator "can't be empty" (not . T.null)

testValidator1 :: NonEmpty (Text -> b -> Validation [Text] b)
testValidator1  = mkValidator "always should pass" (const True)

testValidator2 :: NonEmpty (Text -> b -> Validation [Text] b)
testValidator2  = mkValidator "always should pass" (const True)

isUPICollectTxnType :: NonEmpty (Text -> UPITxnType -> Validation [Text] UPITxnType)
isUPICollectTxnType = mkValidator "inappropriate txn_type" (`elem` txns)
  where txns = [UPI.UPI_COLLECT]

isUPIPayTxnType :: NonEmpty (Text -> UPITxnType -> Validation [Text] UPITxnType)
isUPIPayTxnType = mkValidator "inappropriate txn_type" (`elem` txns)
  where txns = [UPI.UPI_PAY, UPI.BHARAT_PAY]

-- #### helpers

mkValidator :: Text -> (t -> Bool) -> NonEmpty (Text -> t -> Validation [Text] t)
mkValidator err pred = (\fn v -> if pred v then _Success # v else _Failure # [fn <> " " <> err]) :| []

decodeTo :: forall t. (Data t, Read t) => (Text, Validation [Text] Text) -> (Text, Validation [Text] t)
decodeTo (f, Failure e) = (f, Failure e)
decodeTo (f, Success v) = case (readMaybe $ toString v) of
  Just x -> (f, _Success # x)
  _ -> (f, _Failure # ["Can't decode " <> v <> " from field " <> f <> ", should be one of " <> (showConstructors @t)])

fromMaybe' :: forall (f :: Symbol) t r
  . ( HasField' f r (Maybe t), KnownSymbol f)
  => r -> (Text, Validation [Text] t)
fromMaybe' r = ((fieldName_ @f) , isPresent' (fieldName_ @f) $ getField @f r)

isPresent' :: Text -> Maybe t -> Validation [Text] t
isPresent' f v = maybe (_Failure # [f <> " not present"]) (_Success # ) v

fieldWithName :: forall (f :: Symbol) v r
  .(Generic r, HasField' f r v, KnownSymbol f) 
  =>  r -> (Text, Validation [Text] v)
fieldWithName r = (fieldName_ @f, Success $ getField @f r)

ifPresent :: (Text, Validation [Text] (Maybe v))
  -> NonEmpty((Text -> v -> Validation [Text] v)) --(Text -> v -> Validation [Text] v)
  -> Validation [Text] (Maybe v)
ifPresent (fName, Success (Just v)) vs = Just <$> (appValidators vs fName v)
ifPresent (_, Success Nothing) _ = Success Nothing
ifPresent (_, Failure e) _ = Failure e

infix 5 <?*>
(<?*>) :: (Text, Validation [Text] (Maybe v))
  -> NonEmpty((Text -> v -> Validation [Text] v)) --(Text -> v -> Validation [Text] v)
  -> Validation [Text] (Maybe v)
(<?*>) = ifPresent

ifTransformed :: (Text, Validation [Text] v)
  -> NonEmpty((Text -> v -> Validation [Text] v)) --(Text -> v -> Validation [Text] v)
  -> Validation [Text] v
ifTransformed (_, Failure e) _ = Failure e
ifTransformed (fName, Success a) vs = appValidators vs fName a

infix 5 <@*>
(<@*>) :: (Text, Validation [Text] v)
  -> NonEmpty((Text -> v -> Validation [Text] v)) --(Text -> v -> Validation [Text] v)
  -> Validation [Text] v
(<@*>) = ifTransformed

appValidators ::  NonEmpty((Text -> v -> Validation [Text] v)) -> Text -> v -> Validation [Text] v
appValidators validators fName v = foldr1 (<*) $ map (($ (fName, v)).uncurry) validators

showConstructors :: forall t. (Data t) => Text
showConstructors = T.pack $ show $ getConstructors @t

getConstructors :: forall t. (Data t) => [Constr]
getConstructors = dataTypeConstrs (dataTypeOf (undefined :: t))

fieldName_ :: forall (f :: Symbol).(KnownSymbol f) => Text
fieldName_ = T.pack $ ((filter (/='"'))) $ P.show $ typeRep @f

infixl 3 <<|>>
(<<|>>) :: Semigroup err => Validation err a -> Validation err a -> Validation err a
(<<|>>) = or
{-# INLINE (<<|>>) #-}

-- unlike (<!>) 'or' collect errors from both variants
or :: Semigroup err => Validation err a -> Validation err a -> Validation err a
or (Failure e1) (Failure e2) =
    Failure (e1 <> e2)
or (Failure _) (Success a) = Success a
or (Success a) _ = Success a
