{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Euler.API.Validators.Transaction where

import EulerHS.Prelude
import EulerHS.Extra.Validation as V

import Data.Generics.Product.Fields
import Data.Functor.Alt
import qualified Data.Text as T
import qualified Euler.Product.Domain.Transaction as DT
import Euler.Product.Domain.CardPayment
import Euler.Product.Domain.NBPayment
import Euler.Product.Domain.WalletPayment
import Euler.Product.Domain.UPIPayment as UPI
import Euler.Product.Domain.Types
import qualified Euler.API.Transaction as AT
import qualified Euler.API.Types as AT
import Euler.Common.Types.Transaction (AuthType(..))


-- TODO Probably we can factor explicit apiTxn out with the help of ReaderT


type V a = Validation [Text] a

transApiTxToDomTx :: AT.Transaction -> V DT.Transaction
transApiTxToDomTx apiTxn = DT.Transaction
      <$> (OrderId    <$> withField @"order_id"    apiTxn textNotEmpty) -- :: OrderId                -- ^
      <*> (MerchantId <$> withField @"merchant_id" apiTxn textNotEmpty) -- :: MerchantId
      <*> transApiTxToDomTxType apiTxn                                               -- :: TransactionType
      <*> withField @"redirect_after_payment" apiTxn pure                -- :: Bool
      <*> withField @"format" apiTxn textNotEmpty                       -- :: _

transApiTxToDomTxType :: AT.Transaction -> V DT.TransactionType
transApiTxToDomTxType apiTxn =  case ( getField @"payment_method_type" apiTxn
                          , getField @"auth_type" apiTxn
                          , getField @"direct_wallet_token" apiTxn) of
    (AT.CARD  , Just ATMPIN, _      ) -> DT.ATMRedirectionTransaction    <$> transApiTxToDomAtmRP apiTxn
                                     <!> DT.ATMSeamlessTransaction       <$> transApiTxToDomAtmSP apiTxn
    (AT.CARD  , _          , _      ) -> DT.CardTransaction              <$> transApiTxToDomCardP apiTxn
    (AT.NB    , _          , _      ) -> DT.NBTransaction                <$> transApiTxToDomNBP apiTxn
    (AT.WALLET, _          , Nothing) -> DT.WalletTransaction            <$> transApiTxToDomWalletP apiTxn
    (AT.WALLET, _          , Just _ ) -> DT.WalletDirectDebitTransaction <$> transApiTxToDomDirectWalletP apiTxn
    (AT.UPI   , _          , _      ) -> DT.UPITransaction               <$> transApiTxToDomUPIP apiTxn

transApiTxToDomAtmSP :: AT.Transaction -> V ATMSeamlessPayment
transApiTxToDomAtmSP apiTxn = ATMSeamlessPayment
    <$> withField @"payment_method" apiTxn (extractJust >=> decode)
    <*> transApiTxToDomCardPType apiTxn                 --CardPaymentType
    <*> withField @"auth_type"      apiTxn (extractJust >=> isAtmCardAuthType)

transApiTxToDomAtmRP :: AT.Transaction -> V ATMRedirectionPayment
transApiTxToDomAtmRP apiTxn = ATMRedirectionPayment
    <$> withField @"payment_method" apiTxn (extractJust >=> decode)
    <*> withField @"auth_type"      apiTxn (extractJust >=> isAtmCardAuthType)

transApiTxToDomUPIP :: AT.Transaction -> V UPIPayment
transApiTxToDomUPIP apiTxn
    =   UPICollect
        <$> withField @"payment_method" apiTxn (extractJust >=> decode)
        <*> withField @"txn_type"       apiTxn (extractJust >=> decode >=> isUPICollectTxnType)
        <*> withField @"upi_vpa"        apiTxn (extractJust >=> textNotEmpty)
    <!> UPIPay
        <$> withField @"payment_method" apiTxn (extractJust >=> decode)
        <*> withField @"txn_type"       apiTxn (extractJust >=> decode >=> isUPIPayTxnType)

transApiTxToDomWalletP :: AT.Transaction -> V WalletPayment
transApiTxToDomWalletP apiTxn = WalletPayment
    <$> withField @"payment_method" apiTxn (extractJust >=> decode)

transApiTxToDomDirectWalletP :: AT.Transaction -> V DirectWalletPayment
transApiTxToDomDirectWalletP apiTxn = DirectWalletPayment
    <$> withField @"payment_method"      apiTxn (extractJust >=> decode)
    <*> withField @"direct_wallet_token" apiTxn (extractJust >=> textNotEmpty)

transApiTxToDomNBP :: AT.Transaction -> V NBPayment
transApiTxToDomNBP apiTxn = NBPayment
    <$> withField @"payment_method" apiTxn (extractJust >=> decode)

transApiTxToDomCardP :: AT.Transaction -> V CardPayment
transApiTxToDomCardP apiTxn = CardPayment
    <$> withField @"payment_method" apiTxn (extractJust >=> decode)
    <*> transApiTxToDomCardPType apiTxn
    <*> withField @"is_emi"     apiTxn pure
    <*> withField @"emi_bank"   apiTxn (insideJust textNotEmpty)
    <*> withField @"emi_tenure" apiTxn pure
    <*> withField @"auth_type"  apiTxn (insideJust isRegularCardAuthType)

transApiTxToDomCardPType :: AT.Transaction -> V CardPaymentType
transApiTxToDomCardPType apiTxn
    =   SavedCardPayment
          <$> withField @"card_token"         apiTxn (extractJust >=> textNotEmpty)
          <*> withField @"card_security_code" apiTxn (extractJust >=> textNotEmpty)
    <!> NewCardPayment
          <$> withField @"card_number"        apiTxn (extractJust >=> cardNumberValidators)
          <*> withField @"name_on_card"       apiTxn (extractJust >=> textNotEmpty)
          <*> withField @"card_exp_month"     apiTxn (extractJust >=> textNotEmpty)
          <*> withField @"card_exp_year"      apiTxn (extractJust >=> textNotEmpty)
          <*> withField @"card_security_code" apiTxn (extractJust >=> textNotEmpty)
          <*> withField @"save_to_locker"     apiTxn extractJust
----------------------------------------------------------------------

isRegularCardAuthType :: Validator AuthType
isRegularCardAuthType = mkValidator "inappropriate field type" (`elem` [THREE_DS, OTP, VISA_CHECKOUT])

isAtmCardAuthType :: Validator AuthType
isAtmCardAuthType = mkValidator "inappropriate field type" (== ATMPIN)

textNotEmpty :: Validator Text
textNotEmpty = mkValidator "can't be empty" (not . T.null)

isUPICollectTxnType :: Validator UPITxnType
isUPICollectTxnType = mkValidator "inappropriate txn_type" (== UPI.UPI_COLLECT)

isUPIPayTxnType :: Validator UPITxnType
isUPIPayTxnType = mkValidator "inappropriate txn_type" (`elem` [UPI.UPI_PAY, UPI.BHARAT_PAY])

cardNumberValidators :: Validator Text
cardNumberValidators =
  parValidate
    [ textNotEmpty
    , testValidator1
    , testValidator2
    ]

testValidator1 :: Validator a
testValidator1  = mkValidator "always should pass" (const True)

testValidator2 :: Validator a
testValidator2  = pure
