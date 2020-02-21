module Euler.Tests.OrderStatus.OrderStatusSpec where

import           EulerHS.Prelude hiding (show)
import           Test.Hspec

import qualified Data.Text as T
import qualified Prelude as P (show)

import           Euler.Product.OLTP.Order.OrderStatus (makeOrderStatusResponse)

import           Euler.API.Order (Chargeback' (..), Mandate' (..), OrderStatusQuery (..),
                                  OrderStatusResponse (..), Paymentlinks (..), Refund' (..),
                                  Risk (..))
import           Euler.Common.Types.Promotion (Promotion')
import           Euler.Storage.Types.OrderReference (OrderReference (..))
import           Euler.Storage.Types.TxnCardInfo (TxnCardInfo (..))
import           Euler.Storage.Types.TxnDetail (TxnDetail (..))

import           Control.Monad.Except (runExcept, Except (..))


spec :: Spec
spec =
    describe "makeOrderStatusResponse" $ do
      it "success makeOrderStatusResponse" $ \rt -> do
        let statusResp = runExcept $ makeOrderStatusResponse
              orderRef
              paymentlinks
              mPromotion
              mMandate
              query
              mTxnDetailJust
              gatewayReferenceId
              mRisk
              txnCardInfo
              mCardBrand
              mRefunds
              mChargeback
        statusResp `shouldBe` Right orderStatusResponse


orderRef :: OrderReference
orderRef = undefined

paymentlinks :: Paymentlinks
paymentlinks = undefined

mPromotion :: Maybe Promotion'
mPromotion = undefined

mMandate :: Maybe Mandate'
mMandate = undefined

query :: OrderStatusQuery
query = undefined

mTxnDetailJust :: Maybe TxnDetail
mTxnDetailJust = undefined

mTxnDetailNothing :: Maybe TxnDetail
mTxnDetailNothing = undefined

gatewayReferenceId :: Text
gatewayReferenceId = undefined

mRisk :: Maybe Risk
mRisk = undefined

txnCardInfo :: Maybe TxnCardInfo
txnCardInfo = undefined

mCardBrand :: Maybe Text
mCardBrand = undefined

mRefunds :: Maybe [Refund']
mRefunds = undefined

mChargeback :: Maybe [Chargeback']
mChargeback = undefined

orderStatusResponse :: OrderStatusResponse
orderStatusResponse = undefined




