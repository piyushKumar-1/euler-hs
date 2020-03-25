{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Euler.Product.Domain.Card where

import EulerHS.Prelude

import Data.Generics.Product.Fields
import Data.Data


data Card = Card
  { expiryYear         :: Maybe Text
  , cardReference      :: Maybe Text
  , savedToLocker      :: Bool
  , expiryMonth        :: Maybe Text
  , nameOnCard         :: Maybe Text
  , cardIssuer         :: Maybe Text
  , lastFourDigits     :: Maybe Text
  , usingSavedCard     :: Maybe Bool
  , cardFingerprint    :: Maybe Text
  , cardIsin           :: Maybe Text
  , cardType           :: Maybe Text
  , cardBrand          :: Maybe Text
  , shouldSendCardIsin :: Bool
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- From Graphh grails-app/services/juspay/JuspayLockerService.groovy 2 years ago

{-
 StoredCard storedCard = new StoredCard(
  merchantAccount: merchantAccount,
  maskedCardNumber: cardDetail.maskedCardNumber,
  nameOnCard: cardDetail.nameOnCard,
  cardExpMonth: cardDetail.cardExpMonth,
  cardExpYear: cardDetail.cardExpYear,
  cardIsin: cardDetail.cardIsin,
  cardLastFourDigits: cardDetail.cardLastFourDigits,
  cardReference: addToLockerResponse.externalId,
  cardFingerprint: addToLockerResponse.cardFingerprint,
  cardGlobalFingerprint: addToLockerResponse.cardGlobalFingerprint,
  vaultProvider: StoredCard.VaultProvider.JUSPAY,
  customerId: customerId,
  nickname: cardDetail.nickname,
  cardType: cardDetail.cardType,
  cardIssuer: cardDetail.cardIssuer,
  cardBrand: cardDetail.cardBrand,
  cardToken: addToLockerResponse.cardId)

static constraints = {
        nameOnCard nullable: true, blank: true
        nickname nullable: true, blank: true
        cardFingerprint nullable: true, blank: true
        cardGlobalFingerprint nullable: true, blank: true
        cardToken nullable: true, blank: true
        cardTokenOfVaultProvider nullable: true, blank: true
        cardType nullable: true, blank: true
        cardIssuer nullable: true, blank: true
        cardBrand nullable: true, blank: true
-}

data StoredCard = StoredCard
  { merchantAccountId     :: Int
  , customerId            :: Text
  , maskedCardNumber      :: Text
  , nameOnCard            :: Maybe Text
  , cardExpMonth          :: Text
  , cardExpYear           :: Text
  , cardIsin              :: Text
  , cardLastFourDigits    :: Text
  , cardReference         :: Text

  , cardFingerprint       :: Maybe Text
  , cardGlobalFingerprint :: Maybe Text
  , vaultProvider         :: VaultProvider

  , nickname              :: Maybe Text

  , cardType              :: Maybe CardType
  , cardIssuer            :: Maybe Text
  , cardBrand             :: Maybe Text
  , cardToken             :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)



data VaultProvider = JUSPAY | PAYU | SODEXO
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Data, Typeable)

data CardType
  = CREDIT
  | DEBIT
  | PREPAID
  | NB
  | WALLET
  | PAYLATER
  | UPI
  | ATM_CARD
  | REWARD
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Data, Typeable)

