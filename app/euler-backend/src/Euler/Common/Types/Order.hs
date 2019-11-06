{-# LANGUAGE DeriveAnyClass #-}

module Euler.Common.Types.Order where

import EulerHS.Prelude

data OrderStatus
  = NEW
  | SUCCESS
  | NOT_FOUND
  | ERROR
  | JUSPAY_DECLINED
  | PENDING_AUTHENTICATION
  | AUTHENTICATION_FAILED
  | AUTHORIZATION_FAILED
  | AUTHORIZING
  | AUTHORIZED
  | CREATED
  | COD_INITIATED
  | VOIDED
  | VOID_INITIATED
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

orderStatusToInt :: OrderStatus -> Int
orderStatusToInt CREATED = 1
orderStatusToInt NEW = 10
orderStatusToInt SUCCESS = 30
orderStatusToInt NOT_FOUND = 40
orderStatusToInt ERROR = -1
orderStatusToInt JUSPAY_DECLINED = 23
orderStatusToInt PENDING_AUTHENTICATION = 15
orderStatusToInt AUTHENTICATION_FAILED = 26
orderStatusToInt AUTHORIZATION_FAILED = 27
orderStatusToInt AUTHORIZING = 28
orderStatusToInt AUTHORIZED = 30
orderStatusToInt VOIDED = 31
orderStatusToInt VOID_INITIATED = 32

-- orderStatusToInt AUTHORIZATION_FAILURE = -1
orderStatusToInt COD_INITIATED = 29

data MandateFeature
  = DISABLED
  | REQUIRED
  | OPTIONAL
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)


data UDF = UDF
  { udf1              :: Maybe Text
  , udf2              :: Maybe Text
  , udf3              :: Maybe Text
  , udf4              :: Maybe Text
  , udf5              :: Maybe Text
  , udf6              :: Maybe Text
  , udf7              :: Maybe Text
  , udf8              :: Maybe Text
  , udf9              :: Maybe Text
  , udf10             :: Maybe Text
  }
  deriving (Generic, Eq, Show)

emptyUDF :: UDF
emptyUDF = UDF
 { udf1  = Nothing
 , udf2  = Nothing
 , udf3  = Nothing
 , udf4  = Nothing
 , udf5  = Nothing
 , udf6  = Nothing
 , udf7  = Nothing
 , udf8  = Nothing
 , udf9  = Nothing
 , udf10 = Nothing
 }
-- * how to represent MerchantAccount?
-- with Beam we can create models with common fields and embed them in data types
-- see https://tathougies.github.io/beam/user-guide/models/
-- Embedding part
{-
Example:

data AddressMixin f
  = Address
  { address           :: Columnar f (Maybe Text)
  , addressCity       :: Columnar f (Maybe Text)
  } deriving (Generic, Beamable)
type Address = AddressMixin Identity
deriving instance Show (AddressMixin Identity)

data EmployeeT f
  = Employee
  { employeeId        :: Columnar f Int32
  , employeeTitle     :: Columnar f (Maybe Text)
  , employeeReportsTo :: PrimaryKey EmployeeT (Nullable f)
  , employeeBirthDate :: Columnar f (Maybe LocalTime)
  , employeeHireDate  :: Columnar f (Maybe LocalTime)
  , employeeAddress   :: AddressMixin f
  } deriving (Generic, Beamable)
-- ...
data CustomerT f
  = Customer
  { customerId        :: Columnar f Int32
  , customerFirstName :: Columnar f Text
  , customerLastName  :: Columnar f Text
  , customerAddress   :: AddressMixin f
  , customerPhone     :: Columnar f (Maybe Text)
  , customerSupportRep :: PrimaryKey EmployeeT (Nullable f)
  } deriving (Generic, Beamable)

Also generic lens have lenses to extract sub type:

data Human = Human
  { name    :: String
  , age     :: Int
  , address :: String
  } deriving (Generic, Show)

data Animal = Animal
  { name    :: String
  , age     :: Int
  } deriving (Generic, Show)

human :: Human
human = Human {name = "Tunyasz", age = 50, address = "London"}

>>> human ^. super @Animal
Animal {name = "Tunyasz", age = 50}

>>> upcast human :: Animal
Animal {name = "Tunyasz", age = 50}

-}
{-
newtype MerchantAccount = MerchantAccount (MAUser MADB)

type MAUser a =
  { id :: NullOrUndefined Int
  , merchantName :: NullOrUndefined String
  , apiKey :: NullOrUndefined String
  , userId :: NullOrUndefined Int
  , alternativeEmail :: NullOrUndefined String
  , contactPerson :: NullOrUndefined String
  , mobile :: NullOrUndefined String
  , telephone :: NullOrUndefined String
  , website :: NullOrUndefined String
  , aboutBusiness :: NullOrUndefined String
  , adminContactEmail :: NullOrUndefined String
  , city :: NullOrUndefined String
  , contactPersonEmail :: NullOrUndefined String
  , contactPersonPrimary :: NullOrUndefined String
  , contactPersonSecondary :: NullOrUndefined String
  , contactPersonSecondaryEmail :: NullOrUndefined String
  , landmark :: NullOrUndefined String
  , officeLine1 :: NullOrUndefined String
  , officeLine2 :: NullOrUndefined String
  , officeLine3 :: NullOrUndefined String
  , state :: NullOrUndefined String
  , zip :: NullOrUndefined String
  , dateCreated :: NullOrUndefined Date
  , lastModified :: NullOrUndefined Date
  , paymentResponseHashKey :: NullOrUndefined String
  , returnUrl :: NullOrUndefined String
  , autoRefundConflictTransactions :: NullOrUndefined Boolean
  , enablePaymentResponseHash :: NullOrUndefined Boolean
  , enableAutomaticRetry :: NullOrUndefined Boolean
  , enableConflictStatusNotification :: NullOrUndefined Boolean
  , webHookCustomHeaders :: NullOrUndefined String
  , conflictStatusEmail :: NullOrUndefined String
  , webHookPassword :: NullOrUndefined String
  , webHookUsername :: NullOrUndefined String
  , enableSaveCardBeforeAuth :: NullOrUndefined Boolean
  , webHookapiversion :: NullOrUndefined String
  , webHookurl :: NullOrUndefined String
  , cardEncodingKey :: NullOrUndefined String
  , txnIdCustomPrefix :: NullOrUndefined String
  , includeSurchargeAmountForRefund :: NullOrUndefined Boolean
  , shouldAddSurcharge :: NullOrUndefined Boolean
  , showSurchargeBreakupScreen :: NullOrUndefined Boolean
  , enabledInstantRefund :: NullOrUndefined Boolean
  , enableTxnFilter :: NullOrUndefined Boolean
  | a
  }

type MADB = (
    version :: Int
  , bankAccountNumber :: NullOrUndefined String
  , creditBalance :: NullOrUndefined Number
  , merchantId :: NullOrUndefined String
  , expressCheckoutEnabled :: NullOrUndefined Boolean
  , inlineCheckoutEnabled :: NullOrUndefined Boolean
  , inlineCheckoutHtml :: NullOrUndefined String
  , iciciKey :: NullOrUndefined String
  , iciciMerchantId :: NullOrUndefined String
  , iciciSecondFactorId :: NullOrUndefined String
  , iciciVerified :: NullOrUndefined Boolean
  , lockerId :: NullOrUndefined String
  , axisMerchantId :: NullOrUndefined String
  , axisVerified :: NullOrUndefined Boolean
  , axisSecureSecret :: NullOrUndefined String
  , axisAccessCode :: NullOrUndefined String
  , hdfcMerchantId :: NullOrUndefined String
  , hdfcPassword :: NullOrUndefined String
  , hdfcVerified :: NullOrUndefined Boolean
  , ebsAccountId :: NullOrUndefined String
  , ebsHash :: NullOrUndefined String
  , ebsVerified :: NullOrUndefined Boolean
  , enableReauthentication :: NullOrUndefined Boolean
  , enableReauthorization :: NullOrUndefined Boolean
  , enableRecapture :: NullOrUndefined Boolean
  , enableSendingLastFourDigits :: NullOrUndefined Boolean
  , domain :: NullOrUndefined String
  , whitelabelEnabled :: NullOrUndefined Boolean
  , payuMerchantKey :: NullOrUndefined String
  , payuSalt :: NullOrUndefined String
  , payuVerified :: NullOrUndefined Boolean
  , otpEnabled :: NullOrUndefined Boolean
  , reverseTokenEnabled :: NullOrUndefined Boolean
  , hdfcTestMode :: NullOrUndefined Boolean
  , realModeOnly :: NullOrUndefined Boolean
  , gatewayDecidedByHealthEnabled :: NullOrUndefined Boolean
  , gatewayPriority :: NullOrUndefined String
  , amexMerchantId :: NullOrUndefined String
  , amexVerified :: NullOrUndefined Boolean
  , mustUseGivenOrderIdForTxn :: NullOrUndefined Boolean
  , enable3DsecureHelpMail :: NullOrUndefined Boolean
  , amexAccessCode :: NullOrUndefined String
  , amexPassword :: NullOrUndefined String
  , amexSecureSecret :: NullOrUndefined String
  , amexUsername :: NullOrUndefined String
  , paypalClientId :: NullOrUndefined String
  , paypalSecret :: NullOrUndefined String
  , hdfcIvrMerchantId :: NullOrUndefined String
  , hdfcIvrPassword :: NullOrUndefined String
  , hdfcIvrVerified :: NullOrUndefined Boolean
  , settlementAccountId :: NullOrUndefined Int
  , enableRefundsInDashboard :: NullOrUndefined Boolean
  , hdfcMerchantCode :: NullOrUndefined String
  , merchantLegalName :: NullOrUndefined String
  , orderFieldsInSettlementReport :: NullOrUndefined String
  , enableSendingCardIsin :: NullOrUndefined Boolean
  , ccavenueAccountId :: NullOrUndefined String
  , ccavenueSecretKey :: NullOrUndefined String
  , ccavenueVerified :: NullOrUndefined Boolean
  , mobileVersion :: NullOrUndefined String
  , enableOrderNotification :: NullOrUndefined Boolean
  , orderNotificationEmail :: NullOrUndefined String
  , ccavenueBackendGateway :: NullOrUndefined String
  , payuTestMode :: NullOrUndefined Boolean
  , ebsBackendGateway :: NullOrUndefined String
  , gatewayPriorityLogic :: NullOrUndefined String
  , useCodeForGatewayPriority :: Boolean
  , enableExternalRiskCheck :: NullOrUndefined Boolean
  , citiKey :: NullOrUndefined String
  , citiMerchantCode :: NullOrUndefined String
  , citiTestMode :: NullOrUndefined Boolean
  , citiVerified :: NullOrUndefined Boolean
  , fetchCardsFromPayu :: NullOrUndefined Boolean
  , prefixMerchantIdForCardKey :: NullOrUndefined Boolean
  , secondaryMerchantAccountId :: NullOrUndefined Int
  , resellerId :: NullOrUndefined String
  , fingerprintOnTokenize :: NullOrUndefined Boolean
  , enableUnauthenticatedOrderStatusApi :: NullOrUndefined Boolean -- should be present and defaulted to False, not maybe?
  , enableUnauthenticatedCardAdd :: NullOrUndefined Boolean
  , timezone :: NullOrUndefined String
  , mandatory2FA :: NullOrUndefined Boolean
  )

  -}